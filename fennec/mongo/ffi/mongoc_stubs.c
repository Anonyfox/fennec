/* OCaml <-> libmongoc glue (mongo-c-driver 2.x).
 *
 * Design rules that make this safe and Eio-friendly:
 *   1. Every function blocks on network I/O, so it releases the OCaml runtime
 *      lock around the libmongoc calls (caml_enter/leave_blocking_section). The
 *      higher layer runs each call in an Eio systhread, so a blocking change
 *      stream never stalls the scheduler.
 *   2. While the lock is released we must NOT touch any OCaml value. So we copy
 *      every input string to the C heap first, do the work into C buffers, then
 *      re-acquire the lock and allocate the OCaml result.
 *   3. Handles are GC-managed custom blocks with finalizers, so pools and change
 *      streams are released deterministically even on exceptions.
 *
 * Guarded by HAVE_MONGOC (set by config/discover.ml): 1 when the static libmongoc built, 0 when it
 * could not (no cmake/curl, unsupported OS) — then every entry point compiles to a stub that raises,
 * so the package still builds and the :memory: backend works.
 */

#if defined(HAVE_MONGOC) && HAVE_MONGOC

#include <mongoc/mongoc.h>
#include <bson/bson.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/threads.h>

/* ------------------------------------------------------------------ pool --- */

typedef struct {
  mongoc_client_pool_t *pool;
  mongoc_uri_t *uri;
} mongo_pool;

#define Pool_val(v) (*((mongo_pool **)Data_custom_val(v)))

static void pool_finalize(value v) {
  mongo_pool *p = Pool_val(v);
  if (p) {
    if (p->pool) mongoc_client_pool_destroy(p->pool);
    if (p->uri) mongoc_uri_destroy(p->uri);
    free(p);
  }
}

static struct custom_operations pool_ops = {
    "mongo.pool",
    pool_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_fixed_length_default};

/* ---------------------------------------------------------- change stream --- */

typedef struct {
  mongoc_client_pool_t *pool;   /* borrowed: where to push the client back */
  mongoc_client_t *client;      /* checked out for the stream's lifetime */
  mongoc_collection_t *coll;
  mongoc_change_stream_t *stream;
  int closed;
} mongo_stream;

#define Stream_val(v) (*((mongo_stream **)Data_custom_val(v)))

static void stream_close(mongo_stream *s) {
  if (!s || s->closed) return;
  if (s->stream) mongoc_change_stream_destroy(s->stream);
  if (s->coll) mongoc_collection_destroy(s->coll);
  if (s->client) mongoc_client_pool_push(s->pool, s->client);
  s->stream = NULL;
  s->coll = NULL;
  s->client = NULL;
  s->closed = 1;
}

static void stream_finalize(value v) {
  mongo_stream *s = Stream_val(v);
  if (s) {
    stream_close(s);
    free(s);
  }
}

static struct custom_operations stream_ops = {
    "mongo.change_stream",
    stream_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_fixed_length_default};

/* --------------------------------------------------------------- helpers --- */

/* A tiny growable byte buffer. The driver's bson_string_t was removed in
 * mongo-c-driver 2.x, so we roll our own for assembling the find JSON array.
 * Used only inside the blocking section, away from any OCaml value. */
typedef struct {
  char *data;
  size_t len;
  size_t cap;
} strbuf;

static void sb_init(strbuf *b) {
  b->cap = 256;
  b->len = 0;
  b->data = (char *)malloc(b->cap);
  b->data[0] = '\0';
}

static void sb_append(strbuf *b, const char *s) {
  size_t n = strlen(s);
  if (b->len + n + 1 > b->cap) {
    while (b->len + n + 1 > b->cap) b->cap *= 2;
    b->data = (char *)realloc(b->data, b->cap);
  }
  memcpy(b->data + b->len, s, n + 1);
  b->len += n;
}

static void sb_free(strbuf *b) { free(b->data); }

/* Parse JSON (possibly empty/NULL -> empty document) into an already-declared
 * bson_t. Returns true on success. Called inside the blocking section. */
static bool json_to_bson(const char *json, bson_t *out, bson_error_t *error) {
  if (json == NULL || json[0] == '\0') {
    bson_init(out);
    return true;
  }
  return bson_init_from_json(out, json, -1, error);
}

/* ------------------------------------------------------------------ init --- */

/* Drop everything below WARNING so the driver's connection-monitor DEBUG/INFO
 * chatter stays out of the application's stderr. */
static void quiet_log_handler(mongoc_log_level_t level, const char *domain,
                             const char *message, void *user_data) {
  (void)domain;
  (void)user_data;
  if (level <= MONGOC_LOG_LEVEL_WARNING)
    fprintf(stderr, "mongoc: %s\n", message);
}

CAMLprim value ocaml_mongo_init(value unit) {
  CAMLparam1(unit);
  mongoc_log_set_handler(quiet_log_handler, NULL);
  mongoc_init();
  CAMLreturn(Val_unit);
}

/* whether the native driver was actually built (true here; the stub build returns false) */
CAMLprim value ocaml_mongo_available(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_bool(1));
}

CAMLprim value ocaml_mongo_pool_new(value v_uri) {
  CAMLparam1(v_uri);
  CAMLlocal1(res);
  bson_error_t error;
  mongoc_uri_t *uri = mongoc_uri_new_with_error(String_val(v_uri), &error);
  if (!uri) caml_failwith(error.message);
  mongoc_client_pool_t *pool = mongoc_client_pool_new(uri);
  if (!pool) {
    mongoc_uri_destroy(uri);
    caml_failwith("mongoc_client_pool_new failed");
  }
  mongo_pool *p = (mongo_pool *)malloc(sizeof(mongo_pool));
  p->pool = pool;
  p->uri = uri;
  res = caml_alloc_custom(&pool_ops, sizeof(mongo_pool *), 0, 1);
  Pool_val(res) = p;
  CAMLreturn(res);
}

/* ------------------------------------------------------------------ ping --- */

CAMLprim value ocaml_mongo_ping(value v_pool, value v_db) {
  CAMLparam2(v_pool, v_db);
  mongo_pool *p = Pool_val(v_pool);
  char *db = strdup(String_val(v_db));
  int ok = 0;

  caml_enter_blocking_section();
  {
    bson_t cmd, reply;
    bson_error_t error;
    bson_init(&cmd);
    BSON_APPEND_INT32(&cmd, "ping", 1);
    mongoc_client_t *client = mongoc_client_pool_pop(p->pool);
    ok = mongoc_client_command_simple(client, db, &cmd, NULL, &reply, &error);
    bson_destroy(&reply);
    mongoc_client_pool_push(p->pool, client);
    bson_destroy(&cmd);
  }
  caml_leave_blocking_section();

  free(db);
  CAMLreturn(Val_bool(ok));
}

/* --------------------------------------------------------------- command --- */

CAMLprim value ocaml_mongo_command(value v_pool, value v_db, value v_cmd) {
  CAMLparam3(v_pool, v_db, v_cmd);
  CAMLlocal1(res);
  mongo_pool *p = Pool_val(v_pool);
  char *db = strdup(String_val(v_db));
  char *cmdj = strdup(String_val(v_cmd));
  char *out = NULL;
  int ok = 0;
  bson_error_t error;
  error.message[0] = '\0';

  caml_enter_blocking_section();
  {
    bson_t cmd, reply;
    if (json_to_bson(cmdj, &cmd, &error)) {
      mongoc_client_t *client = mongoc_client_pool_pop(p->pool);
      if (mongoc_client_command_simple(client, db, &cmd, NULL, &reply, &error)) {
        out = bson_as_canonical_extended_json(&reply, NULL);
        ok = 1;
        bson_destroy(&reply);
      }
      mongoc_client_pool_push(p->pool, client);
      bson_destroy(&cmd);
    }
  }
  caml_leave_blocking_section();

  free(db);
  free(cmdj);
  if (!ok) {
    char msg[512];
    snprintf(msg, sizeof msg, "command: %s", error.message);
    if (out) bson_free(out);
    caml_failwith(msg);
  }
  res = caml_copy_string(out);
  bson_free(out);
  CAMLreturn(res);
}

/* ------------------------------------------------------------------ find --- */

CAMLprim value ocaml_mongo_find(value v_pool, value v_db, value v_coll,
                                value v_filter, value v_opts) {
  CAMLparam5(v_pool, v_db, v_coll, v_filter, v_opts);
  CAMLlocal1(res);
  mongo_pool *p = Pool_val(v_pool);
  char *db = strdup(String_val(v_db));
  char *coll = strdup(String_val(v_coll));
  char *filterj = strdup(String_val(v_filter));
  char *optsj = strdup(String_val(v_opts));
  char *out = NULL;
  int ok = 0;
  bson_error_t error;
  error.message[0] = '\0';

  caml_enter_blocking_section();
  {
    bson_t filter, opts;
    if (json_to_bson(filterj, &filter, &error)) {
      if (json_to_bson(optsj, &opts, &error)) {
        mongoc_client_t *client = mongoc_client_pool_pop(p->pool);
        mongoc_collection_t *c = mongoc_client_get_collection(client, db, coll);
        mongoc_cursor_t *cur =
            mongoc_collection_find_with_opts(c, &filter, &opts, NULL);
        strbuf buf;
        sb_init(&buf);
        sb_append(&buf, "[");
        const bson_t *doc;
        int first = 1;
        while (mongoc_cursor_next(cur, &doc)) {
          char *s = bson_as_canonical_extended_json(doc, NULL);
          if (!first) sb_append(&buf, ",");
          sb_append(&buf, s);
          bson_free(s);
          first = 0;
        }
        sb_append(&buf, "]");
        if (mongoc_cursor_error(cur, &error)) {
          ok = 0;
        } else {
          out = strdup(buf.data);
          ok = 1;
        }
        sb_free(&buf);
        mongoc_cursor_destroy(cur);
        mongoc_collection_destroy(c);
        mongoc_client_pool_push(p->pool, client);
        bson_destroy(&opts);
      }
      bson_destroy(&filter);
    }
  }
  caml_leave_blocking_section();

  free(db);
  free(coll);
  free(filterj);
  free(optsj);
  if (!ok) {
    char msg[512];
    snprintf(msg, sizeof msg, "find: %s", error.message);
    if (out) free(out);
    caml_failwith(msg);
  }
  res = caml_copy_string(out);
  free(out);
  CAMLreturn(res);
}

/* ------------------------------------------------------------- aggregate --- */

CAMLprim value ocaml_mongo_aggregate(value v_pool, value v_db, value v_coll,
                                     value v_pipeline, value v_opts) {
  CAMLparam5(v_pool, v_db, v_coll, v_pipeline, v_opts);
  CAMLlocal1(res);
  mongo_pool *p = Pool_val(v_pool);
  char *db = strdup(String_val(v_db));
  char *coll = strdup(String_val(v_coll));
  char *pipej = strdup(String_val(v_pipeline));
  char *optsj = strdup(String_val(v_opts));
  char *out = NULL;
  int ok = 0;
  bson_error_t error;
  error.message[0] = '\0';

  caml_enter_blocking_section();
  {
    /* mongoc_collection_aggregate accepts a document with a "pipeline" field, so
     * wrap the JSON array exactly as the watch path does. Empty -> []. */
    const char *pj = (pipej && pipej[0] != '\0') ? pipej : "[]";
    char *wrapped = bson_strdup_printf("{\"pipeline\": %s}", pj);
    bson_t pipeline, opts;
    int pp = bson_init_from_json(&pipeline, wrapped, -1, &error);
    bson_free(wrapped);
    if (pp) {
      if (json_to_bson(optsj, &opts, &error)) {
        mongoc_client_t *client = mongoc_client_pool_pop(p->pool);
        mongoc_collection_t *c = mongoc_client_get_collection(client, db, coll);
        mongoc_cursor_t *cur = mongoc_collection_aggregate(
            c, MONGOC_QUERY_NONE, &pipeline, &opts, NULL);
        strbuf buf;
        sb_init(&buf);
        sb_append(&buf, "[");
        const bson_t *doc;
        int first = 1;
        while (mongoc_cursor_next(cur, &doc)) {
          char *s = bson_as_canonical_extended_json(doc, NULL);
          if (!first) sb_append(&buf, ",");
          sb_append(&buf, s);
          bson_free(s);
          first = 0;
        }
        sb_append(&buf, "]");
        if (mongoc_cursor_error(cur, &error)) {
          ok = 0;
        } else {
          out = strdup(buf.data);
          ok = 1;
        }
        sb_free(&buf);
        mongoc_cursor_destroy(cur);
        mongoc_collection_destroy(c);
        mongoc_client_pool_push(p->pool, client);
        bson_destroy(&opts);
      }
      bson_destroy(&pipeline);
    }
  }
  caml_leave_blocking_section();

  free(db);
  free(coll);
  free(pipej);
  free(optsj);
  if (!ok) {
    char msg[512];
    snprintf(msg, sizeof msg, "aggregate: %s", error.message);
    if (out) free(out);
    caml_failwith(msg);
  }
  res = caml_copy_string(out);
  free(out);
  CAMLreturn(res);
}

/* ------------------------------------------------ insert / update / delete --- */

/* shared shape: build a reply bson, translate to JSON, raise on driver error */
typedef enum { OP_INSERT, OP_UPDATE, OP_DELETE } write_op;

static value mongo_write(write_op op, mongo_pool *p, char *db, char *coll,
                         char *aj, char *bj) {
  CAMLparam0();
  CAMLlocal1(res);
  char *out = NULL;
  int ok = 0;
  bson_error_t error;
  error.message[0] = '\0';

  caml_enter_blocking_section();
  {
    bson_t a, b, reply;
    int parsed = json_to_bson(aj, &a, &error);
    int parsed_b = 1;
    if (parsed && op == OP_UPDATE) parsed_b = json_to_bson(bj, &b, &error);
    if (parsed && parsed_b) {
      mongoc_client_t *client = mongoc_client_pool_pop(p->pool);
      mongoc_collection_t *c = mongoc_client_get_collection(client, db, coll);
      switch (op) {
        case OP_INSERT:
          ok = mongoc_collection_insert_one(c, &a, NULL, &reply, &error);
          break;
        case OP_UPDATE:
          ok = mongoc_collection_update_one(c, &a, &b, NULL, &reply, &error);
          break;
        case OP_DELETE:
          ok = mongoc_collection_delete_one(c, &a, NULL, &reply, &error);
          break;
      }
      if (ok) out = bson_as_relaxed_extended_json(&reply, NULL);
      bson_destroy(&reply);
      mongoc_collection_destroy(c);
      mongoc_client_pool_push(p->pool, client);
      if (op == OP_UPDATE && parsed_b) bson_destroy(&b);
      bson_destroy(&a);
    } else if (parsed) {
      bson_destroy(&a);
    }
  }
  caml_leave_blocking_section();

  if (!ok) {
    char msg[512];
    snprintf(msg, sizeof msg, "write: %s", error.message);
    if (out) bson_free(out);
    caml_failwith(msg);
  }
  res = caml_copy_string(out);
  bson_free(out);
  CAMLreturn(res);
}

CAMLprim value ocaml_mongo_insert_one(value v_pool, value v_db, value v_coll,
                                      value v_doc) {
  CAMLparam4(v_pool, v_db, v_coll, v_doc);
  mongo_pool *p = Pool_val(v_pool);
  char *db = strdup(String_val(v_db));
  char *coll = strdup(String_val(v_coll));
  char *doc = strdup(String_val(v_doc));
  value r = mongo_write(OP_INSERT, p, db, coll, doc, NULL);
  free(db);
  free(coll);
  free(doc);
  CAMLreturn(r);
}

CAMLprim value ocaml_mongo_update_one(value v_pool, value v_db, value v_coll,
                                      value v_filter, value v_update) {
  CAMLparam5(v_pool, v_db, v_coll, v_filter, v_update);
  mongo_pool *p = Pool_val(v_pool);
  char *db = strdup(String_val(v_db));
  char *coll = strdup(String_val(v_coll));
  char *filter = strdup(String_val(v_filter));
  char *update = strdup(String_val(v_update));
  value r = mongo_write(OP_UPDATE, p, db, coll, filter, update);
  free(db);
  free(coll);
  free(filter);
  free(update);
  CAMLreturn(r);
}

CAMLprim value ocaml_mongo_delete_one(value v_pool, value v_db, value v_coll,
                                      value v_filter) {
  CAMLparam4(v_pool, v_db, v_coll, v_filter);
  mongo_pool *p = Pool_val(v_pool);
  char *db = strdup(String_val(v_db));
  char *coll = strdup(String_val(v_coll));
  char *filter = strdup(String_val(v_filter));
  value r = mongo_write(OP_DELETE, p, db, coll, filter, NULL);
  free(db);
  free(coll);
  free(filter);
  CAMLreturn(r);
}

/* --------------------------------------------------------- change streams --- */

CAMLprim value ocaml_mongo_watch_open(value v_pool, value v_db, value v_coll,
                                      value v_pipeline, value v_opts) {
  CAMLparam5(v_pool, v_db, v_coll, v_pipeline, v_opts);
  CAMLlocal1(res);
  mongo_pool *p = Pool_val(v_pool);
  char *db = strdup(String_val(v_db));
  char *coll = strdup(String_val(v_coll));
  char *pipej = strdup(String_val(v_pipeline));
  char *optsj = strdup(String_val(v_opts));
  mongo_stream *s = NULL;
  int ok = 0;
  bson_error_t error;
  error.message[0] = '\0';

  caml_enter_blocking_section();
  {
    bson_t pipeline, opts;
    /* a pipeline is a JSON array; wrap it as {"pipeline":[...]} so libbson can
     * parse it, then hand the array element to watch. Empty -> no pipeline. */
    int have_pipe = (pipej && pipej[0] != '\0');
    bson_t pipe_doc;
    int pp = 1;
    if (have_pipe) {
      char *wrapped = bson_strdup_printf("{\"pipeline\": %s}", pipej);
      pp = bson_init_from_json(&pipe_doc, wrapped, -1, &error);
      bson_free(wrapped);
    }
    if (pp && json_to_bson(optsj, &opts, &error)) {
      bson_iter_t it, arr;
      if (have_pipe && bson_iter_init_find(&it, &pipe_doc, "pipeline") &&
          BSON_ITER_HOLDS_ARRAY(&it)) {
        uint32_t len;
        const uint8_t *data;
        bson_iter_array(&it, &len, &data);
        bson_init_static(&pipeline, data, len);
      } else {
        bson_init(&pipeline);
      }
      (void)arr;
      mongoc_client_t *client = mongoc_client_pool_pop(p->pool);
      mongoc_collection_t *c = mongoc_client_get_collection(client, db, coll);
      mongoc_change_stream_t *cs =
          mongoc_collection_watch(c, &pipeline, &opts);
      const bson_t *err_doc;
      if (mongoc_change_stream_error_document(cs, &error, &err_doc)) {
        mongoc_change_stream_destroy(cs);
        mongoc_collection_destroy(c);
        mongoc_client_pool_push(p->pool, client);
        ok = 0;
      } else {
        s = (mongo_stream *)malloc(sizeof(mongo_stream));
        s->pool = p->pool;
        s->client = client;
        s->coll = c;
        s->stream = cs;
        s->closed = 0;
        ok = 1;
      }
      bson_destroy(&opts);
      if (have_pipe) bson_destroy(&pipe_doc);
    } else if (have_pipe && pp) {
      bson_destroy(&pipe_doc);
    }
  }
  caml_leave_blocking_section();

  free(db);
  free(coll);
  free(pipej);
  free(optsj);
  if (!ok) {
    char msg[512];
    snprintf(msg, sizeof msg, "watch: %s", error.message);
    caml_failwith(msg);
  }
  res = caml_alloc_custom(&stream_ops, sizeof(mongo_stream *), 0, 1);
  Stream_val(res) = s;
  CAMLreturn(res);
}

CAMLprim value ocaml_mongo_watch_next(value v_stream) {
  CAMLparam1(v_stream);
  CAMLlocal2(res, str);
  mongo_stream *s = Stream_val(v_stream);
  if (s->closed) caml_failwith("watch_next: stream is closed");
  char *out = NULL;
  int state = 0; /* 0 = timeout/none, 1 = event, -1 = error */
  bson_error_t error;
  error.message[0] = '\0';

  caml_enter_blocking_section();
  {
    const bson_t *doc;
    if (mongoc_change_stream_next(s->stream, &doc)) {
      out = bson_as_canonical_extended_json(doc, NULL);
      state = 1;
    } else if (mongoc_change_stream_error_document(s->stream, &error, NULL)) {
      state = -1;
    } else {
      state = 0;
    }
  }
  caml_leave_blocking_section();

  if (state == -1) {
    char msg[512];
    snprintf(msg, sizeof msg, "watch_next: %s", error.message);
    caml_failwith(msg);
  }
  if (state == 0) CAMLreturn(Val_int(0)); /* None */
  str = caml_copy_string(out);
  bson_free(out);
  res = caml_alloc(1, 0); /* Some str */
  Store_field(res, 0, str);
  CAMLreturn(res);
}

CAMLprim value ocaml_mongo_watch_close(value v_stream) {
  CAMLparam1(v_stream);
  stream_close(Stream_val(v_stream));
  CAMLreturn(Val_unit);
}

#else /* !HAVE_MONGOC — native driver not built; every entry point raises a clear error */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>

static value mongo_unavailable(void) {
  caml_failwith(
      "fennec-mongo: native driver not built (libmongoc unavailable at build time). Use the "
      "in-memory (:memory:) backend, or rebuild on a host with cmake + a C toolchain.");
  return Val_unit; /* unreachable */
}

CAMLprim value ocaml_mongo_init(value a) { (void)a; return Val_unit; }
CAMLprim value ocaml_mongo_available(value a) { (void)a; return Val_bool(0); }
CAMLprim value ocaml_mongo_pool_new(value a) { (void)a; return mongo_unavailable(); }
CAMLprim value ocaml_mongo_ping(value a, value b) { (void)a; (void)b; return mongo_unavailable(); }
CAMLprim value ocaml_mongo_command(value a, value b, value c) { (void)a; (void)b; (void)c; return mongo_unavailable(); }
CAMLprim value ocaml_mongo_find(value a, value b, value c, value d, value e) { (void)a; (void)b; (void)c; (void)d; (void)e; return mongo_unavailable(); }
CAMLprim value ocaml_mongo_aggregate(value a, value b, value c, value d, value e) { (void)a; (void)b; (void)c; (void)d; (void)e; return mongo_unavailable(); }
CAMLprim value ocaml_mongo_insert_one(value a, value b, value c, value d) { (void)a; (void)b; (void)c; (void)d; return mongo_unavailable(); }
CAMLprim value ocaml_mongo_update_one(value a, value b, value c, value d, value e) { (void)a; (void)b; (void)c; (void)d; (void)e; return mongo_unavailable(); }
CAMLprim value ocaml_mongo_delete_one(value a, value b, value c, value d) { (void)a; (void)b; (void)c; (void)d; return mongo_unavailable(); }
CAMLprim value ocaml_mongo_watch_open(value a, value b, value c, value d, value e) { (void)a; (void)b; (void)c; (void)d; (void)e; return mongo_unavailable(); }
CAMLprim value ocaml_mongo_watch_next(value a) { (void)a; return mongo_unavailable(); }
CAMLprim value ocaml_mongo_watch_close(value a) { (void)a; return mongo_unavailable(); }

#endif

