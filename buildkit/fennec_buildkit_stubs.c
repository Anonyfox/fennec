/* OCaml <-> native FFI for Fennec Buildkit.
   esbuild functions come from the Go c-archive; CSS from the Rust staticlib. */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <string.h>
#include <stdlib.h>

/* Go c-archive (fennec_esbuild): C.int <-> int, *C.char <-> char* */
extern int fennec_esbuild_ctx_create(char *opts_json);
extern char *fennec_esbuild_ctx_rebuild(int handle, int *out_len);
extern void fennec_esbuild_ctx_dispose(int handle);

/* Rust staticlib (fennec_css) */
extern char *fennec_css_transform(const char *src, int minify);
extern char *fennec_css_scss(const char *src, int minify);
extern void fennec_css_free(char *p);

/* ---- esbuild ---- */

CAMLprim value fennec_bk_ctx_create(value opts) {
  CAMLparam1(opts);
  CAMLreturn(Val_int(fennec_esbuild_ctx_create((char *)String_val(opts))));
}

CAMLprim value fennec_bk_ctx_rebuild(value handle) {
  CAMLparam1(handle);
  CAMLlocal1(res);
  int n = 0;
  char *out = fennec_esbuild_ctx_rebuild(Int_val(handle), &n);
  if (n == -2) { /* out holds the formatted error text */
    char buf[4096];
    if (out) {
      strncpy(buf, out, sizeof(buf) - 1);
      buf[sizeof(buf) - 1] = '\0';
      free(out);
    } else {
      strcpy(buf, "esbuild build error");
    }
    caml_failwith(buf);
  }
  if (n <= 0 || out == NULL) {
    res = caml_alloc_string(0);
  } else {
    res = caml_alloc_string(n);
    memcpy((char *)Bytes_val(res), out, n);
    free(out);
  }
  CAMLreturn(res);
}

CAMLprim value fennec_bk_ctx_dispose(value handle) {
  CAMLparam1(handle);
  fennec_esbuild_ctx_dispose(Int_val(handle));
  CAMLreturn(Val_unit);
}

/* ---- CSS / SCSS ---- */

static value copy_and_free(char *out, const char *err) {
  CAMLparam0();
  CAMLlocal1(res);
  if (out == NULL) caml_failwith(err);
  res = caml_copy_string(out);
  fennec_css_free(out);
  CAMLreturn(res);
}

CAMLprim value fennec_bk_css(value src, value minify) {
  CAMLparam2(src, minify);
  CAMLreturn(copy_and_free(fennec_css_transform((char *)String_val(src), Int_val(minify)),
                           "lightningcss: parse error"));
}

CAMLprim value fennec_bk_scss(value src, value minify) {
  CAMLparam2(src, minify);
  CAMLreturn(copy_and_free(fennec_css_scss((char *)String_val(src), Int_val(minify)),
                           "scss: compile error"));
}
