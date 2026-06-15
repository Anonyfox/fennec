#!/bin/sh
# JS-BOUNDARY GATE. The reactive layer (fennec.pulse) must cross-compile to JavaScript over the
# in-memory backend, so its bytecode LINK CLOSURE must contain NO native compilation unit — no libmongoc
# driver, no LMDB/Burrow engine, no mongosh wire server, no TLS. We inspect the closure of a tiny
# executable (js_gate) that links the reactive stack over Backend.Mini. If a native dependency ever
# becomes reachable from fennec.pulse, its units appear in this closure and the gate FAILS the build — so
# the kind of silent rot that ships a broken/impossible client bundle is caught immediately.
#
# We inspect the .bc closure (ocamlobjinfo "compilation unit" lines), NOT the .bc.js: js_of_ocaml's
# runtime bundles dead caml_failwith stubs for many C primitives into EVERY .bc.js, so grepping the
# .bc.js gives false hits. The bytecode closure is the ground truth for what actually links (and ships).
set -eu
bc="$1"

units=$(ocamlobjinfo "$bc" 2>/dev/null | grep -E "compilation unit" || true)
# sanity: if inspection produced nothing the gate cannot vouch for anything — fail loudly, never pass silently
if [ -z "$units" ]; then
  echo "js-boundary gate ERROR: ocamlobjinfo found no compilation units in $bc — cannot verify the closure." >&2
  exit 1
fi

violations=$(printf '%s\n' "$units" \
  | grep -iE "Burrow|Mongo_ffi|Mongoc|Fennec_mongo_dynamic|Fennec_mongo_driver|Lmdb|Wire_server|Adapters|Tls_eio" || true)
if [ -n "$violations" ]; then
  echo "JS BOUNDARY VIOLATION: native units are reachable from the reactive (fennec.pulse) closure —" >&2
  printf '%s\n' "$violations" | sed 's/^/    /' >&2
  echo "fennec.pulse must depend only on the PURE seam (fennec-mongo.backend); the native backends live" >&2
  echo "in fennec-mongo.dynamic (libmongoc + LMDB + TLS), which must never reach a JS-shipped lib." >&2
  exit 1
fi

echo "js-boundary OK: fennec.pulse closure is native-free ($(printf '%s\n' "$units" | wc -l | tr -d ' ') units) — safe to cross-compile to JS"
