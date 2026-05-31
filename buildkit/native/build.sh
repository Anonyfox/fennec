#!/usr/bin/env bash
# Build the native bundler archives (esbuild=Go, css=Rust) for the HOST platform
# and emit the exact dune linker flags this platform needs. Run by the dune rule;
# cwd is the package build dir, where native/ has been copied. Outputs into cwd:
#   libfennec_esbuild.a  libfennec_esbuild.h  libfennec_css.a  link_flags.sexp
set -euo pipefail

out="$(pwd)"
os="$(uname -s)"

# 1) esbuild -> Go c-archive (produces .a AND .h next to it; the C stub declares
#    the Go symbols by hand, so the header is discarded to avoid a dune target clash)
( cd native/go && go build -buildmode=c-archive -o "$out/libfennec_esbuild.a" . )
rm -f "$out/libfennec_esbuild.h"

# 2) css/scss -> Rust staticlib
( cd native/rust && cargo build --release --quiet )
cp native/rust/target/release/libfennec_css.a "$out/libfennec_css.a"

# 3) the exact native libs the Rust staticlib needs on this platform
rust_libs="$(cd native/rust && cargo rustc --release --quiet -- --print native-static-libs 2>&1 \
  | sed -n 's/.*native-static-libs:[[:space:]]*//p' | head -1 || true)"

# 4) emit per-OS dune link flags, deduped, skipping libs the OCaml runtime already
#    links (-lSystem/-lc/-lm) to avoid duplicate-library warnings.
sexp="("
seen=" "
add() { case "$seen" in *" $1 "*) : ;; *) sexp="$sexp -cclib $1"; seen="$seen$1 ";; esac; }

if [ "$os" = "Darwin" ]; then
  # Go (CoreFoundation/Security/resolv) — frameworks are two-token flags
  sexp="$sexp -cclib -framework -cclib CoreFoundation -cclib -framework -cclib Security"
  add "-lresolv"
else
  # Go on Linux needs pthread/dl; resolv for cgo DNS
  add "-lpthread"; add "-ldl"; add "-lresolv"
fi

for l in $rust_libs; do
  case "$l" in -lSystem|-lc|-lm|"") continue ;; esac
  add "$l"
done

sexp="$sexp )"
printf '%s\n' "$sexp" > "$out/link_flags.sexp"
echo "[fennec-buildkit] $os archives built; link flags: $sexp"
