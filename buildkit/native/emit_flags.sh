#!/usr/bin/env bash
# Emit, on stdout-redirect, the dune linker flags (an sexp) for the system
# libraries the esbuild (Go) and CSS (Rust) static archives need on THIS OS.
# Written as a single dune target so (c_library_flags (:include ...)) is
# unambiguous. Libraries the OCaml runtime already links (-lc/-lm) are omitted.
set -euo pipefail

case "$(uname -s)" in
  Darwin)
    # Go: CoreFoundation + Security + resolv. Rust: iconv.
    printf '(-cclib -framework -cclib CoreFoundation -cclib -framework -cclib Security -cclib -lresolv -cclib -liconv)\n'
    ;;
  *)
    # Linux/glibc. Go: pthread + dl + resolv. Rust staticlib: gcc_s (unwind) +
    # rt + util + pthread + dl.
    printf '(-cclib -lpthread -cclib -ldl -cclib -lresolv -cclib -lrt -cclib -lutil -cclib -lgcc_s)\n'
    ;;
esac
