#!/usr/bin/env bash
# Emit (on stdout) the dune (c_library_flags ...) sexp listing the system
# libraries the esbuild (Go) and CSS (Rust) static archives need on THIS OS.
# These flags are passed straight to the C linker, so they are raw linker args
# (NO -cclib wrapping). Libraries the OCaml runtime already links (-lc/-lm) are
# omitted. The dune rule redirects this into link_flags.sexp.
set -euo pipefail

case "$(uname -s)" in
  Darwin)
    # Go: CoreFoundation + Security + resolv. Rust: iconv. notify (fs events):
    # CoreServices (FSEvents).
    printf '(-framework CoreFoundation -framework CoreServices -framework Security -lresolv -liconv)\n'
    ;;
  *)
    # Linux/glibc. Go: pthread + dl + resolv. Rust staticlib: gcc_s (unwind) +
    # rt + util + pthread + dl. notify uses inotify (no extra lib).
    printf '(-lpthread -ldl -lresolv -lrt -lutil -lgcc_s)\n'
    ;;
esac
