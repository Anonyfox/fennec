#!/usr/bin/env bash
# Portability guard: assert the binary depends ONLY on shared libraries that ship with the host OS.
# This is what makes "no pre-install needed besides mongod" a *tested invariant* rather than a hope —
# if a build ever picks up a Homebrew/dev dynamic library (e.g. OpenSSL) instead of statically
# linking it, this fails loudly instead of shipping a broken downstream binary.
#
# Usage: check.sh <path-to-executable>
set -euo pipefail

bin="${1:?usage: check.sh <executable>}"
os="$(uname -s)"
bad=()

case "$os" in
  Darwin)
    # Everything under /usr/lib and /System is part of macOS (served from the dyld shared cache).
    # Anything else — /opt/homebrew, /usr/local, @rpath to a bundled dylib — is a portability defect.
    while IFS= read -r line; do
      path="$(printf '%s' "$line" | awk '{print $1}')"
      case "$path" in
        /usr/lib/*|/System/*) ;;            # OS-provided, fine
        "$bin"|"$bin:") ;;                   # the binary naming itself
        *) bad+=("$path") ;;
      esac
    done < <(otool -L "$bin" | tail -n +2)
    ;;

  Linux)
    # Allowlist of sonames that are part of any standard glibc/musl userland plus the few ubiquitous
    # libs we deliberately keep dynamic. NB: musl names its C library libc.musl-<arch>.so.1.
    allow='^(linux-vdso|ld-linux.*|ld-musl.*|libc\.musl-[a-z0-9_]+|libc|libm|libdl|libpthread|librt|libresolv|libgcc_s|libstdc\+\+|libz|libsasl2)\.so'
    while IFS= read -r line; do
      # ldd lines look like:  libfoo.so.1 => /path (0x..)  OR  /lib/ld.so (0x..)
      soname="$(printf '%s' "$line" | awk '{print $1}')"
      [ -z "$soname" ] && continue
      case "$soname" in
        *.so|*.so.*) : ;;
        *) continue ;;
      esac
      base="$(basename "$soname")"
      if ! printf '%s' "$base" | grep -Eq "$allow"; then
        bad+=("$base")
      fi
    done < <(ldd "$bin" 2>/dev/null || true)
    ;;

  *)
    echo "portability check: unsupported OS '$os' — skipping" >&2
    exit 0
    ;;
esac

if [ "${#bad[@]}" -gt 0 ]; then
  echo "PORTABILITY FAILURE: $bin depends on non-system libraries:" >&2
  printf '  %s\n' "${bad[@]}" >&2
  echo "These must be statically linked (see mongo/config/discover.ml)." >&2
  exit 1
fi

echo "portability OK: $bin depends only on OS-provided libraries"
