#!/usr/bin/env bash
# Build mongo-c-driver (libmongoc + libbson) from source as *static* archives with a *native* TLS
# backend, then install into a persistent per-OS cache. This is what makes the downstream app binary
# self-contained across the prod matrix (Linux + macOS) — see config/discover.ml + portability/check.sh.
#
# Why from source (vs. a Homebrew/apt bottle):
#   - Native TLS: Secure Transport on macOS, OpenSSL on Linux. The driver then trusts the OS
#     certificate store directly (Keychain / /etc/ssl), so TLS to Atlas works on a clean host with
#     zero extra setup. A statically-linked Homebrew OpenSSL would bake in Homebrew's OPENSSLDIR and
#     fail on a host that never `brew install`ed anything.
#   - Static only (ENABLE_SHARED=OFF): we link the .a archives straight into the OCaml stubs, so the
#     final binary needs no uncommon shared libraries — besides mongod, nothing to pre-install.
#     (Locked in by portability/check.sh.)
#   - SASL/zstd/snappy off, zlib bundled: sheds optional deps. SCRAM auth (the MongoDB default) runs
#     through the TLS backend's crypto, not Cyrus SASL.
#
# Idempotent: if the cache prefix already holds a built driver for this exact (version, OS, backend),
# it exits immediately. The cache lives outside the repo and survives `dune clean`, so only the very
# first build is slow.
#
# Output (stdout, last line): the absolute install prefix. config/discover.ml reads it.
set -euo pipefail

VERSION="2.3.0"
TARBALL_SHA256="0ef2c33345482d444ef766ebf3f066b4596bd6867a24ab6889b76dd51cb23878"
URL="https://github.com/mongodb/mongo-c-driver/releases/download/${VERSION}/mongo-c-driver-${VERSION}.tar.gz"

# --- pick the native TLS backend for this OS ----------------------------------
os="$(uname -s)"
case "$os" in
  Darwin) backend="darwin"; ssl="DARWIN" ;;
  Linux)  backend="openssl"; ssl="OPENSSL" ;;
  *) echo "build.sh: unsupported OS '$os' (native mongo driver — :memory: still works)" >&2; exit 1 ;;
esac

# --- cache layout (survives dune clean) ---------------------------------------
cache_root="${FENNEC_MONGO_CACHE:-${XDG_CACHE_HOME:-$HOME/.cache}/fennec-mongo-c}"
prefix="${cache_root}/${VERSION}-${os}-${backend}"
stamp="${prefix}/.fennec-mongo-built"

# Fast path: already built. Emit the prefix and stop.
if [ -f "$stamp" ]; then
  printf '%s\n' "$prefix"
  exit 0
fi

# Everything below only runs on the first build for this triple.
echo "build.sh: building mongo-c-driver ${VERSION} (${os}/${ssl}) -> ${prefix}" >&2

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

tarball="${work}/mcd.tar.gz"

# --- fetch + verify -----------------------------------------------------------
# Reuse the repo-vendored tarball if its checksum matches; else download.
vendored="$(cd "$(dirname "$0")" && pwd)/mcd.tar.gz"
sha_of() {
  if command -v shasum >/dev/null 2>&1; then shasum -a 256 "$1" | awk '{print $1}'
  else sha256sum "$1" | awk '{print $1}'; fi
}
if [ -f "$vendored" ] && [ "$(sha_of "$vendored")" = "$TARBALL_SHA256" ]; then
  cp "$vendored" "$tarball"
else
  curl -fsSL "$URL" -o "$tarball"
fi
got="$(sha_of "$tarball")"
if [ "$got" != "$TARBALL_SHA256" ]; then
  echo "build.sh: checksum mismatch for source tarball" >&2
  echo "  expected $TARBALL_SHA256" >&2
  echo "  got      $got" >&2
  exit 1
fi

# --- extract ------------------------------------------------------------------
src="${work}/src"
mkdir -p "$src"
tar xzf "$tarball" -C "$src" --strip-components=1

# --- configure ----------------------------------------------------------------
build="${work}/build"
cmake -S "$src" -B "$build" -G "Unix Makefiles" \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX="$prefix" \
  -DENABLE_MONGOC=ON \
  -DENABLE_SHARED=OFF \
  -DENABLE_PIC=ON \
  -DENABLE_STATIC=ON \
  -DENABLE_SSL="$ssl" \
  -DENABLE_SASL=OFF \
  -DENABLE_SNAPPY=OFF \
  -DENABLE_ZSTD=OFF \
  -DENABLE_ZLIB=BUNDLED \
  -DENABLE_SRV=ON \
  -DENABLE_CLIENT_SIDE_ENCRYPTION=OFF \
  -DENABLE_MONGODB_AWS_AUTH=OFF \
  -DENABLE_TESTS=OFF \
  -DENABLE_EXAMPLES=OFF \
  -DENABLE_MAN_PAGES=OFF \
  -DENABLE_HTML_DOCS=OFF \
  -DENABLE_UNINSTALL=OFF \
  >&2

# --- build + install ----------------------------------------------------------
ncpu="$( (command -v nproc >/dev/null 2>&1 && nproc) || sysctl -n hw.ncpu 2>/dev/null || echo 2)"
cmake --build "$build" --parallel "$ncpu" >&2
cmake --install "$build" >&2

# --- stamp + report -----------------------------------------------------------
# The stamp is the idempotency gate: its presence means "fully installed".
date > "$stamp"
printf '%s\n' "$prefix"
