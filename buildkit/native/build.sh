#!/usr/bin/env bash
# Compile the native bundler archives for the HOST platform. Run by the dune
# archive rule; cwd is the package build dir, where native/ has been copied.
# Outputs into cwd:  libfennec_esbuild.a  libfennec_esbuild.h  libfennec_css.a
set -euo pipefail

out="$(pwd)"

# esbuild -> Go c-archive (produces .a + .h)
( cd native/go && go build -buildmode=c-archive -o "$out/libfennec_esbuild.a" . )

# css/scss -> Rust staticlib
( cd native/rust && cargo build --release --quiet )
cp native/rust/target/release/libfennec_css.a "$out/libfennec_css.a"

echo "[fennec-buildkit] native archives built"
