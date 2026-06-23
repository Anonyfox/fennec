# `fennec image` — local image processing, batteries included

Convert, resize, centre-crop, and strip metadata — and build favicons — from one CLI, with **no
ImageMagick, no libvips, no node tooling** to install. The engine (the Rust [`image`] crate + libwebp)
is linked into the `fennec` binary the same way the CSS engine and esbuild are, so it's just there.

```
fennec image in.jpg out.webp                        # convert (format from the extension)
fennec image hero.jpg hero.webp -r 1280 -q 80       # resize to 1280 wide, quality 80
fennec image in.png out.webp -r 800x600 --fit cover # fill the box, centre-crop the overflow
fennec image photo.jpg -f webp                      # -> photo.webp (no output path needed)
fennec image favicon logo.png -o public/            # the favicon set + the <head> snippet
```

Formats: **jpg, png, gif, webp, ico** (in and out; many more decode-only). AVIF is intentionally left
out for now — its encoder (`rav1e`) is by far the heaviest dependency; WebP covers ~90% of the need.

## The flags (one stage each, combine freely)

| flag | meaning |
|---|---|
| `-o, --out PATH` | output path (or, for `favicon`, the output directory) — alt to the `OUTPUT` positional |
| `-f, --format`   | `jpg`/`png`/`gif`/`webp`/`ico`; else inferred from the output extension |
| `-r, --resize`   | `800` (width), `x600` (height), or `800x600` (box; aspect preserved) |
| `--fit`          | for a `WxH` box: `contain` (fit inside, default) or `cover` (fill + centre-crop) |
| `-q, --quality`  | 1–100 for lossy formats (jpeg/webp); a sane per-format default otherwise |
| `-s, --strip`    | drop metadata (EXIF / XMP / ICC) |

`fennec image favicon INPUT -o DIR` generates `favicon.ico`, `apple-touch-icon.png`, the PWA
`icon-192.png` / `icon-512.png`, a `manifest.webmanifest`, and prints the `<link>` tags.

## Structure — a clean isolated library

The logic is `fennec_image` (this directory); the native engine is reused from `fennec_buildkit`. The
typed boundary means the pipeline is total and an unknown format/geometry is rejected once, at the CLI.

| module | role |
|---|---|
| `Format` | the output formats — a closed variant, so encode is exhaustive (`of_string` / `of_extension` / `extension`). |
| `Geometry` | the `-r` request — `Width` / `Height` / `Box`, parsed from `W`/`WxH`/`Wx`/`xH`. |
| `Op` | the typed transform options (`fit`, quality, strip) + serialisation to the engine's `k=v;…` wire string. |
| `Engine` | the one FFI boundary — typed `Format.t` + `Op.t` → `Fennec_buildkit.Image.process` → bytes, errors as `result`. |
| `Favicon` | the icon set (data) + `generate` + the HTML/manifest snippets. |

The native seam lives in `buildkit`: `buildkit/native/rust/src/image.rs` (`fennec_image_process` —
decode → resize/fit → encode), the C stub in `buildkit/fennec_buildkit_stubs.c`, and the OCaml
`Fennec_buildkit.Image.process`. Binary in/out (not a C string), so it carries an explicit length. The
Rust is built `panic = "abort"`, so the engine uses `Result` everywhere — a well-formed call never
panics. Adds ~5 MB to the CLI.
