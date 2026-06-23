//! Local image processing for the `fennec image` subcommand: decode any common input format, optionally
//! resize / center-crop, and re-encode to a web format (jpeg/png/gif/webp/ico). Pure-Rust core via the
//! `image` crate, plus libwebp (`webp`) for lossy WebP. AVIF is intentionally omitted for now.
//!
//! The whole surface is ONE C entry point (`fennec_image_process`) + its free, kept binary-safe (the
//! payload is raw bytes, not a C string). The typed correctness — formats, geometry, fit — lives on the
//! OCaml side (`fennec_image`); across the boundary the options are a tiny `k=v;…` string, like esbuild.
//!
//! Safety: the crate is built `panic = "abort"`, so a panic would kill the CLI. Every fallible step
//! returns a `Result` (no `unwrap`/`expect` on inputs), so well-formed calls never panic.

use image::{imageops::FilterType, DynamicImage, ImageEncoder, ImageFormat};
use std::ffi::CStr;
use std::os::raw::{c_char, c_int};

fn read(p: *const c_char) -> String {
    if p.is_null() {
        return String::new();
    }
    unsafe { CStr::from_ptr(p) }.to_str().unwrap_or("").to_string()
}

#[derive(Default)]
struct Opts {
    w: u32,
    h: u32,
    fit_cover: bool,
    quality: u8,
    strip: bool,
}

// opts wire form: "w=800;h=600;fit=cover;q=80;strip=1" — any subset; an absent key keeps the default.
fn parse_opts(s: &str) -> Opts {
    let mut o = Opts::default();
    for kv in s.split(';') {
        if let Some((k, v)) = kv.split_once('=') {
            match k {
                "w" => o.w = v.parse().unwrap_or(0),
                "h" => o.h = v.parse().unwrap_or(0),
                "fit" => o.fit_cover = v == "cover",
                "q" => o.quality = v.parse().unwrap_or(0),
                "strip" => o.strip = v == "1",
                _ => {}
            }
        }
    }
    o
}

fn apply_resize(img: DynamicImage, o: &Opts) -> DynamicImage {
    let (iw, ih) = (img.width().max(1), img.height().max(1));
    match (o.w, o.h) {
        (0, 0) => img,
        // single dimension → scale the other to preserve aspect ratio
        (w, 0) => img.resize_exact(w, ((ih as u64 * w as u64) / iw as u64).max(1) as u32, FilterType::Lanczos3),
        (0, h) => img.resize_exact(((iw as u64 * h as u64) / ih as u64).max(1) as u32, h, FilterType::Lanczos3),
        // both → cover (fill the box, center-crop overflow) or contain (fit inside, aspect preserved)
        (w, h) if o.fit_cover => img.resize_to_fill(w, h, FilterType::Lanczos3),
        (w, h) => img.resize(w, h, FilterType::Lanczos3),
    }
}

fn encode(img: &DynamicImage, format: &str, o: &Opts) -> Result<Vec<u8>, String> {
    let mut out = std::io::Cursor::new(Vec::<u8>::new());
    match format {
        "webp" => {
            let rgba = img.to_rgba8();
            let enc = webp::Encoder::from_rgba(rgba.as_raw(), rgba.width(), rgba.height());
            let q = if o.quality == 0 { 80.0 } else { o.quality as f32 };
            return Ok(enc.encode(q).to_vec());
        }
        "jpeg" | "jpg" => {
            // JPEG has no alpha; flatten to RGB. Quality is honoured.
            let rgb = img.to_rgb8();
            let q = if o.quality == 0 { 80 } else { o.quality };
            image::codecs::jpeg::JpegEncoder::new_with_quality(&mut out, q)
                .write_image(rgb.as_raw(), rgb.width(), rgb.height(), image::ExtendedColorType::Rgb8)
                .map_err(|e| format!("jpeg encode: {e}"))?;
        }
        "png" => img.write_to(&mut out, ImageFormat::Png).map_err(|e| format!("png encode: {e}"))?,
        "gif" => img.write_to(&mut out, ImageFormat::Gif).map_err(|e| format!("gif encode: {e}"))?,
        "ico" => img.write_to(&mut out, ImageFormat::Ico).map_err(|e| format!("ico encode: {e} (ICO frames must be <= 256px)"))?,
        other => return Err(format!("unsupported output format: {other}")),
    }
    let _ = o.strip; // re-encoding through these encoders already emits none of the source metadata (EXIF/XMP/ICC)
    Ok(out.into_inner())
}

fn process(input: &[u8], format: &str, opts: &str) -> Result<Vec<u8>, String> {
    if input.is_empty() {
        return Err("empty input".to_string());
    }
    let o = parse_opts(opts);
    let img = image::load_from_memory(input).map_err(|e| format!("decode: {e}"))?;
    let img = apply_resize(img, &o);
    encode(&img, format, &o)
}

fn write_err(buf: *mut c_char, cap: c_int, msg: &str) {
    if buf.is_null() || cap <= 1 {
        return;
    }
    let n = msg.len().min(cap as usize - 1);
    unsafe {
        std::ptr::copy_nonoverlapping(msg.as_ptr(), buf as *mut u8, n);
        *buf.add(n) = 0;
    }
}

/// Process `input` (decoded by content) into `format`, applying `opts` (the `k=v;…` string). On success
/// returns the encoded bytes and writes the length to `out_len`; on failure writes `-1` to `out_len`,
/// the message into `err_buf` (capacity `err_cap`), and returns null. Free a success buffer with
/// [`fennec_image_free`].
#[no_mangle]
pub extern "C" fn fennec_image_process(
    input: *const u8,
    input_len: c_int,
    format: *const c_char,
    opts: *const c_char,
    out_len: *mut c_int,
    err_buf: *mut c_char,
    err_cap: c_int,
) -> *mut u8 {
    let bytes: &[u8] =
        if input.is_null() || input_len <= 0 { &[] } else { unsafe { std::slice::from_raw_parts(input, input_len as usize) } };
    match process(bytes, &read(format), &read(opts)) {
        Ok(v) => {
            let len = v.len();
            unsafe {
                *out_len = len as c_int;
            }
            Box::into_raw(v.into_boxed_slice()) as *mut u8
        }
        Err(e) => {
            write_err(err_buf, err_cap, &e);
            unsafe {
                *out_len = -1;
            }
            std::ptr::null_mut()
        }
    }
}

/// Free a buffer returned by [`fennec_image_process`] (reconstructs the boxed slice of `len` bytes).
#[no_mangle]
pub extern "C" fn fennec_image_free(ptr: *mut u8, len: c_int) {
    if !ptr.is_null() && len > 0 {
        unsafe {
            drop(Box::from_raw(std::slice::from_raw_parts_mut(ptr, len as usize)));
        }
    }
}
