//! CSS/SCSS for Fennec: SCSS (grass: vars, mixins, @for, functions) -> optimize,
//! flatten nesting, reduce calc, minify, autoprefix-via-targets (Lightning CSS).
//! Exposed as a C staticlib statically linked into the OCaml binary.
use lightningcss::stylesheet::{MinifyOptions, ParserOptions, PrinterOptions, StyleSheet};
use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_int};

fn to_c(s: String) -> *mut c_char {
    CString::new(s).map(|c| c.into_raw()).unwrap_or(std::ptr::null_mut())
}
fn read(src: *const c_char) -> Option<String> {
    unsafe { CStr::from_ptr(src) }.to_str().ok().map(|s| s.to_string())
}

fn optimize(css: &str, minify: bool) -> Option<String> {
    let mut sheet = StyleSheet::parse(css, ParserOptions::default()).ok()?;
    if minify {
        sheet.minify(MinifyOptions::default()).ok()?;
    }
    sheet
        .to_css(PrinterOptions { minify, ..Default::default() })
        .ok()
        .map(|r| r.code)
}

/// Optimize modern CSS (nesting, calc, dedupe, minify).
#[no_mangle]
pub extern "C" fn fennec_css_transform(src: *const c_char, minify: c_int) -> *mut c_char {
    match read(src).and_then(|css| optimize(&css, minify != 0)) {
        Some(out) => to_c(out),
        None => std::ptr::null_mut(),
    }
}

/// Compile SCSS (grass) then optimize with Lightning CSS.
#[no_mangle]
pub extern "C" fn fennec_css_scss(src: *const c_char, minify: c_int) -> *mut c_char {
    let scss = match read(src) {
        Some(s) => s,
        None => return std::ptr::null_mut(),
    };
    let css = match grass::from_string(scss, &grass::Options::default()) {
        Ok(c) => c,
        Err(_) => return std::ptr::null_mut(),
    };
    match optimize(&css, minify != 0) {
        Some(out) => to_c(out),
        None => to_c(css),
    }
}

/// Compile a SCSS *file* (grass `from_path`, which resolves `@use`/`@import`
/// relative to the file and its directory — so a component's stylesheet can sit
/// next to it and be pulled in by an app's entry sheet). Then optimize.
#[no_mangle]
pub extern "C" fn fennec_css_scss_path(path: *const c_char, minify: c_int) -> *mut c_char {
    let p = match read(path) {
        Some(s) => s,
        None => return std::ptr::null_mut(),
    };
    let css = match grass::from_path(&p, &grass::Options::default()) {
        Ok(c) => c,
        Err(_) => return std::ptr::null_mut(),
    };
    match optimize(&css, minify != 0) {
        Some(out) => to_c(out),
        None => to_c(css),
    }
}

#[no_mangle]
pub extern "C" fn fennec_css_free(p: *mut c_char) {
    if !p.is_null() {
        unsafe { drop(CString::from_raw(p)) };
    }
}
