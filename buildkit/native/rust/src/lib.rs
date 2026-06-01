//! CSS/SCSS for Fennec: SCSS (grass: vars, mixins, @for, functions) -> optimize,
//! flatten nesting, reduce calc, minify, autoprefix-via-targets (Lightning CSS).
//! Exposed as a C staticlib statically linked into the OCaml binary.
use lightningcss::stylesheet::{MinifyOptions, ParserOptions, PrinterOptions, StyleSheet};
use notify::{Config, Event, RecommendedWatcher, RecursiveMode, Watcher};
use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_int, c_void};
use std::path::Path;
use std::sync::mpsc::{channel, Receiver};
use std::time::Duration;

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

// ---- filesystem events (CLI dev supervisor) ----
//
// Cross-platform fs watching via the `notify` crate. The handle owns the watcher
// (kept alive) and the event channel. [wait] blocks until an event arrives or the
// timeout elapses — so the supervisor reacts to a finished rebuild the instant the
// OS reports it, with no polling, while the timeout still bounds crash-checking.

struct WatchHandle {
    watcher: RecommendedWatcher,
    rx: Receiver<Result<Event, notify::Error>>,
}

fn mode(recursive: c_int) -> RecursiveMode {
    if recursive != 0 {
        RecursiveMode::Recursive
    } else {
        RecursiveMode::NonRecursive
    }
}

/// Start a watcher on [path] with the given recursion mode. Returns an opaque
/// handle, or null on failure (caller then falls back to polling). More paths can
/// be added with [fennec_watch_add] — all feed the one event channel. This lets
/// the supervisor watch the exe's dir (non-recursive) and the served web root
/// (recursive, a small stable subtree) WITHOUT ever recursing into node_modules /
/// build trees, which on Linux would exhaust inotify watches and flood events.
#[no_mangle]
pub extern "C" fn fennec_watch_start(path: *const c_char, recursive: c_int) -> *mut c_void {
    let p = match read(path) {
        Some(s) => s,
        None => return std::ptr::null_mut(),
    };
    let (tx, rx) = channel();
    let mut watcher = match RecommendedWatcher::new(tx, Config::default()) {
        Ok(w) => w,
        Err(_) => return std::ptr::null_mut(),
    };
    if watcher.watch(Path::new(&p), mode(recursive)).is_err() {
        return std::ptr::null_mut();
    }
    Box::into_raw(Box::new(WatchHandle { watcher, rx })) as *mut c_void
}

/// Add another [path] (with its own recursion mode) to an existing watcher; its
/// events arrive on the same channel. Returns 1 on success, 0 on failure.
#[no_mangle]
pub extern "C" fn fennec_watch_add(handle: *mut c_void, path: *const c_char, recursive: c_int) -> c_int {
    if handle.is_null() {
        return 0;
    }
    let p = match read(path) {
        Some(s) => s,
        None => return 0,
    };
    let h = unsafe { &mut *(handle as *mut WatchHandle) };
    if h.watcher.watch(Path::new(&p), mode(recursive)).is_ok() {
        1
    } else {
        0
    }
}

/// Block until an fs event arrives or [timeout_ms] elapses. Returns 1 if at least
/// one event was seen (draining any others), 0 on timeout/disconnect.
#[no_mangle]
pub extern "C" fn fennec_watch_wait(handle: *mut c_void, timeout_ms: c_int) -> c_int {
    if handle.is_null() {
        return 0;
    }
    let h = unsafe { &*(handle as *const WatchHandle) };
    match h.rx.recv_timeout(Duration::from_millis(timeout_ms.max(0) as u64)) {
        Ok(_) => {
            while h.rx.try_recv().is_ok() {}
            1
        }
        Err(_) => 0,
    }
}

#[no_mangle]
pub extern "C" fn fennec_watch_free(handle: *mut c_void) {
    if !handle.is_null() {
        unsafe { drop(Box::from_raw(handle as *mut WatchHandle)) };
    }
}
