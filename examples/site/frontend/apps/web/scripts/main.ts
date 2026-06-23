// A DROPPED vendor script — the kind of third-party JS/TS you can't get as OCaml (a charting lib, a
// widget, an analytics snippet). esbuild bundles this entry (resolving any `import`s + transpiling the
// TS) into one IIFE staged at /_apps/web/vendor.js, loaded BEFORE the jsoo app bundle — so the globals
// it defines are reachable from OCaml via js_of_ocaml FFI at hydration. Drop more files into scripts/
// and `import` them here, in the load order you want.

const VERSION: string = "1.0"; // a TS type annotation — esbuild transpiles it away

// expose a vendor API on window (FFI-reachable from a component: Js.Unsafe.global##.fennecVendor)
(window as any).fennecVendor = {
  version: VERSION,
  greet: (who: string): string => `vendor says hi to ${who}`,
};

// an observable side effect, proving the bundle ran (before hydration) — the browser test asserts this
document.documentElement.setAttribute("data-vendor", "ready");
