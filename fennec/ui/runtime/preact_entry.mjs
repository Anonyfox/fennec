// The React runtime for the client bundle: preact/compat (~12KB gzipped) exposed
// as window.React / window.ReactDOM / window.JsxRuntime. The app's Melange bundle
// externalizes react/react-dom/react/jsx-runtime to these globals (via the shim
// banner), so the heavy runtime ships once, separately, and never rebuilds.
//
// preact/compat ships only the legacy render/hydrate API; reason-react's
// ReactDOM.Client emits createRoot/hydrateRoot, so we add a React-18 root shim.
import * as preactCompat from "preact/compat";
import { render, hydrate } from "preact/compat";
import * as JsxRuntime from "preact/jsx-runtime";

const R = Object.assign({}, preactCompat);

if (!R.createRoot) {
  R.createRoot = (container) => ({
    render: (el) => render(el, container),
    unmount: () => render(null, container),
  });
}
if (!R.hydrateRoot) {
  R.hydrateRoot = (container, el) => {
    hydrate(el, container);
    return {
      render: (e) => render(e, container),
      unmount: () => render(null, container),
    };
  };
}

window.React = R;
window.ReactDOM = R;
window.JsxRuntime = JsxRuntime;
