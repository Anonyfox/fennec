// Isomorphic integration test: proves the full fennec loop end to end.
//
//   1. SSR markup is present in the initial HTML (before any JS runs)
//   2. react.js installs the preact runtime as window.React (createRoot)
//   3. app.js hydrates the SAME component over the server-rendered #root
//   4. the hydrated component is interactive (button click increments state)
//
// Run against a live `fennec dev`/server on :8200 via `node hydration.test.mjs`,
// or it boots the built server itself when BOOT=1. Uses jsdom (devDependency).
import { JSDOM } from "jsdom";
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";

const BASE = process.env.BASE || "http://localhost:8200";
let serverProc = null;

async function ensureServer() {
  try {
    await fetch(BASE + "/");
    return; // already up
  } catch {
    /* boot it */
  }
  // default to the dune-built exe relative to the repo root (this file lives at
  // examples/helloworld/test/). Override with SERVER_EXE.
  const exe =
    process.env.SERVER_EXE ||
    new URL("../../../_build/default/examples/helloworld/server.exe", import.meta.url).pathname;
  serverProc = spawn(exe, [], { stdio: "ignore" });
  for (let i = 0; i < 50; i++) {
    try {
      await fetch(BASE + "/");
      return;
    } catch {
      await sleep(100);
    }
  }
  throw new Error("server did not start");
}

let failures = 0;
function check(name, cond) {
  console.log(`  ${cond ? "ok  " : "FAIL"} ${name}`);
  if (!cond) failures++;
}

await ensureServer();

const html = await (await fetch(BASE + "/")).text();
const reactJs = await (await fetch(BASE + "/react.js")).text();
const appJs = await (await fetch(BASE + "/app.js")).text();

// runScripts: "dangerously" lets <script> elements execute in the page's own
// context, so `window` resolves correctly inside the bundles (just like a real
// browser). We inject react.js then app.js as script elements.
const dom = new JSDOM(html, { pretendToBeVisual: true, runScripts: "dangerously" });
const { window } = dom;

// 1. SSR markup present before any JS
check("SSR renders h1", /Hello, world/.test(window.document.querySelector("h1")?.textContent));
check("SSR renders button at 0", /clicked 0 times/.test(window.document.querySelector("button")?.textContent));
check("props inlined", /"name":"world"/.test(html));

function injectScript(src) {
  const el = window.document.createElement("script");
  el.textContent = src;
  window.document.body.appendChild(el);
}

// 2. runtime
injectScript(reactJs);
check("react.js sets window.React.createRoot", typeof window.React?.createRoot === "function");

// 3. hydrate
injectScript(appJs);

// 4. interactivity
const btn = window.document.querySelector("button");
btn.dispatchEvent(new window.MouseEvent("click", { bubbles: true }));
btn.dispatchEvent(new window.MouseEvent("click", { bubbles: true }));
await sleep(50);
check("button increments after hydration", /clicked 2 times/.test(window.document.querySelector("button")?.textContent));

if (serverProc) serverProc.kill();
console.log(failures === 0 ? "isomorphic loop OK" : `${failures} check(s) failed`);
process.exit(failures === 0 ? 0 : 1);
