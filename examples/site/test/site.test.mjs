// Isomorphic integration test for the multi-endpoint site. Boots the server,
// then for the WEB app: checks SSR markup + metadata, drives the real
// react.js + app.js bundles through jsdom to prove hydration (button), and that
// the client Head runtime set document.title. Also checks the ADMIN app is a
// distinct bundle/endpoint with its own metadata.
import { JSDOM } from "jsdom";
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";

const WEB = "http://localhost:8200";
const ADMIN = "http://localhost:8201";
let proc = null;

async function ensure() {
  try { await fetch(WEB + "/"); return; } catch {}
  const exe = process.env.SERVER_EXE ||
    new URL("../../../_build/default/examples/site/server.exe", import.meta.url).pathname;
  proc = spawn(exe, [], { stdio: "ignore" });
  for (let i = 0; i < 50; i++) { try { await fetch(WEB + "/"); return; } catch { await sleep(100); } }
  throw new Error("server did not start");
}

let fails = 0;
const check = (n, c) => { console.log(`  ${c ? "ok  " : "FAIL"} ${n}`); if (!c) fails++; };

await ensure();

// --- WEB app: SSR + metadata ---
const html = await (await fetch(WEB + "/")).text();
const reactJs = await (await fetch(WEB + "/react.js")).text();
const appJs = await (await fetch(WEB + "/app.js")).text();
check("web SSR renders h1", /Welcome to the Fennec site/.test(html));
check("web SSR title (Head)", /<title>Home — Fennec Site<\/title>/.test(html));
check("web SSR og:title (Head)", /property="og:title"/.test(html));
check("web counter present", /clicks: 0/.test(html));

// --- WEB app: hydration + client Head ---
const dom = new JSDOM(html, { pretendToBeVisual: true, runScripts: "dangerously", url: WEB + "/" });
const w = dom.window;
const inject = (s) => { const e = w.document.createElement("script"); e.textContent = s; w.document.body.appendChild(e); };
inject(reactJs);
check("react.js installs window.React", typeof w.React?.createRoot === "function");
inject(appJs);
await sleep(80);
const btn = w.document.querySelector(".counter");
btn?.dispatchEvent(new w.MouseEvent("click", { bubbles: true }));
btn?.dispatchEvent(new w.MouseEvent("click", { bubbles: true }));
await sleep(50);
check("web hydrated + interactive", /clicks: 2/.test(w.document.querySelector(".counter")?.textContent || ""));
check("web CSR set document.title", /Home — Fennec Site/.test(w.document.title));

// --- ADMIN app: distinct bundle + metadata ---
const adminHtml = await (await fetch(ADMIN + "/")).text();
const adminJs = await (await fetch(ADMIN + "/app.js")).text();
check("admin SSR title", /<title>Dashboard — Admin<\/title>/.test(adminHtml));
check("admin distinct bundle", adminJs !== appJs);
check("admin whitelabel body class", /class="admin"/.test(adminHtml));

// --- API + static ---
check("web API", /"app":"web"/.test(await (await fetch(WEB + "/api/health")).text()));
check("admin robots disallow", /Disallow: \//.test(await (await fetch(ADMIN + "/robots.txt")).text()));

if (proc) proc.kill();
console.log(fails === 0 ? "site loop OK" : `${fails} check(s) failed`);
process.exit(fails === 0 ? 0 : 1);
