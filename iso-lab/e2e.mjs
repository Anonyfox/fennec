import { JSDOM } from 'jsdom';
import { readFileSync } from 'node:fs';

const html = readFileSync(new URL('./index.html', import.meta.url), 'utf8');
// deep-link: the page was SSR'd for /shop/products/7 (app mounted at base /shop)
const dom = new JSDOM(html, {
  url: 'http://localhost/shop/products/7',
  runScripts: 'dangerously',
  pretendToBeVisual: true,
  beforeParse(w) { w.TextDecoder = globalThis.TextDecoder; w.TextEncoder = globalThis.TextEncoder; },
});
const { document } = dom.window;

let pass = 0, fail = 0;
const eq = (label, got, want) => {
  if (String(got) === String(want)) { pass++; console.log(`  ✓ ${label} = ${got}`); }
  else { fail++; console.log(`  ✗ ${label}: got ${JSON.stringify(got)}, want ${JSON.stringify(want)}`); }
};
const wait = ms => new Promise(r => setTimeout(r, ms));

const pageName   = () => document.querySelector('.page')?.getAttribute('data-page');
const h1         = () => document.querySelector('.page h1')?.textContent;
const headTitle  = () => document.querySelector('head title')?.textContent;
const titleCount = () => document.querySelectorAll('head title').length;
const attr       = (sel, n) => document.querySelector(sel)?.getAttribute(n);
const path       = () => dom.window.location.pathname;
const fire       = el => el.dispatchEvent(new dom.window.MouseEvent('click', { bubbles: true, cancelable: true }));
const click      = sel => fire(document.querySelector(sel));
const counterInc = () => fire(document.querySelector('.counter').querySelectorAll('button')[1]);
const todos      = () => document.querySelectorAll('.todos li').length;
const stats      = () => document.querySelector('.stats')?.textContent;

(async () => {
  await wait(120);

  console.log('— deep-link hydration (mounted at base /shop, route is RELATIVE) —');
  eq('page = product', pageName(), 'product');
  eq('param rendered', h1(), 'Product #7');
  eq('per-page title', headTitle(), 'Product 7 · shop');
  eq('location', path(), '/shop/products/7');

  console.log('— typed path sigil (p): base auto-prefixed; ext: outer reach —');
  eq('nav-home href (p "/")', attr('.nav-home', 'href'), '/shop');
  eq('nav-products href (p "/products")', attr('.nav-products', 'href'), '/shop/products');
  eq('back link href', attr('.back', 'href'), '/shop/products');
  eq('ext link is NOT base-prefixed', attr('.nav-admin', 'href'), '/admin');

  console.log('— SPA nav: click "← all products" (no reload) —');
  click('.back');
  await wait(20);
  eq('page = products', pageName(), 'products');
  eq('title swapped', headTitle(), 'Products · shop');
  eq('pushState updated location', path(), '/shop/products');
  eq('still exactly one <title> (Head.use cleaned up on unmount)', titleCount(), 1);

  console.log('— outer-reach (ext) link is NOT hijacked by the in-scope interceptor —');
  click('.nav-admin');
  await wait(20);
  eq('ext click left our route untouched', pageName(), 'products');
  eq('ext click did not pushState within app', path(), '/shop/products');

  console.log('— global store survives nav; add a todo here —');
  click('#add');
  eq('todo added', todos(), 1);
  eq('stats', stats(), 'todos in store: 1');

  console.log('— reactive params: click Product 9 (same pattern, new param) —');
  click('.p9');
  await wait(20);
  eq('page = product', pageName(), 'product');
  eq('param updated (remount)', h1(), 'Product #9');
  eq('title tracks param', headTitle(), 'Product 9 · shop');
  eq('location', path(), '/shop/products/9');
  eq('one <title> (no accumulation across 3 page visits)', titleCount(), 1);

  console.log('— nav Home: local state + client-side data + client-only + on_mount —');
  click('.nav-home');
  await wait(120);  // greeting + browser-only fetch (30ms) + mounts
  eq('page = home', pageName(), 'home');
  eq('title', headTitle(), 'Home · shop');
  counterInc(); counterInc();
  eq('local counter state', document.querySelector('.counter .count').textContent, 2);
  eq('greeting fetched on client nav (not seeded)', document.querySelector('.greeting .msg').textContent, 'Hello again, from the client 🔁');
  eq('client-only data loaded', document.querySelector('.browser-box .bdata').textContent, 'Loaded in the browser ✨');
  eq('on_mount ran', document.querySelector('.browser-box .mounted').textContent, 'mounted ✓');

  console.log('— nav back to Products: global store PERSISTED (proves no full reload) —');
  click('.nav-products');
  await wait(20);
  eq('page = products', pageName(), 'products');
  eq('todo still there (global store survived SPA nav)', todos(), 1);

  console.log('— browser back button (popstate) —');
  // history: …/products/9 -> /shop (home) -> /shop/products ; back() from /shop/products lands on Home
  dom.window.history.back();
  await wait(30);
  eq('popstate navigates (no reload)', pageName(), 'home');
  eq('popstate updated location', path(), '/shop');

  console.log('— SSR payload was empty (product page has no resources) —');
  eq('empty data context embedded', /window\.__ISO_DATA__=\{\}/.test(html), true);

  console.log(`\n${fail === 0 ? 'PASS' : 'FAIL'} — ${pass} passed, ${fail} failed`);
  process.exit(fail === 0 ? 0 : 1);
})();
