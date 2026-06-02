import { JSDOM } from 'jsdom';
import { readFileSync } from 'node:fs';

const html = readFileSync(new URL('./index.html', import.meta.url), 'utf8');
// the page was SSR'd for the deep link /shop/products/7 (app mounted at base /shop)
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

const page       = () => document.querySelector('.page')?.getAttribute('data-page');
const h1         = () => document.querySelector('.page h1')?.textContent;
const headTitle  = () => document.querySelector('head title')?.textContent;
const titleCount = () => document.querySelectorAll('head title').length;
const descCount  = () => document.querySelectorAll('head meta[name="description"]').length;
const og         = () => document.querySelector('head meta[property="og:title"]')?.getAttribute('content');
const attr       = (sel, n) => document.querySelector(sel)?.getAttribute(n);
const text       = sel => document.querySelector(sel)?.textContent;
const path       = () => dom.window.location.pathname;
const fire       = el => el.dispatchEvent(new dom.window.MouseEvent('click', { bubbles: true, cancelable: true }));
const click      = sel => fire(document.querySelector(sel));
const counterBtn = i => document.querySelector('.counter').querySelectorAll('button')[i]; // 0=−, 1=+
const todos      = () => document.querySelectorAll('.todos li').length;
const stats      = () => text('.stats');
// jump to an arbitrary in-app URL the way the browser would (history + popstate)
const goto = p => { dom.window.history.pushState({}, '', p); dom.window.dispatchEvent(new dom.window.Event('popstate')); };

(async () => {
  await wait(120);

  console.log('— deep-link hydration: product page, base-relative route —');
  eq('page', page(), 'product');
  eq('param', h1(), 'Product #7');
  eq('title', headTitle(), 'Product 7 · shop');
  eq('location', path(), '/shop/products/7');
  eq('one <title>', titleCount(), 1);
  eq('layout og survives across pages', og(), 'iso shop');

  console.log('— nav hrefs: typed p (base-prefixed) + ext (outer reach) —');
  eq('nav-home', attr('.nav-home', 'href'), '/shop');
  eq('nav-products', attr('.nav-products', 'href'), '/shop/products');
  eq('nav-admin (ext, no base)', attr('.nav-admin', 'href'), '/admin');
  eq('back link', attr('.back', 'href'), '/shop/products');

  console.log('— back link → products —');
  click('.back'); await wait(20);
  eq('page', page(), 'products');
  eq('title', headTitle(), 'Products · shop');
  eq('location', path(), '/shop/products');
  eq('one <title> (Head cleanup on unmount)', titleCount(), 1);
  eq('one description', descCount(), 1);
  eq('product links built by p', attr('.p7', 'href'), '/shop/products/7');
  eq('product links built by p', attr('.p9', 'href'), '/shop/products/9');

  console.log('— FORM: type into the input (reads target_value), add, controlled clear —');
  eq('stats initial', stats(), 'todos in store: 0');
  const typeAdd = txt => {
    const input = document.querySelector('#todo-input');
    input.value = txt;
    input.dispatchEvent(new dom.window.Event('input', { bubbles: true }));  // -> onInput reads target_value
    click('#add');
  };
  typeAdd('milk'); typeAdd('eggs');
  eq('two typed todos', todos(), 2);
  eq('typed value used (not a fixed string)', document.querySelector('.todos li').firstChild.textContent.trim(), 'milk');
  eq('controlled input cleared after add', document.querySelector('#todo-input').value, '');
  eq('stats', stats(), 'todos in store: 2');
  click('.todos li .rm');                 // remove first (keyed reconcile)
  eq('one todo after remove', todos(), 1);
  eq('now first is eggs', document.querySelector('.todos li').firstChild.textContent.trim(), 'eggs');
  eq('stats after remove', stats(), 'todos in store: 1');

  console.log('— reactive param: click Product 9 —');
  click('.p9'); await wait(20);
  eq('page', page(), 'product');
  eq('param', h1(), 'Product #9');
  eq('title tracks param', headTitle(), 'Product 9 · shop');
  eq('location', path(), '/shop/products/9');
  eq('still one <title> after 3 page visits', titleCount(), 1);

  console.log('— Home: local counter (+/−), client data, client-only, on_mount —');
  click('.nav-home'); await wait(120);
  eq('page', page(), 'home');
  eq('title', headTitle(), 'Home · shop');
  fire(counterBtn(1)); fire(counterBtn(1)); fire(counterBtn(0));   // + + −
  eq('counter local state (1+1−1=1)', text('.counter .count'), 1);
  eq('greeting fetched on client nav', text('.greeting .msg'), 'Hello again, from the client 🔁');
  eq('greeting ready', text('.greeting .gstatus').trim(), '(ready)');
  eq('client-only data', text('.browser-box .bdata'), 'Loaded in the browser ✨');
  eq('on_mount ran', text('.browser-box .mounted'), 'mounted ✓');
  eq('localStorage visit counter (Browser facade)', text('.visits'), 'visits (localStorage): 1');
  eq('localStorage actually written', dom.window.localStorage.getItem('visits'), '1');

  console.log('— greeting refetch: real dynamic fetch (loading → ready) —');
  click('#refetch');
  eq('refetch shows loading', text('.greeting .gstatus').trim(), '(loading)');
  await wait(60);
  eq('refetch resolved', text('.greeting .msg'), 'Hello again, from the client 🔁');
  eq('refetch ready again', text('.greeting .gstatus').trim(), '(ready)');

  console.log('— catch-all: unknown in-app path → not found page —');
  goto('/shop/nope/here'); await wait(20);
  eq('page', page(), 'notfound');
  eq('catch-all param', text('.missing'), 'no route: /nope/here');  // path keeps its leading /
  eq('location', path(), '/shop/nope/here');

  console.log('— global store persisted across all that navigation (no reload) —');
  click('.nav-products'); await wait(20);
  eq('page', page(), 'products');
  eq('todo survived', todos(), 1);
  eq('stats', stats(), 'todos in store: 1');

  console.log('— popstate (browser back) —');
  dom.window.history.back(); await wait(30);   // back to /shop/nope/here
  eq('popstate navigates', page(), 'notfound');
  eq('popstate location', path(), '/shop/nope/here');

  console.log('— outer-reach (ext) link is NOT hijacked —');
  click('.nav-admin'); await wait(20);
  eq('ext left our route untouched', page(), 'notfound');

  console.log('— SSR payload empty for the product deep-link —');
  eq('empty data context', /window\.__ISO_DATA__=\{\}/.test(html), true);

  console.log('— SECOND APP (/admin) reuses the shared <Counter/> —');
  const adminHtml = readFileSync(new URL('./admin.html', import.meta.url), 'utf8');
  const adm = new JSDOM(adminHtml, {
    url: 'http://localhost/admin', runScripts: 'dangerously', pretendToBeVisual: true,
    beforeParse(w) { w.TextDecoder = globalThis.TextDecoder; w.TextEncoder = globalThis.TextEncoder; },
  });
  await wait(120);
  const ad = adm.window.document;
  eq('admin page rendered', ad.querySelector('.page')?.getAttribute('data-page'), 'admin-home');
  eq('admin title', ad.querySelector('head title')?.textContent, 'Admin · dashboard');
  eq('admin reuses shared Counter', !!ad.querySelector('.counter'), true);
  // the SAME component is live in the admin app — increment it
  ad.querySelector('.counter').querySelectorAll('button')[1]
    .dispatchEvent(new adm.window.MouseEvent('click', { bubbles: true }));
  eq('shared Counter works in admin', ad.querySelector('.counter .count').textContent, 1);
  eq('admin location is its own base', adm.window.location.pathname, '/admin');

  console.log(`\n${fail === 0 ? 'PASS' : 'FAIL'} — ${pass} passed, ${fail} failed`);
  process.exit(fail === 0 ? 0 : 1);
})();
