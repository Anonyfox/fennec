import { JSDOM } from 'jsdom';
import { readFileSync } from 'node:fs';

const html = readFileSync(new URL('./index.html', import.meta.url), 'utf8');
const dom = new JSDOM(html, {
  runScripts: 'dangerously',
  beforeParse(w) { w.TextDecoder = globalThis.TextDecoder; w.TextEncoder = globalThis.TextEncoder; },
});
const { document } = dom.window;

let pass = 0, fail = 0;
const eq = (label, got, want) => {
  if (String(got) === String(want)) { pass++; console.log(`  ✓ ${label} = ${got}`); }
  else { fail++; console.log(`  ✗ ${label}: got ${JSON.stringify(got)}, want ${JSON.stringify(want)}`); }
};
const counts = () => [...document.querySelectorAll('.counter .count')].map(e => e.textContent);
const stats  = () => document.querySelector('.stats').textContent;
const title  = () => document.querySelector('h1').textContent;
const lis     = () => document.querySelectorAll('.todos li').length;
const click  = el => el.dispatchEvent(new dom.window.Event('click'));
const counterBtns = i => document.querySelectorAll('.counter')[i].querySelectorAll('button'); // [−, +]
// head helpers
const titleCount = () => document.querySelectorAll('head title').length;
const headTitle  = () => document.querySelector('head title').textContent;
const descCount  = () => document.querySelectorAll('head meta[name="description"]').length;
const desc       = () => document.querySelector('head meta[name="description"]').getAttribute('content');
const jsonld     = () => document.querySelectorAll('head script[type="application/ld+json"]').length;
const og         = () => document.querySelector('head meta[property="og:title"]');

setTimeout(() => {
  console.log('— after hydration —');
  eq('counter[0]', counts()[0], 0);
  eq('counter[1]', counts()[1], 0);
  eq('stats', stats(), 'todos in store: 0');

  console.log('— HEAD: rehydration-safe (no duplication) + override resolved —');
  eq('exactly one <title> (no dup on hydrate)', titleCount(), 1);
  eq('title = Stats override (deepest wins)', headTitle(), '0 todos · iso');
  eq('exactly one description', descCount(), 1);
  eq('description = Stats override', desc(), 'Live todo dashboard');
  eq('App og:title survives (not overridden)', og() ? og().getAttribute('content') : null, 'iso PoC');
  eq('App json-ld survives', jsonld(), 1);

  console.log('— LOCAL state: +counter[0] twice, +counter[1] once —');
  click(counterBtns(0)[1]); click(counterBtns(0)[1]);
  click(counterBtns(1)[1]);
  eq('counter[0] independent', counts()[0], 2);
  eq('counter[1] independent', counts()[1], 1);
  eq('stats untouched (fine-grained)', stats(), 'todos in store: 0');

  console.log('— GLOBAL state: add todo (App re-renders) —');
  click(document.querySelector('#add'));
  eq('store count via stats', stats(), 'todos in store: 1');
  eq('title reflects store', title(), 'iso — 1 todos');
  eq('li rendered', lis(), 1);
  eq('counter[0] PERSISTED across parent re-render', counts()[0], 2);
  eq('counter[1] PERSISTED across parent re-render', counts()[1], 1);
  console.log('  · head reacts in the browser (dynamic title):');
  eq('title updated reactively', headTitle(), '1 todos · iso');
  eq('still exactly one <title>', titleCount(), 1);
  eq('description still single + unchanged', descCount() + ':' + desc(), '1:Live todo dashboard');

  console.log('— add 2 more, then remove first (keyed) —');
  click(document.querySelector('#add'));
  click(document.querySelector('#add'));
  eq('three todos', lis(), 3);
  eq('first item text', document.querySelector('.todos li').firstChild.textContent.trim(), 'item 1');
  click(document.querySelector('.todos li .rm')); // remove item 1
  eq('two todos after remove', lis(), 2);
  eq('now first is item 2', document.querySelector('.todos li').firstChild.textContent.trim(), 'item 2');
  eq('stats after remove', stats(), 'todos in store: 2');
  eq('counters still local-intact', counts().join(','), '2,1');
  eq('title tracks removal', headTitle(), '2 todos · iso');

  console.log(`\n${fail === 0 ? 'PASS' : 'FAIL'} — ${pass} passed, ${fail} failed`);
  process.exit(fail === 0 ? 0 : 1);
}, 300);
