# The Economics of Fennec

**Every AI coding tool demos beautifully on a weekend project and falls apart on a real one. Fennec is built for the part that comes after the demo.**

This is the money argument: why building with Fennec gets *cheaper, faster, and safer per feature* than the mainstream stack — and why that lead doesn't shrink as your project grows, it *widens*. The claims below are backed by public, linked numbers from the outside world. The conclusion is simple:

> In every other stack, the cost and risk of an AI-built feature **grows with the size of your codebase** — and past a certain size it explodes. In Fennec it stays roughly **flat**. Same model, same prompts, completely different curve.

---

## TL;DR

| | Mainstream stack | Fennec |
|---|---|---|
| **Edit → verified result** | seconds to minutes, across separate compile / test / lint calls | **~200ms, delivered inside the edit itself** |
| **LLM cost per feature** | baseline | **~3–5× lower** |
| **Cost as the project grows** | rises **exponentially** (tangle) | stays **~flat** (bounded by design) |
| **Runtime speed** | varies | **faster than Go, within ~1.5× of Rust** |
| **The stack** | 4–6 libraries you glue together | **one bundle: frontend + backend + database** |
| **Realtime** | a bolt-on you wire up | **on by default** |

You don't trade dev speed for runtime speed, or simplicity for power. You get all of it in one thing — and the whole thing is tuned for how AI coding *actually works day to day*.

---

## First, the shape of the problem (in plain terms)

When people compare tools they ask "which is faster?" The better question is **"how does the cost grow as the project grows?"** Three shapes, and the difference between them is everything:

- **Constant** — each change costs the same whether your app is 10 files or 10,000. The dream.
- **Linear** — twice the project, twice the cost per change. Survivable.
- **Exponential** — each new tangled piece doesn't *add* to the cost, it *multiplies* it. This is the one that kills projects.

Here's the part nobody tells you up front: **the mainstream AI-coding stack is on the exponential curve, and that's why your amazing AI velocity quietly turns to mud.** Let's make that concrete enough that anyone can see it.

### Why a great AI tool turns useless on a big codebase

An AI makes a change correctly only if *everything that change touches* is also correct — at the same time. Call the number of things-that-must-line-up **N**, and the AI's per-thing reliability **p**.

The whole change succeeds about **p^N** of the time. Watch what that does:

| Things that must line up (N) | At 95% reliability each | 
|---|---|
| 5 (a small, clean change) | 0.95⁵ ≈ **77%** |
| 15 (a medium feature) | 0.95¹⁵ ≈ **46%** |
| 40 (one change in a tangled app) | 0.95⁴⁰ ≈ **13%** |

The AI didn't get dumber between those rows. **The project got more tangled, so more things had to be right at once, and the misses multiply.** That multiplication *is* the swamp — the moment vibecoding stops working and you need a senior human to slowly dig the complexity back out by hand. It's not a feeling; it's measured. Cursor users in real codebases saw effective productivity **drop 19%**, code review time **rise 40%**, and pull-request complexity **balloon 154%** ([study](https://arxiv.org/pdf/2511.04427)). The industry's own guides admit AI agents are "great for new projects or small changes, but in large established codebases they can make developers less productive" ([Augment](https://www.augmentcode.com/tools/ai-coding-assistants-for-large-codebases-a-complete-guide)).

### "Won't smarter models just fix this?"

Partly, and only for a while. Models *are* getting better — the length of task an AI can finish reliably has been [doubling roughly every 7 months](https://metr.org/blog/2025-03-19-measuring-ai-ability-to-complete-long-tasks/). But that raises **p**, the reliability per thing — and you cannot out-run an exponent by nudging its base. Push reliability from 95% to 98% and our 40-thing change goes from 13% to 45%. Better! Then the project grows to 80 tangled things and you're back to 20%. **Smarter models buy you time; they do not change the shape of the curve.** Worse, the usual escape hatch — "just let the AI search the codebase" (RAG) — provably can't carry the load here: retrieval is good at finding *a few relevant snippets* and bad at *"is everything that touches this still correct?"* questions, which is exactly the question that matters at scale ([research](https://arxiv.org/pdf/2510.20548)). And doubling a task's length [quadruples the failure rate](https://arxiv.org/html/2509.09677v3), not doubles it, because the errors compound.

**The only thing that changes the shape is making each change need fewer things-at-once.** `0.95⁵ ≈ 77%` no matter how big your app gets — *if every change stays a small-N change.* That is the entire game, and it's the one thing the rest of the industry isn't playing. Fennec is.

---

## Act 1 — The first hour: quick wins from token one

You feel Fennec before any of the scaling theory matters, on the very first feature.

**One bundle, not a parts bin.** A typical modern app is assembled from a backend framework, an API/RPC layer, a database client, a job queue, and a realtime/websocket library — four to six tools that don't know each other exist, each with its own glue, config, and failure modes. Fennec is **one** thing: frontend, backend, and database, end to end, with realtime on by default. There's no integration tax because there's nothing to integrate. Fewer moving parts means fewer files the AI has to pull into its head to do anything at all.

**Terse by design, so the AI thinks in features, not boilerplate.** Fennec leans on a rare OCaml capability — rewriting its own syntax surface — to delete ceremony. One concern lives in one short file; tests sit inline next to the code as one-liners; the boilerplate that bloats other stacks is simply gone. The result: a feature is **a fraction of the tokens** to express *and* to hold in context. That matters for cost (you process fewer tokens) and for quality (more on that below).

**The feedback loop runs inside the edit.** This is the quiet killer feature. In a normal AI workflow, "make a change and confirm it works" is *several* steps: edit, then a separate call to compile, then another to run tests (and the AI burns reasoning just deciding *which* tests), maybe another to lint. Every one of those round trips re-sends the whole growing conversation to the model. Fennec's dev server **hijacks the edit itself**: every atomic change blocks for ~200ms and hands the result — compiles? tests that touched this code still pass? — straight back as part of the same action.

How much does that save? Public agent traces run anywhere from [~13 to ~33 steps per task](https://github.com/SWE-agent/mini-swe-agent/), and a large share of those steps are verification. Folding verification into the edit removes them. Here's the math on a single feature, using current [Anthropic pricing](https://platform.claude.com/docs/en/about-claude/pricing) (Sonnet at $3/$15 per million tokens in/out, with prompt caching on):

| | Mainstream (edit + separate verify calls) | Fennec (edit *is* the verify) |
|---|---|---|
| Round trips to the model | ~35 | ~13 |
| Tokens carried per step | ~80K (more files, more boilerplate) | ~40K (terse, colocated) |
| Total input tokens processed | ~2.8M | ~0.52M |
| **Cost / feature** | **~$1.89** | **~$0.55** |

That's **~3.4× cheaper** with caching turned on — and caching is the thing that helps the *incumbent* most. Turn it off (a slow, human-in-the-loop session where the cache expires) and the gap widens to **~5×**. On raw work done, it's **~5× fewer tokens** pushed through the model. Wall-clock tracks the same shape: **~3–5× faster** end to end, with the verify step itself going from tens of seconds to ~200ms.

For comparison on raw cycle speed alone: a Rust project's compiler can take [30–120 seconds](https://tech-insider.org/rust-vs-go-2026-2/) on a medium change; a TypeScript typecheck runs [seconds to tens of seconds](https://devblogs.microsoft.com/typescript/typescript-native-port/); Go is fast at ~1s but *you still spend a whole model round trip to invoke it.* Fennec's ~200ms happens **without a round trip at all.** That's why speed alone (Go has it) isn't the win — putting the result *inside the edit* is.

**Bottom line for Act 1:** before any cleverness, a Fennec feature is roughly **3–5× cheaper and 3–5× faster to build**, every feature, from the first one.

---

## Act 2 — The scaling cliff, and how Fennec walks past it

Now apply the curve from the top of this doc. As a normal codebase grows, **N** — the number of things every change must get right at once — grows with it, because tangle accumulates: a callback wired by a string here, an untyped event there, server code that leaks into the client by accident, a field used in thirty places. `p^N` collapses, and you hit the swamp.

**Fennec is engineered to keep N from growing.** Three mechanisms, each attacking a different source of tangle:

1. **Everything about one concern lives in one file.** When the AI needs to change "tickets," the entire surface of tickets is in front of it — not scattered across a router, a service, a schema, and a test directory it has to find by search. Retrieval becomes *complete by construction* instead of a lucky top-k guess. The single biggest cause of AI mistakes at scale — "it didn't see the other place that mattered" — is designed out.

2. **The compiler holds the global rules, so the AI doesn't have to.** In a normal stack, "did I break the thirty things that use this?" lives in the *AI's head* — it has to remember them and reason each one correctly, and every one of those is a chance to slip. In Fennec those rules are checked by the compiler. The AI makes the change; the compiler instantly lists every place that broke, by file and line. **A reasoning problem that would collapse exponentially becomes a checklist the AI works down, each item verified in 200ms.** That is the difference between a task that falls off the cliff and one that doesn't.

3. **The worst kinds of tangle simply won't compile.** Delete something a reaction depends on and it's a compile error, not a silent dead hook. Try to reference server-only code (a secret, a database call) from the browser and it won't build. The invisible coupling that quietly drives `N` toward catastrophe in other stacks is made *unrepresentable* — it can't accumulate in the first place.

Put together: **the number of things the AI must juggle per change stays roughly constant as your project grows from a prototype to an enterprise system.** You stay on the 77%-per-change row while everyone else slides down to 13%. That's the curve bending from exponential to flat — not by a smarter model, but by a substrate that refuses to let complexity hide.

And there's a quality dividend on top. Every frontier model — Claude, GPT, Gemini alike — [loses accuracy as the relevant information spreads out across a long context](https://www.morphllm.com/context-rot), by more than 30% in the worst zones. Because Fennec keeps each feature's working set small and colocated, the model operates at a *higher point on its own accuracy curve* the whole time. Cheaper, faster — and more often *right the first time*, which is the most expensive thing of all to get wrong.

---

## Act 3 — And it's production-grade, not just pleasant to build

Dev-time savings would be hollow if you paid for them at runtime. You don't.

- **Speed.** Fennec compiles to native code. In our benchmarks it runs **faster than Go** and lands **within ~1.5× of Rust** — top-tier performance, not a scripting-language compromise. (The repo ships a perf-regression guard so this stays true.)
- **Scales both ways.** Vertical and horizontal. Background jobs run **at-most-once across replicas** by design, scheduled work coordinates itself, and realtime publications fan out cleanly — the operational concerns that usually require a second system are built in.
- **Truly full-stack.** One language and one mental model spans the browser UI, the server logic, and the database, with realtime updates flowing end to end by default. The same terseness and the same compiler guarantees that make the AI fast also make the *whole* stack coherent — a change to your data model is checked all the way from the database to the pixel.

So the bundle is: **state-of-the-art runtime speed + real operational scaling + a complete frontend/backend/database story with realtime by default** — and *all* of it co-designed around the realities of building with AI.

---

## The whole pitch in one breath

Most tools force a choice: fast to build *or* fast to run; simple *or* powerful; a clean prototype *or* an enterprise system. Fennec's bet is that those tradeoffs were never fundamental — they were just accidents of stacks that grew up before AI was the one writing the code.

- **At hour one:** ~3–5× cheaper and faster per feature, in one bundle instead of six.
- **As you grow:** the cost curve stays flat where everyone else's goes exponential, because the design keeps *N* small no matter the project size.
- **In production:** faster than Go, near Rust, scaling vertically and horizontally, full-stack and realtime by default.

The industry is spending its money making the AI smarter — raising **p**. Fennec spent its design making each change need less to be right at once — shrinking **N**. The math says **N** is the term that wins at scale. That's the entire thesis, and it's why Fennec gets *better* exactly where everything else gets worse.

---

### The receipts

Model pricing — [Anthropic API pricing](https://platform.claude.com/docs/en/about-claude/pricing) · Agent step counts — [SWE-agent traces](https://github.com/SWE-agent/mini-swe-agent/) · Long-task horizon & model progress — [METR](https://metr.org/blog/2025-03-19-measuring-ai-ability-to-complete-long-tasks/) · Compounding failure at scale — [Illusion of Diminishing Returns](https://arxiv.org/html/2509.09677v3) · RAG can't do global reasoning — [GlobalRAG](https://arxiv.org/pdf/2510.20548) · Long-context accuracy loss — [context rot](https://www.morphllm.com/context-rot) · AI in large codebases — [Cursor productivity study](https://arxiv.org/pdf/2511.04427), [Augment guide](https://www.augmentcode.com/tools/ai-coding-assistants-for-large-codebases-a-complete-guide) · Compile speeds — [TypeScript](https://devblogs.microsoft.com/typescript/typescript-native-port/), [Rust vs Go](https://tech-insider.org/rust-vs-go-2026-2/).
