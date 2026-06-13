# Agent Fastlane Dev Loop

Fennec's human `fennec dev` loop is already the source of truth for application
build state, last-good serving, reload kind, affected surface, diagnostics, and
unit test results. The agent fastlane adds one bridge: a post-tool hook can
block on that dev-loop verdict and inject it into the next model step.

Scope: this is for applications being developed with Fennec. It is not the
framework-internal loop for editing this repository; Fennec framework and
monorepo work uses direct focused `dune build` / `dune runtest` checks.

The normal path is intentionally small:

```sh
fennec dev --agent --attach --port 9123
```

`--attach` asks Fennec to install one guarded user-level hook for the active
coding harness. The hook is cwd/root guarded, so it does not run for unrelated
repositories. Internally the installed hook runs:

```sh
fennec agent hook --timeout 12
```

After that, the agent edits the application normally. The first edit that reaches the hook
injects a post-edit model block starting with
`Fennec dev feedback after this tool`. Agents should consume that block as the
dev verdict and should not tail logs, run `dune build`, run `dune runtest`, or
use explicit wait commands after every application edit.

## Hook Contract

`fennec dev --agent` keeps the normal terminal UI and additionally writes an
append-only JSONL event journal under the agent state directory. Each event has
a monotonic id. The hook command reads that journal and emits
`hookSpecificOutput.additionalContext` JSON for harnesses that support command
hooks.

The post-tool hook is stateful. Without any explicit marker from the harness it
uses a persisted cursor, skips the initial `ready` event, and returns the next
undelivered Fennec verdict. This matters because fast builds can settle before
the hook process starts; the hook must still catch that already-written event.
On success the cursor advances, so later hook calls do not replay stale
feedback.

Harnesses that pass explicit event ids can still do so; those are low-level
implementation details. They are not the public agent workflow.

Every hook wait is deadline-bounded. On timeout or dead devserver, the hook
still returns model-visible context instead of hanging the agent.

## Verdicts

Agent output is rendered from the same internal dev verdict each cycle. Green
build settles are emitted after the test lane has had its chance to run, so the
agent sees the complete consequence of the edit rather than an early partial
signal.

Verdicts include:

- served change: backend restart, reload, CSS hot-swap, or no served change
- affected surface: backend, component, route, app, styles, assets, tests,
  config when inferable
- unit test result when tests ran
- build timing fields
- focused compiler diagnostics with file/line/code frame on failures
- last-good serving state on failures

Test-only edits are first-class. A successful test-only settle reports
`build ok · no served change`, `affected: tests`, and the test tally.

## Recovery

Agents may run this cheap recovery command:

```sh
fennec agent status
```

`status` reports pid/root/events, the latest id and summary, the configured port
when known, and whether the recorded dev pid is alive. It is for inspection and
recovery, not the normal feedback loop.

## Dynamic Attach

`--attach` is the product surface for dynamic attach from a running chat. It
must not create repo-local instruction or hook files. It installs user-level
harness config with a Fennec marker and a root guard, and should dedupe previous
Fennec hooks for the same root.

Dynamic attach has two observable states:

- installed: Fennec wrote the harness hook config
- live: a subsequent edit injected `Fennec dev feedback after this tool`

Only the live state saves model calls. Installation output is setup status; the
post-edit feedback block is the useful fastlane signal.

## Claude Code

Claude Code command hooks can point `PostToolUse` or `PostToolBatch` at:

```sh
fennec agent hook --timeout 12
```

Use batch hooks when available, because they coalesce multifile edits into one
feedback verdict before the next model call. Use per-tool post hooks when batch
hooks are unavailable.

Do not teach Claude to run explicit waits after edits.

Observed local result on 2026-06-09: Claude Code 2.1.81 in a fresh
`claude -p --model haiku` session did pick up the `~/.claude/settings.json`
change made by `fennec dev --agent --attach` while the session was already
running. The next Edit tool fired the PostToolUse hook and injected:

```text
Fennec dev feedback after this tool:
examples/site/frontend/components/stats.mlx changed backend restart
affected: backend; component stats
tests 15 passed, 0 failed · 1 lib
```

## Codex

Codex command hooks support the same Fennec bridge. Fennec writes
`~/.codex/hooks.json` and the matching trusted hash in `~/.codex/config.toml`,
guarded to the project root.

When command hooks are available for file-editing tools, wire the post-tool hook
to:

```sh
fennec agent hook --timeout 12
```

Interactive hook-capable sessions then get true evented feedback for native
application edits. Agents should treat that injected feedback as the normal
compile/test signal, not add ad-hoc build/test probes after every application edit.

Compatibility note from local verification on 2026-06-09:

- Codex CLI 0.138.0 fresh interactive sessions fired the trusted PostToolUse
  hook after `apply_patch` and injected model-visible Fennec feedback.
- Codex CLI 0.138.0 did not hot-load a hook installed by
  `fennec dev --agent --attach` in the same already-running chat before the
  next `apply_patch`. The edit changed the file and the Fennec journal recorded
  the verdict, but no Codex `PostToolUse hook` event appeared in the transcript.
- `codex exec` is not a reliable proof path for this feature; use an
  interactive session transcript that shows `PostToolUse hook` and the Fennec
  feedback block.

## Native Future

The current native bridge is deliberately conservative: a JSONL journal plus a
blocking hook reader with a persisted cursor. A later version can replace the
file-following process with a socket API, but the product contract should stay
the same:

- start `fennec dev --agent --attach`
- prove the post-tool hook is live
- edit normally
- consume stable dev verdicts, not terminal output

## Mechanical Verification

Run focused direct Dune tests after changing the event journal or generated guide:

```sh
dune runtest cli/dev
```

Useful live checks:

- start `fennec dev --agent --attach` on an isolated port
- make one normal app edit and verify live attach injects `Fennec dev feedback after this tool`
- separately verify the hook catches feedback that settled before the hook process started
- make a unit-test-only edit and verify the verdict reports `affected: tests`
- restore the files and confirm the devserver stops cleanly
