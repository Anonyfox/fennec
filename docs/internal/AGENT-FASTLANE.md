# Agent Fastlane Dev Loop

Fennec's human `fennec dev` loop is already the source of truth for build
state, last-good serving, reload kind, affected surface, diagnostics, and unit
test results. The agent fastlane adds one bridge: a post-tool hook can block on
that dev-loop verdict and inject it into the next model step.

The normal path is intentionally small:

```sh
fennec dev --agent --port 9123
```

Then configure the coding harness once so its post-tool or post-edit hook runs:

```sh
fennec agent hook --timeout 12
```

After that, the agent edits normally. It should not run manual `dune build`,
`dune runtest`, or explicit wait commands after every edit. The hook verdict is
the feedback loop.

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

## Claude Code

Claude Code command hooks can point `PostToolUse` or `PostToolBatch` at:

```sh
fennec agent hook --timeout 12
```

Use batch hooks when available, because they coalesce multifile edits into one
feedback verdict before the next model call. Use per-tool post hooks when batch
hooks are unavailable.

Do not teach Claude to run explicit waits after edits. If the hook cannot be
installed in the active harness, the correct report is that Fennec fastlane is
not attached.

## Codex

Codex hook support varies by harness release and mode. When command hooks are
available for file-editing tools, wire the post-tool hook to:

```sh
fennec agent hook --timeout 12
```

Interactive hook-capable harnesses get true evented feedback. Modes that do not
fire hooks for native file edits cannot provide the full fastlane contract; do
not paper over that by making agents run ad-hoc build/test probes after every
edit.

## Native Future

The current native bridge is deliberately conservative: a JSONL journal plus a
blocking hook reader with a persisted cursor. A later version can replace the
file-following process with a socket API, but the product contract should stay
the same:

- start `fennec dev --agent`
- configure one post-tool hook
- edit normally
- consume stable dev verdicts, not terminal output

## Mechanical Verification

Run focused tests after changing the event journal or generated guide:

```sh
dune runtest cli/dev
```

Useful live checks:

- start `fennec dev --agent` on an isolated port
- make an app edit and run one post-tool `fennec agent hook --timeout 12`
- verify the hook catches feedback that settled before the hook process started
- make a unit-test-only edit and verify the verdict reports `affected: tests`
- restore the files and confirm the devserver stops cleanly
