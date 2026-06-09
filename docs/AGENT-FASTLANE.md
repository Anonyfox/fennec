# Agent Fastlane Dev Loop

Fennec's human `fennec dev` loop is already the source of truth for build state,
last-good serving, reload kind, and inline test results. For agents, the missing
piece is not another output format; it is a hook-friendly bridge that waits at
tool boundaries and injects the dev-loop consequence into the next model step.

The native path is:

```sh
fennec dev --agent --port 9123
fennec agent status
fennec agent mark
fennec agent wait --timeout 12
fennec agent wait --after 42 --timeout 12
fennec agent hook --timeout 12
```

`fennec dev --agent` keeps the normal human dev UI and additionally writes a
machine-readable event journal under the agent state directory. `fennec agent
wait` and `fennec agent hook` follow that journal with a blocking event stream,
so hooks do not parse terminal text and do not need an extra build/test command
after edits. Every wait has a hard timeout.

Events carry monotonic ids. `fennec agent wait --after ID` ignores old events and
returns only an event with `id > ID`; before tailing, it scans already-written
events, so it catches the race where the dev settle lands just before the hook
starts. `fennec agent mark` snapshots the latest id and stores it under a loose
session/tool key when the harness provides one on stdin; `fennec agent hook`
uses that marker when present, otherwise it snapshots the latest id at hook
start.

`fennec agent status` is the cheap recovery command. It reports pid/root/events,
the latest id and summary, the configured port when known, and whether the
recorded dev pid is still alive.

The older shell bridge remains useful while dogfooding:

```sh
tools/agent/fennec-dev start --port 9123
tools/agent/fennec-dev ensure --port 9123
tools/agent/fennec-dev wait --timeout 12
tools/agent/fennec-dev hook --timeout 12
tools/agent/fennec-dev stop
```

`start` runs `fennec dev` in the background and captures plain append-only output
under `${XDG_STATE_HOME:-~/.local/state}/fennec/agent/<repo-hash>/dev.log`.
Because stdout is not a TTY, the existing UI avoids cursor repainting and
becomes suitable for hooks. Set `FENNEC_AGENT_STATE_DIR` to override the state
location.

`ensure` is the agent entrypoint. It attaches to an existing bridge if one is
alive and starts a new one when the pid is missing or stale. `restart` is
available when the log shows a wedged process. `status` prints the log path and,
when the bridge uses `screen`, the command a human can use to attach.

`wait` starts from the current log offset and blocks until the next semantic
settle: ready, reload, css, resolved, build failed, watcher restart, or crash.
Internally it follows the dev log with a blocking event stream (`tail -f` plus
`select`), while the offset file catches events that settled just before the
hook started.

`hook` wraps `wait` and emits Claude Code-compatible JSON with
`hookSpecificOutput.additionalContext`, so the feedback lands next to the tool
result instead of requiring the model to remember to poll. If the dev loop is
not running when the hook fires, the hook starts it before waiting. If no settle
arrives before the timeout, the hook still returns model-visible context telling
the agent to inspect `tools/agent/fennec-dev status` before trusting the edit.

## Experimental Hook Wiring

For now this document is the only persisted discovery surface for the
experiment. Do not commit root agent instruction files or harness config files
for this mechanism until the contract is settled.

Temporary local harness configs can point at:

```sh
fennec agent hook --timeout 12
```

If the harness supports pre-tool hooks, wire pre-tool to:

```sh
fennec agent mark
```

and post-tool/batch to:

```sh
fennec agent hook --timeout 12
```

That gives the post hook an exact pre-edit event id. Without pre-tool support,
the post hook still works with a hook-start snapshot and a bounded timeout.

After testing, remove those local configs again. Hook trust is local harness
state, not Fennec source.

## Claude Code

Claude Code supports command hooks for `PostToolUse` and `PostToolBatch`, passes
hook input on stdin, and lets hooks inject `additionalContext` into the next
model request. A project-local `.claude/settings.local.json` can wire this
without committing personal automation:

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Edit|Write|MultiEdit",
        "hooks": [
          {
            "type": "command",
            "command": "fennec agent hook --timeout 12"
          }
        ]
      }
    ],
    "PostToolBatch": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "fennec agent hook --timeout 12"
          }
        ]
      }
    ]
  }
}
```

Use `PostToolBatch` when the harness can run parallel edits, because it coalesces
feedback once before the next model call. Use `PostToolUse` when tool batches are
not available or when immediate per-tool feedback is preferred. If Claude Code
pre-tool hooks are available, add `fennec agent mark` there for exact causality.

If a shell command edits files, run `fennec agent wait --timeout 12`
once after the shell batch. Do not attach the hook to every `Bash` call by
default; most shell commands are reads, and waiting after them adds avoidable
latency.

Start or attach to the dev loop before asking for code changes:

```sh
fennec dev --agent --port 9123
```

Stop it when the session ends:

```sh
Ctrl-C the foreground `fennec dev`, or stop the wrapper if using `tools/agent/fennec-dev`.
```

## Codex

Codex hook coverage has changed across 2026 releases. Local smoke tests against
Codex CLI 0.137.0 verified that interactive Codex TUI `PostToolUse` hooks fire
for both `Bash` and native `apply_patch`, and that
`hookSpecificOutput.additionalContext` is visible to the next model step.
`codex exec` did not fire the same `PostToolUse` hook in the smoke test, so use
the interactive/TUI harness for true evented feedback.

For a temporary local Codex smoke, the relevant inline config shape is:

```toml
[[hooks.PostToolUse]]
matcher = "^(apply_patch|Edit|Write)$"

[[hooks.PostToolUse.hooks]]
type = "command"
command = "fennec agent hook --timeout 12"
timeout = 30
statusMessage = "Waiting for Fennec dev feedback"
```

Start Codex with hooks enabled and review/trust the hook. In automation or
throwaway smoke tests, `--dangerously-bypass-hook-trust` can bypass the trust
prompt for that invocation. If Codex exposes reliable pre-tool hooks in the
active mode, point them at `fennec agent mark`; interactive Codex TUI 0.137.0
was verified for post-tool `apply_patch` hooks, while `codex exec` was not.

If a Codex build or mode does not fire hooks for file edits, the fallback
instruction is: after a native file-editing tool call, run exactly one wait
command before reasoning about the result:

```sh
fennec agent wait --timeout 12
```

That is still worse than true hook injection, but it keeps the check cheap,
semantic, and coalesced. The model sees one settle summary instead of running
ad-hoc build/test/curl probes.

## Native Future

The current native path is intentionally conservative: an append-only JSONL
journal with monotonic event ids plus a blocking `fennec agent wait`/`hook`
reader. It already emits an `idle` event for green no-op settles, so hooks do
not hang just because the edit produced no served change. A later richer version
can move from file-following to a socket API:

- `--agent-socket PATH` for `status`, `wait_idle`, and `wait_next`.
- Monotonic event IDs and causal waits such as `wait_idle_since <timestamp>`.
- Stable event objects for ready, build_ok, build_failed, css, reload, tests,
  crash, and watcher_restart.
- Harness hooks that call `wait_idle_since` after writes and inject the result.

That removes even the journal-following process. The important contract already
exists: hooks consume stable dev events, not terminal output, and the human TUI
does not change.

## Mechanical Verification

Run the inline tests when changing the native event journal:

```sh
dune runtest cli/dev
```

Run the shell bridge verifier when changing the wrapper:

```sh
tools/agent/verify-fennec-dev-fastlane
```

It starts an isolated dev loop, verifies `ensure` can recover a stopped loop
with the previous dev args, performs a rapid multifile edit batch, calls one
hook, restores the files, then tests the settled-before-hook race by observing a
dev event land before invoking the hook. It also asserts that `stop` leaves no
`fennec dev` or `dune --watch` process behind.
