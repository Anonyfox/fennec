# Agent Fastlane Dev Loop

Fennec's human `fennec dev` loop is already the source of truth for build state,
last-good serving, reload kind, and inline test results. For agents, the missing
piece is not another output format; it is a hook-friendly bridge that waits at
tool boundaries and injects the dev-loop consequence into the next model step.

This repo includes an additive bridge:

```sh
tools/agent/fennec-dev start --port 9123
tools/agent/fennec-dev wait --timeout 12
tools/agent/fennec-dev hook --timeout 12
tools/agent/fennec-dev stop
```

`start` runs `fennec dev` in the background and captures plain append-only output
under `${XDG_STATE_HOME:-~/.local/state}/fennec/agent/<repo-hash>/dev.log`.
Because stdout is not a TTY, the existing UI avoids cursor repainting and
becomes suitable for hooks. Set `FENNEC_AGENT_STATE_DIR` to override the state
location.

`wait` starts from the current log offset and blocks until the next semantic
settle: ready, reload, css, resolved, build failed, watcher restart, or crash.
Internally it follows the dev log with a blocking event stream (`tail -f` plus
`select`), while the offset file catches events that settled just before the
hook started.

`hook` wraps `wait` and emits Claude Code-compatible JSON with
`hookSpecificOutput.additionalContext`, so the feedback lands next to the tool
result instead of requiring the model to remember to poll.

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
            "command": "${CLAUDE_PROJECT_DIR}/tools/agent/fennec-dev hook --timeout 12"
          }
        ]
      }
    ],
    "PostToolBatch": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "${CLAUDE_PROJECT_DIR}/tools/agent/fennec-dev hook --timeout 12"
          }
        ]
      }
    ]
  }
}
```

Use `PostToolBatch` when the harness can run parallel edits, because it coalesces
feedback once before the next model call. Use `PostToolUse` when tool batches are
not available or when immediate per-tool feedback is preferred.

If a shell command edits files, run `tools/agent/fennec-dev wait --timeout 12`
once after the shell batch. Do not attach the hook to every `Bash` call by
default; most shell commands are reads, and waiting after them adds avoidable
latency.

Start the dev loop before asking for code changes:

```sh
tools/agent/fennec-dev start --port 9123
```

Stop it when the session ends:

```sh
tools/agent/fennec-dev stop
```

## Codex

Codex hook coverage has changed across 2026 releases. A smoke test against
Codex CLI 0.120.0 confirmed that `PostToolUse` for `Bash` can inject
`hookSpecificOutput.additionalContext` into the next model step. The same smoke
did not fire for the native `apply_patch` file-change tool in that build, so do
not assume Codex can remove post-edit checks for native file edits unless the
active CLI version has been verified.

When the active Codex CLI fires `PostToolUse` for the file-writing tools in use,
point it at the same hook command:

```sh
tools/agent/fennec-dev hook --timeout 12
```

If a Codex build does not fire hooks for `apply_patch`, the fallback skill
instruction is: after a native file-editing tool call, run exactly one wait
command before reasoning about the result:

```sh
tools/agent/fennec-dev wait --timeout 12
```

That is still worse than true hook injection, but it keeps the check cheap,
semantic, and coalesced. The model sees one settle summary instead of running
ad-hoc build/test/curl probes.

## Skill

The repo-local skill lives at:

```text
docs/agent-fastlane/SKILL.md
```

Install it into Codex with your normal skill workflow if you want automatic
triggering. It deliberately tells agents to prefer the bridge over one-off
`dune build`, `dune runtest`, or terminal polling while the bridge is active.

## Native Future

The bridge is intentionally conservative. The first-class version should move
the same semantics into `fennec dev` itself:

- `--agent-socket PATH` for `status`, `wait_idle`, and `wait_next`.
- Monotonic event IDs and causal waits such as `wait_idle_since <timestamp>`.
- Stable event objects for ready, build_ok, build_failed, css, reload, tests,
  crash, and watcher_restart.
- Harness hooks that call `wait_idle_since` after writes and inject the result.

That removes log parsing and closes the race where a build settles before a hook
starts. The bridge gives Claude/Codex a working fastlane today without changing
the human TUI.

## Mechanical Verification

Run the verifier when changing the bridge:

```sh
tools/agent/verify-fennec-dev-fastlane
```

It starts an isolated dev loop, performs a rapid multifile edit batch, calls one
hook, restores the files, then tests the settled-before-hook race by observing a
dev event land before invoking the hook. It also asserts that `stop` leaves no
`fennec dev` or `dune --watch` process behind.
