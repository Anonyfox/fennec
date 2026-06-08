---
name: fennec-dev-fastlane
description: Use when working in a Fennec repo with `fennec dev` running or when asked to use the agentic/realtime dev loop. Guides Codex to use the Fennec agent bridge for post-edit build, reload, and test feedback instead of manually polling terminals or running redundant one-off build/test commands.
---

# Fennec Dev Fastlane

Use the bridge when it exists:

```sh
tools/agent/fennec-dev status
```

If it is not running and the task involves iterative coding, start it once:

```sh
tools/agent/fennec-dev start --port 9123
```

After editing files, prefer one semantic wait over ad-hoc checks:

```sh
tools/agent/fennec-dev wait --timeout 12
```

Interpret the result:

- `build failed`: fix the reported diagnostic first. The last good server may still be serving.
- `reload`: backend or JS changed; use the served URL if browser/HTTP verification matters.
- `css`: style-only hot swap; avoid restarting or rebuilding manually.
- `tests ... failed`: fix tests before declaring the change done.
- timeout/no output: fall back to a targeted command such as `dune build` or inspect `tools/agent/fennec-dev status`.

Do not repeatedly poll the dev terminal. The bridge coalesces the next dev-loop settle after the edit. Use one wait after a batch of related edits, not after every tiny internal read.

For Claude Code, configure `tools/agent/fennec-dev hook --timeout 12` as a `PostToolBatch` or `PostToolUse` hook so the feedback is injected automatically. For Codex, use hooks when the active CLI version fires them for file-edit tools; Codex CLI 0.120.0 was verified to inject `PostToolUse` context for `Bash`, but not for native `apply_patch`. If file-edit hooks are not verified, run the single wait command explicitly after edits.

When changing the bridge itself, verify the contract mechanically:

```sh
tools/agent/verify-fennec-dev-fastlane
```

The verifier covers a rapid multifile edit batch, the settled-before-hook race, file restore, and watcher cleanup.
