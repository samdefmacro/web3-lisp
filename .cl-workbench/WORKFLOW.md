<!-- cl-workbench-template: WORKFLOW.md.template template=a41889577fc3 rendered=ce5f29589049 version=0.13.0-dev -->
# The development loop for web3-lisp (cl-workbench managed, containerized warm image)

Reference this file from `CLAUDE.md` / `AGENTS.md` (or paste it in). Generated
by `cl-workbench init`; regenerate a stale copy with `cl-workbench init --update`.

This is a Common Lisp Workbench managed project (`.cl-workbench/project.toml`,
execution profile container-required). Run `cl-workbench doctor --strict` from
the project root once per substantive session before the first build, eval, or
test. SBCL never runs on the host: every entry point below runs inside the
pinned project container (`web3-lisp-sbcl:2.5.2-1`), and the warm image's Swank port stays
private to the container (evals are a `docker exec` of the Workbench client; no
host port is published). Container, session, and FASL-volume identities are
checkout-specific, so parallel checkouts never collide — and two sessions on
the SAME checkout share one warm image: do not stop it unless you started it.

```
scripts/dev.sh start            # warm image in the project container
scripts/dev.sh eval '(+ 1 2)'   # ~0.3 s per eval
scripts/dev.sh test             # the whole suite in the warm image
scripts/dev.sh test SELECTOR    # one selector, when the project defines one
scripts/docker-test.sh          # the cold battery in a fresh container: verification of record
scripts/dev.sh stop
```

`cl-workbench repl eval FORM`, `test [SELECTOR]`, and
`validation run warm-unit SELECTOR|cold-unit` route to these same entry
points through `.cl-workbench/adapter`.

Discipline: ground before writing (`dev.sh eval '(describe ...)'`, apropos);
develop in small evals; after editing, reload
(`dev.sh eval '(asdf:load-system "...")'`) and re-run the touched tests;
`defstruct` layout changes and **any `defconstant` value change require an
image restart** (`dev.sh stop` + `dev.sh start`): a reload can leave the
symbol reporting the new value while compiled callers keep the old one, and
a warm suite then passes against a value no longer in the source. The cold
battery compiles fresh and stays the verification of record. A test form that
selects zero tests fails (rc 1); it can never pass silently.

Eval exit codes: 0 ok / 1 lisp error (+backtrace, condition type, restarts) /
2 connection (image down, NOT your code — `dev.sh start`) / 3 timed
out+interrupted (image survives; raise `DEV_EVAL_TIMEOUT` for long forms) /
4 hard hang. A long foreign call cannot be interrupted mid-call and may show
rc=4 even though the image recovers when it returns: re-probe with a trivial
eval before restarting.

Workbench queues payload-free outcome records under `.cl-workbench/state/`
(git-ignored); the Claude Code paren hook logs to
`.cl-workbench/state/paren-hook.log`.

## Recording lessons

When you write a `⚠️` in a commit message or a doc because something here
was a trap, record it in the same turn:

    ~/.claude/skills/develop-common-lisp/scripts/lesson add SLUG "what happened and what to do" "web3-lisp#<PR> or docs/<file>"

(blockchain-specific traps: add `--skill develop-blockchain-with-common-lisp`).
Exit 3 means the slug already exists — the trap has recurred and prose did not
stop it: add a check that fires mechanically (dev.sh, the adapter, or a test
with a positive control) instead of a second line. No branch, review, or
approval is needed for the log line itself.
