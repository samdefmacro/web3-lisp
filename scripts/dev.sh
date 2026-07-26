#!/usr/bin/env bash
# dev.sh — warm-image development helper (cl-agent-repl).
# Fill the PROJECT ADAPTER block; everything below it is generic.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUNTIME_DIR="$ROOT/.dev-runtime/swank-dev"
PIDFILE="$RUNTIME_DIR/swank.pid"
LOGFILE="$RUNTIME_DIR/swank.log"
METRICS_LOG="$RUNTIME_DIR/eval-metrics.log"

### BEGIN PROJECT ADAPTER ####################################################
# web3-lisp: host SBCL + Quicklisp; Coalton needs the 4GB heap.
PORT="${DEV_SWANK_PORT:-4008}"
HOST="127.0.0.1"

start_image() {
  exec env DEV_SWANK_PORT="$PORT" \
    sbcl --dynamic-space-size 4096 --noinform \
      --load scripts/dev-swank-server.lisp
}

# test MODULE runs the module runner, e.g. `dev.sh test rlp` ->
# (web3-tests/runner::run-rlp-tests). test-all asserts the failed count is 0
# (run-all-tests returns (values passed failed)).
test_one() {
  DEV_EVAL_TIMEOUT="${DEV_EVAL_TIMEOUT:-600}" \
    eval_form "(web3-tests/runner::run-$1-tests)"
}
test_all() {
  DEV_EVAL_TIMEOUT="${DEV_EVAL_TIMEOUT:-3600}" \
    eval_form '(multiple-value-bind (passed failed) (web3-tests/runner:run-all-tests)
  (declare (ignore passed))
  (unless (zerop failed) (error "~D web3 tests failed" failed)))'
}
### END PROJECT ADAPTER ######################################################

usage() {
  cat <<'USAGE'
Usage: scripts/dev.sh COMMAND [ARGS]

Commands:
  start | stop | status      Manage the warm Swank dev image
  eval FORM                  Evaluate FORM in the warm image (~0.1s)
  test NAME                  Run one test in the warm image
  test-all                   Run the full suite in the warm image
  docs-check                 Verify PAX documentation transcripts
  help                       Show this help

Eval exit codes: 0 ok, 1 lisp error, 2 connection error, 3 timed out
(interrupted, image survived), 4 hard hang (restart the image). Every eval
is logged to .dev-runtime/swank-dev/eval-metrics.log.
Env: DEV_EVAL_TIMEOUT (20), DEV_EVAL_MAX_OUTPUT (10000).
USAGE
}

is_pid_running() {
  [[ -f "$PIDFILE" ]] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null
}

port_listener() {
  lsof -nP -iTCP:"$PORT" -sTCP:LISTEN 2>/dev/null || true
}

wait_for_port() {
  local i
  for i in {1..120}; do
    if port_listener | grep -q ":${PORT}"; then
      return 0
    fi
    sleep 1
  done
  return 1
}

start_server() {
  mkdir -p "$RUNTIME_DIR"
  if is_pid_running; then
    echo "Dev image already running: pid $(cat "$PIDFILE")"
    return 0
  fi
  if port_listener | grep -q ":${PORT}"; then
    echo "Port ${PORT} already has a listener; reusing it."
    return 0
  fi
  : > "$LOGFILE"
  (
    cd "$ROOT"
    start_image >>"$LOGFILE" 2>&1
  ) &
  echo $! > "$PIDFILE"
  if wait_for_port; then
    echo "Started dev image on ${HOST}:${PORT} (pid $(cat "$PIDFILE"))"
    echo "Log: $LOGFILE"
  else
    echo "Timed out waiting for Swank on port ${PORT}" >&2
    echo "Log: $LOGFILE" >&2
    return 1
  fi
}

stop_server() {
  if is_pid_running; then
    local pid
    pid="$(cat "$PIDFILE")"
    kill "$pid" 2>/dev/null || true
    rm -f "$PIDFILE"
    echo "Stopped dev image pid $pid"
  else
    rm -f "$PIDFILE"
    echo "No helper-managed dev image is running."
  fi
}

status_server() {
  if is_pid_running; then
    echo "Helper-managed process: running pid $(cat "$PIDFILE")"
  else
    echo "Helper-managed process: not running"
  fi
  if port_listener | grep -q ":${PORT}"; then
    echo "Port ${PORT}: listening"
  else
    echo "Port ${PORT}: not listening"
  fi
}

log_metrics() { # $1 rc, $2 start_epoch, $3 form
  local snip
  snip=$(printf '%s' "$3" | tr '\n' ' ' | cut -c1-80)
  mkdir -p "$RUNTIME_DIR"
  printf '%s rc=%s dur_s=%s form=%s\n' \
    "$(date '+%Y-%m-%dT%H:%M:%S')" "$1" "$(( $(date +%s) - $2 ))" "$snip" \
    >> "$METRICS_LOG" 2>/dev/null || true
}

eval_form() {
  if [[ $# -eq 0 ]]; then
    echo "eval requires a Lisp FORM argument" >&2
    return 2
  fi
  local start rc=0
  start=$(date +%s)
  (cd "$ROOT" && DEV_SWANK_HOST="$HOST" DEV_SWANK_PORT="$PORT" \
    sbcl --script scripts/dev-swank-eval.lisp "$@") || rc=$?
  log_metrics "$rc" "$start" "$*"
  return $rc
}

docs_check() {
  (cd "$ROOT" && sbcl --non-interactive --load scripts/docs-check.lisp)
}

cmd="${1:-help}"
shift || true
case "$cmd" in
  start) start_server ;;
  stop) stop_server ;;
  status) status_server ;;
  eval) eval_form "$@" ;;
  test) test_one "$@" ;;
  test-all) test_all ;;
  docs-check) docs_check ;;
  help|-h|--help) usage ;;
  *) echo "Unknown command: $cmd" >&2; usage >&2; exit 2 ;;
esac
