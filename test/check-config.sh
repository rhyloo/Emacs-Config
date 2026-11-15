#!/usr/bin/env bash
# Smoke test: start this configuration in a hidden terminal (tmux), run
# test/check-config.el and print its report. Exits non-zero if a check fails.
# Usage: test/check-config.sh            (tests the directory this script lives in)
#        EMACS=/path/to/emacs test/check-config.sh   (tests another Emacs binary)
set -euo pipefail
config_dir="$(cd "$(dirname "$0")/.." && pwd)"
work="$(mktemp -d)"
trap 'tmux kill-session -t check-config 2>/dev/null || true; rm -rf "$work"' EXIT
printf '# Prueba\n\n```mermaid\ngraph TD\n  A-->B\n```\n' > "$work/test.md"
printf '* Hola\n' > "$work/test.org"
tmux new-session -d -s check-config -x 200 -y 50 \
  "FUNC_DIR=$work PROBE_OUT=$work/report.txt ${EMACS:-emacs} -nw --init-directory $config_dir -l $config_dir/test/check-config.el"
for _ in $(seq 1 300); do [ -f "$work/report.txt" ] && break; sleep 1; done
[ -f "$work/report.txt" ] || { echo "FAIL: no report after 300 s"; exit 1; }
cat "$work/report.txt"; echo
grep -q "^ERROR" "$work/report.txt" && { echo "FAIL"; exit 1; }
grep -q "mode=org-mode version=.*elpaca" "$work/report.txt" || { echo "FAIL: org"; exit 1; }
grep -q "^md: mode=markdown-mode preview-bound=markdown-preview-mode" "$work/report.txt" || { echo "FAIL: markdown"; exit 1; }
grep -q "mermaid-js=t onupdate=t" "$work/report.txt" || { echo "FAIL: mermaid preview"; exit 1; }
grep -q "^reading: olivetti=t imenu-key=counsel-imenu" "$work/report.txt" || { echo "FAIL: reading aids"; exit 1; }
grep -q "^files: dirvish=t side-key=dirvish-side" "$work/report.txt" || { echo "FAIL: dirvish"; exit 1; }
grep -q "^git: diff-hl=t" "$work/report.txt" || { echo "FAIL: diff-hl"; exit 1; }
grep -q "^dirvish: vc-state=nil" "$work/report.txt" || { echo "FAIL: dirvish vc-state is back (slow)"; exit 1; }
grep -q "^preview dead-page: dropped=t errors=nil" "$work/report.txt" || { echo "FAIL: preview with a closed page"; exit 1; }
grep -q "^inline-diagram: svg=t sized=t spaces=t hide-markup=t" "$work/report.txt" || { echo "FAIL: in-buffer diagrams"; exit 1; }
grep -q "^reading-mode: read-only=t line-numbers=nil" "$work/report.txt" || { echo "FAIL: Markdown opens for reading"; exit 1; }
grep -q "^table: boxed=t fits=t drawn-while-reading=t revealed-inside=t drawn-outside=t" "$work/report.txt" || { echo "FAIL: drawn tables"; exit 1; }
grep -q "^terminal: ghostel-live=t" "$work/report.txt" || { echo "FAIL: ghostel"; exit 1; }
grep -q "^claude: key=claude-code-ide-menu cli=t backend=ghostel" "$work/report.txt" || { echo "FAIL: claude"; exit 1; }
grep -q "^claude-mcp: setLevel=t unknown-method=caught-as-error" "$work/report.txt" || { echo "FAIL: claude MCP errors"; exit 1; }
[ "$(grep -A1 -- "--- \*Warnings\* ---" "$work/report.txt" | tail -1)" = "none" ] || { echo "FAIL: warnings"; exit 1; }
# Diagram really drawn: render the static page in headless Chrome, if present.
if command -v google-chrome >/dev/null; then
  timeout 60 google-chrome --headless=new --disable-gpu --no-sandbox \
    --virtual-time-budget=8000 --dump-dom "file://$work/mermaid.html" 2>/dev/null \
    | grep -q "<svg" || { echo "FAIL: mermaid diagram not drawn"; exit 1; }
  echo "mermaid: diagram drawn in headless Chrome"
else
  echo "mermaid: skipped drawing check (no google-chrome)"
fi
echo "OK"
