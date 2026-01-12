#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 2 ]]; then
  echo "usage: $0 <target> <script>"
  exit 2
fi

target="$1"
script="$2"

profile_dir="${PROFILE_DIR:-${BUILD_PROFILE_DIR:-build/profiles.d}}"
profile_log="${PROFILE_LOG:-${profile_dir}/profile-$(date +%Y%m%dT%H%M%S%z)-$$.jsonl}"

mkdir -p "$profile_dir"
start="$(date +%s%N)"

if [[ ! -f "$script" ]]; then
  echo "missing recipe script: $script"
  echo "run: make regen-makefile"
  exit 2
fi

bash "$script"
rc=$?

end="$(date +%s%N)"
elapsed_ms="$(( (end - start) / 1000000 ))"
if [[ $rc -eq 0 ]]; then
  status="ok"
else
  status="fail"
fi

jq -nc \
  --arg target "$target" \
  --argjson start_ns "$start" \
  --argjson end_ns "$end" \
  --argjson elapsed_ms "$elapsed_ms" \
  --arg status "$status" \
  '{target:$target,start_ns:$start_ns,end_ns:$end_ns,elapsed_ms:$elapsed_ms,status:$status}' \
  >> "$profile_log"

exit "$rc"
