#!/usr/bin/env bash
set -euo pipefail

agda_bin="${AGDA_BIN:-agda}"
export LANG=C.utf8
export LC_ALL=C.utf8
export GHC_CHARENC=UTF-8

if [[ -n "${BUILD_CORES:-}" ]]; then
  cores="$BUILD_CORES"
else
  cores="$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)"
fi

base_workdir="${BUILD_WORKDIR:-${WORKDIR:-.}}"
compile_dir="${AGDA_COMPILE_DIR:-${base_workdir}/build/agda}"
xdg_data="${XDG_DATA_HOME:-${base_workdir}/build/xdg-data}"
xdg_cache="${XDG_CACHE_HOME:-${base_workdir}/build/xdg-cache}"
agda_data_dir="$(env XDG_DATA_HOME="$xdg_data" XDG_CACHE_HOME="$xdg_cache" AGDA_EXEC_OPTIONS= "$agda_bin" --library-file=/dev/null --no-libraries --no-default-libraries --print-agda-data-dir 2>/dev/null)"
agda_env=(env XDG_DATA_HOME="$xdg_data" XDG_CACHE_HOME="$xdg_cache" AGDA_EXEC_OPTIONS= AGDA_DATA_DIR="$agda_data_dir")
agda_prim_dir="${agda_data_dir}/lib/prim"
agda_flags="-i src/agda --include-path=${agda_prim_dir} --no-default-libraries --no-libraries --ghc-flag=-Wno-star-is-type --ghc-flag=-j${cores}"

"${agda_env[@]}" "$agda_bin" $agda_flags --compile-dir="$compile_dir" --compile src/agda/Examples/ExporterMakefile.agda
"$compile_dir/ExporterMakefile"
cp Makefile.generated Makefile
