#!/usr/bin/env bash
# CUDA build + run environment for `pm --features cuda` on NixOS.
#
# NixOS splits the CUDA toolkit into per-component store paths and bindgen_cuda
# (candle-kernels' build script) expects one classic root with bin/ + include/.
# This script (1) assembles a merged root of symlinks, (2) exports the env for
# building, (3) exports LD_LIBRARY_PATH for running. Source it, then:
#
#   source cuda-env.sh
#   cargo build --release --features cuda
#   ./target/release/pm train-set ...        # runs on the RTX 4080
#
# Verified 2026-07-10: candle 0.9 + CUDA 12.9 + driver 610.43, pm train uses
# ~1.2 GB GPU during a fit.

set -u

_first() { ls -d "$@" 2>/dev/null | grep -v '\.drv$' | head -1; }

NVCC_DIR=$(_first /nix/store/*cuda_nvcc*[0-9.])
CUDART_DIR=$(_first /nix/store/*cuda_cudart*[0-9.])
CCCL_DIR=$(_first /nix/store/*cuda_cccl*[0-9.])
NVRTC_LIB=$(_first /nix/store/*cuda_nvrtc*-lib)
CURAND_LIB=$(_first /nix/store/*libcurand*-lib)
CUBLAS_LIB=$(_first /nix/store/*libcublas*-lib)

CROOT="${CUDA_MERGED_ROOT:-/tmp/cuda-root-$USER}"
mkdir -p "$CROOT/include" "$CROOT/bin"
ln -sf "$NVCC_DIR"/bin/* "$CROOT/bin/" 2>/dev/null
ln -sfn "$NVCC_DIR/nvvm" "$CROOT/nvvm"
for d in "$CUDART_DIR/include" "$CCCL_DIR/include" "$NVCC_DIR/include"; do
    [ -d "$d" ] && ln -sf "$d"/* "$CROOT/include/" 2>/dev/null
done

export CUDA_ROOT="$CROOT"
export CUDA_PATH="$CROOT"
export NVCC_APPEND_FLAGS="-I$CROOT/include"
export RUSTFLAGS="${RUSTFLAGS:-} -L$CUDART_DIR/lib -L$NVRTC_LIB/lib -L$CURAND_LIB/lib -L$CUBLAS_LIB/lib -L/run/opengl-driver/lib"
export LD_LIBRARY_PATH="/run/opengl-driver/lib:$CUDART_DIR/lib:$NVRTC_LIB/lib:$CURAND_LIB/lib:$CUBLAS_LIB/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

echo "cuda env ready (root: $CROOT)"
