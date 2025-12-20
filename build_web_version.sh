#!/bin/bash

set -euo pipefail

export CARGO_TARGET_DIR=target/

cargo build --profile wasm-release --target wasm32-unknown-unknown

wasm-bindgen \
    --target web \
    --no-typescript \
    --out-dir ./web \
    --out-name "yinsh" \
    ./target/wasm32-unknown-unknown/wasm-release/yinsh.wasm

#wasm-opt -Oz -o ./web/yinsh_bg.wasm ./web/yinsh_bg.wasm

#rsync --archive --stats --progress --human-readable web/* shark.fish:david-peter.de/yinsh/preview/
