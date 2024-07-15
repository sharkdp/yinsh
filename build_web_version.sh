#!/bin/bash

set -euo pipefail

cargo build --release --target wasm32-unknown-unknown

wasm-bindgen --no-typescript --target web \
    --out-dir ./web \
    --out-name "yinsh" \
    ./target/wasm32-unknown-unknown/release/yinsh.wasm

rsync --archive --stats --progress --human-readable web/* shark.fish:david-peter.de/yinsh/preview/
