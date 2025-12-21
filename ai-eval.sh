#!/bin/bash

set -e

cargo run --release --bin yinsh-ai-eval -- "$@"
