#!/bin/bash

set -e

cargo run --release --example ai_game -- "$@"
