#!/bin/bash

set -e

games=100000
depth=4
epochs=100

data_file="training_data.bin"

yinsh_nn_training() {
    cargo run -p yinsh_nn_training --release -- "$@"
}

# Generate training data (skip if data file exists)
if [[ ! -f "$data_file" ]]; then
    echo "Generating training data..."
    yinsh_nn_training generate \
        --games "$games" \
        --depth "$depth" \
        --output "$data_file"
else
    echo "Using existing training data: $data_file"
fi

# Train the network
yinsh_nn_training train \
    --input "$data_file" \
    --epochs "$epochs"
