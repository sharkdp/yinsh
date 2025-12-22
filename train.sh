#!/bin/bash

set -e

training_data_generation_games=20000
training_data_generation_depth=4
epochs=20

cargo run \
    -p yinsh_nn_training \
    --release \
    "$training_data_generation_games" \
    "$training_data_generation_depth" \
    "$epochs"
