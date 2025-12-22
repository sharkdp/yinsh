use yinsh_nn::NNHeuristic;
use yinsh_nn_training::{generate_game_records, split_train_val};

fn calculate_mse(heuristic: &NNHeuristic, inputs: &[Vec<f32>], targets: &[Vec<f32>]) -> f32 {
    let mut mse = 0.0;
    for (input, target) in inputs.iter().zip(targets.iter()) {
        let output = heuristic.network().forward(input);
        let error = (output[0] - target[0]).powi(2);
        mse += error;
    }
    mse / inputs.len() as f32
}

fn main() {
    let num_games: usize = std::env::args()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(500);

    let search_depth: usize = std::env::args()
        .nth(2)
        .and_then(|s| s.parse().ok())
        .unwrap_or(4);

    let epochs: usize = std::env::args()
        .nth(3)
        .and_then(|s| s.parse().ok())
        .unwrap_or(100);

    println!("=== Yinsh Neural Network Training ===");
    println!("Games: {}", num_games);
    println!("Search depth: {}", search_depth);
    println!("Epochs: {}", epochs);
    println!();

    // Generate training data
    println!("Generating training data from self-play games...");
    let records = generate_game_records(num_games, search_depth);
    let total_states: usize = records.iter().map(|r| r.states.len()).sum();
    println!(
        "Generated {} states from {} games",
        total_states, num_games
    );
    println!();

    // Split into train/validation (at the game level to avoid data leakage)
    let (train_samples, val_samples) = split_train_val(records, 0.1);
    println!(
        "Training samples: {}, Validation samples: {}",
        train_samples.len(),
        val_samples.len()
    );
    println!();

    // Prepare training data as slices
    let train_inputs: Vec<Vec<f32>> = train_samples.iter().map(|s| s.features.clone()).collect();
    let train_targets: Vec<Vec<f32>> = train_samples.iter().map(|s| vec![s.label]).collect();
    let val_inputs: Vec<Vec<f32>> = val_samples.iter().map(|s| s.features.clone()).collect();
    let val_targets: Vec<Vec<f32>> = val_samples.iter().map(|s| vec![s.label]).collect();

    // Create and train the network
    println!("Training neural network...");
    let mut heuristic = NNHeuristic::new_untrained(10_000);

    // Print initial validation score
    let initial_val_mse = calculate_mse(&heuristic, &val_inputs, &val_targets);
    println!("Initial validation MSE: {:.6}", initial_val_mse);
    println!();

    // Train using fit method with mini-batches
    let batch_size = 32;
    let samples_per_epoch = train_inputs.len();

    for epoch in 0..epochs {
        let mut epoch_loss = 0.0;

        for batch_start in (0..samples_per_epoch).step_by(batch_size) {
            let batch_end = (batch_start + batch_size).min(samples_per_epoch);

            for i in batch_start..batch_end {
                heuristic
                    .network_mut()
                    .fit_one(&train_inputs[i], &train_targets[i]);
            }

            // Calculate batch loss
            for i in batch_start..batch_end {
                let output = heuristic.network().forward(&train_inputs[i]);
                epoch_loss += (output[0] - train_targets[i][0]).powi(2);
            }
        }

        let avg_train_loss = epoch_loss / samples_per_epoch as f32;
        let val_mse = calculate_mse(&heuristic, &val_inputs, &val_targets);

        println!(
            "Epoch {}/{}: train_loss = {:.6}, val_loss = {:.6}",
            epoch + 1,
            epochs,
            avg_train_loss,
            val_mse
        );
    }

    println!();

    // Save the model
    let model_path = "crates/yinsh_nn/model.bin";
    heuristic.save(model_path);
    println!("Model saved to {}", model_path);
}
