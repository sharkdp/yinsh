use std::path::PathBuf;

use clap::{Parser, Subcommand};
use yinsh_nn::NNHeuristic;
use yinsh_nn_training::{TrainingData, generate_game_records, record_to_samples};

#[derive(Parser)]
#[command(name = "yinsh-nn-train")]
#[command(about = "Training pipeline for Yinsh neural network heuristic")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate training data from self-play games
    Generate {
        /// Output file for training data
        #[arg(long, default_value = "training_data.bin")]
        output: PathBuf,

        /// Number of games to play
        #[arg(short = 'n', long, default_value = "1000")]
        games: usize,

        /// Search depth for AI during self-play
        #[arg(long, default_value = "4")]
        depth: usize,
    },

    /// Train the neural network on generated data
    Train {
        /// Input file with training data
        #[arg(long, default_value = "training_data.bin")]
        input: PathBuf,

        /// Output file for trained model
        #[arg(long, default_value = "crates/yinsh_nn/model.bin")]
        output: PathBuf,

        /// Number of training epochs
        #[arg(long, default_value = "100")]
        epochs: usize,

        /// Validation split ratio
        #[arg(long, default_value = "0.1")]
        val_ratio: f32,

        /// Early stopping patience (epochs without improvement before stopping)
        #[arg(long, default_value = "10")]
        patience: usize,
    },
}

fn calculate_mse(heuristic: &NNHeuristic, inputs: &[Vec<f32>], targets: &[Vec<f32>]) -> f32 {
    let mut mse = 0.0;
    for (input, target) in inputs.iter().zip(targets.iter()) {
        let output = heuristic.network().forward(input);
        let error = (output[0] - target[0]).powi(2);
        mse += error;
    }
    mse / inputs.len() as f32
}

fn cmd_generate(output: PathBuf, games: usize, depth: usize) {
    println!("=== Generating Training Data ===");
    println!("Games: {}", games);
    println!("Search depth: {}", depth);
    println!("Output: {}", output.display());
    println!();

    println!("Playing self-play games...");
    let records = generate_game_records(games, depth);

    let total_states: usize = records.iter().map(|r| r.states.len()).sum();
    println!("Generated {} states from {} games", total_states, games);

    // Convert to samples
    let samples: Vec<_> = records.into_iter().flat_map(record_to_samples).collect();
    println!("Sampled {} training examples", samples.len());

    // Save to file
    let data = TrainingData { samples };
    data.save(&output).expect("Failed to save training data");
    println!("Saved to {}", output.display());
}

fn cmd_train(input: PathBuf, output: PathBuf, epochs: usize, val_ratio: f32, patience: usize) {
    println!("=== Training Neural Network ===");
    println!("Input: {}", input.display());
    println!("Output: {}", output.display());
    println!("Epochs: {}", epochs);
    println!("Validation ratio: {}", val_ratio);
    println!("Early stopping patience: {}", patience);
    println!();

    // Load training data
    println!("Loading training data...");
    let data = TrainingData::load(&input).expect("Failed to load training data");
    println!("Loaded {} samples", data.samples.len());

    // Split into train/validation
    let (train_samples, val_samples) = data.split(val_ratio);
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

    // Early stopping state
    let mut best_val_loss = f32::MAX;
    let mut best_epoch = 0;
    let mut epochs_without_improvement = 0;

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

        // Check for improvement
        let improved = val_mse < best_val_loss;
        if improved {
            best_val_loss = val_mse;
            best_epoch = epoch + 1;
            epochs_without_improvement = 0;

            // Save best model
            heuristic.save(output.to_str().unwrap());
        } else {
            epochs_without_improvement += 1;
        }

        println!(
            "Epoch {}/{}: train_loss = {:.6}, val_loss = {:.6}{}",
            epoch + 1,
            epochs,
            avg_train_loss,
            val_mse,
            if improved { " (best)" } else { "" }
        );

        // Early stopping check
        if epochs_without_improvement >= patience {
            println!();
            println!(
                "Early stopping: no improvement for {} epochs",
                patience
            );
            break;
        }
    }

    println!();
    println!(
        "Best model from epoch {} with val_loss = {:.6}",
        best_epoch, best_val_loss
    );
    println!("Model saved to {}", output.display());
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Generate {
            output,
            games,
            depth,
        } => cmd_generate(output, games, depth),
        Commands::Train {
            input,
            output,
            epochs,
            val_ratio,
            patience,
        } => cmd_train(input, output, epochs, val_ratio, patience),
    }
}
