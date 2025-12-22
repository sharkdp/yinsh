use std::sync::mpsc;

use indicatif::{ProgressBar, ProgressStyle};
use rayon::prelude::*;

use yinsh::{GameState, Move, Player};
use yinsh_ai::{possible_moves, SimpleHeuristic, YinshAi, YinshAiPlayer};
use yinsh_nn::extract_features;

/// A training sample: (features, label)
pub struct Sample {
    pub features: Vec<f32>,
    pub label: f32,
}

/// Outcome of a game
#[derive(Debug, Clone, Copy)]
pub enum Outcome {
    Draw,
    Winner(Player),
}

/// Result of a self-play game, including all visited states
pub struct GameRecord {
    pub states: Vec<GameState>,
    pub outcome: Outcome,
}

/// Play a single game using the given AI, collecting all game states.
fn play_game(ai: &impl YinshAiPlayer) -> GameRecord {
    use rand::seq::SliceRandom;

    let mut game_state = GameState::initial();
    let mut states = Vec::new();

    // Random opening: 4 ring placements
    for _ in 0..4 {
        let mut free_coords: Vec<_> = game_state.board.free_coords().collect();
        free_coords.shuffle(&mut rand::rng());
        game_state.perform_move(&Move::PlaceRing(free_coords[0]));
    }

    // Play until game ends
    while game_state.winner().is_none() {
        if possible_moves(&game_state).count() == 0 {
            return GameRecord {
                states,
                outcome: Outcome::Draw,
            };
        }

        // Record state before move
        states.push(game_state.clone());

        let player_move = ai.choose_move(&game_state);
        game_state.perform_move(&player_move);
    }

    GameRecord {
        states,
        outcome: Outcome::Winner(game_state.winner().unwrap()),
    }
}

/// Generate game records from self-play games.
pub fn generate_game_records(num_games: usize, search_depth: usize) -> Vec<GameRecord> {
    let pb = ProgressBar::new(num_games as u64);
    pb.set_style(
        ProgressStyle::default_bar()
            .template("{spinner:.cyan} [{bar:40.cyan/blue}] {pos}/{len} games")
            .unwrap()
            .progress_chars("━╸─"),
    );

    let (tx, rx) = mpsc::channel::<GameRecord>();

    std::thread::scope(|s| {
        s.spawn(move || {
            (0..num_games).into_par_iter().for_each_with(tx, |tx, _| {
                let ai = YinshAi::new(SimpleHeuristic::default(), search_depth);
                let record = play_game(&ai);
                tx.send(record).unwrap();
            });
        });

        let mut records = Vec::new();
        for record in rx {
            records.push(record);
            pb.inc(1);
        }

        pb.finish_and_clear();
        records
    })
}

/// Maximum number of samples to take from each game.
const SAMPLES_PER_GAME: usize = 5;

/// Convert a game record to training samples.
///
/// Randomly samples up to SAMPLES_PER_GAME states from the game.
/// Label is +1 for A win, -1 for B win, 0 for draw.
/// Labels are discounted by game progress (later states get stronger labels).
pub fn record_to_samples(record: GameRecord) -> Vec<Sample> {
    use rand::seq::SliceRandom;

    let base_label = match record.outcome {
        Outcome::Draw => 0.0,
        Outcome::Winner(Player::A) => 1.0,
        Outcome::Winner(Player::B) => -1.0,
    };

    let num_states = record.states.len();

    // Create indexed states for sampling
    let mut indexed_states: Vec<(usize, GameState)> = record
        .states
        .into_iter()
        .enumerate()
        .collect();

    // Randomly sample up to SAMPLES_PER_GAME states
    indexed_states.shuffle(&mut rand::rng());
    indexed_states.truncate(SAMPLES_PER_GAME);

    indexed_states
        .into_iter()
        .map(|(i, state)| {
            // Discount factor: states closer to the end get stronger labels
            // This helps the network learn that later positions are more indicative of outcome
            let progress = (i + 1) as f32 / num_states as f32;
            let discount = progress.sqrt(); // sqrt gives more weight to later states

            let label = base_label * discount;
            let features = extract_features(&state);

            Sample { features, label }
        })
        .collect()
}

/// Generate training data from self-play games.
///
/// Returns a vector of (features, label) samples.
pub fn generate_training_data(num_games: usize, search_depth: usize) -> Vec<Sample> {
    generate_game_records(num_games, search_depth)
        .into_iter()
        .flat_map(record_to_samples)
        .collect()
}

/// Shuffle and split game records into training and validation sets,
/// then convert to samples. This ensures whole games stay together,
/// avoiding data leakage from correlated states.
pub fn split_train_val(mut records: Vec<GameRecord>, val_ratio: f32) -> (Vec<Sample>, Vec<Sample>) {
    use rand::seq::SliceRandom;

    records.shuffle(&mut rand::rng());

    let val_count = (records.len() as f32 * val_ratio) as usize;
    let val_records = records.split_off(records.len() - val_count);

    let train_samples: Vec<Sample> = records.into_iter().flat_map(record_to_samples).collect();
    let val_samples: Vec<Sample> = val_records.into_iter().flat_map(record_to_samples).collect();

    (train_samples, val_samples)
}

/// Calculate mean squared error on a validation set.
pub fn calculate_mse(heuristic: &yinsh_nn::NNHeuristic, samples: &[Sample]) -> f32 {
    let mut total_error = 0.0;
    for sample in samples {
        let output = heuristic.network().forward(&sample.features);
        let prediction = output[0];

        let error = (prediction - sample.label).powi(2);
        total_error += error;
    }

    total_error / samples.len() as f32
}
