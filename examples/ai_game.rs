use std::sync::mpsc;

use indicatif::{ProgressBar, ProgressStyle};
use rand::seq::SliceRandom;
use rayon::prelude::*;
use tracing::debug;

use yinsh::{GameState, Move, Player, SimpleHeuristic, YinshAi, YinshAiPlayer, possible_moves};

#[derive(Debug, Clone, Copy)]
enum Outcome {
    Draw,
    Winner(Player),
}

impl Outcome {
    fn invert(&self) -> Self {
        match self {
            Outcome::Draw => Outcome::Draw,
            Outcome::Winner(Player::A) => Outcome::Winner(Player::B),
            Outcome::Winner(Player::B) => Outcome::Winner(Player::A),
        }
    }
}

#[derive(Debug)]
struct GameResult {
    game_id: usize,
    outcome: Outcome,
    num_moves: usize,
}

fn play_match(a: &impl YinshAiPlayer, b: &impl YinshAiPlayer) -> (Outcome, usize) {
    let mut game_state = GameState::initial();
    let mut num_moves = 0;

    // Make two random ring moves for both players to improve the statistics.
    for _ in 0..4 {
        let mut free_coords: Vec<_> = game_state.board.free_coords().collect();
        free_coords.shuffle(&mut rand::thread_rng());
        game_state.perform_move(&Move::PlaceRing(free_coords[0]));
        num_moves += 1;
    }

    while game_state.winner().is_none() {
        if possible_moves(&game_state).count() == 0 {
            return (Outcome::Draw, num_moves);
        }

        let player_move = if game_state.active_player == Player::A {
            a.choose_move(&game_state)
        } else {
            b.choose_move(&game_state)
        };
        game_state.perform_move(&player_move);
        num_moves += 1;
    }

    (Outcome::Winner(game_state.winner().unwrap()), num_moves)
}

fn play_matches(a: &impl YinshAiPlayer, b: &impl YinshAiPlayer, num_games: usize) -> f64 {
    debug!(
        player_a = %a.identifier(),
        depth_a = a.search_depth(),
        player_b = %b.identifier(),
        depth_b = b.search_depth(),
        num_games,
        "Starting tournament"
    );

    let total_games = num_games * 2; // Each pair plays twice (swapping sides)

    println!("Player A: {} (depth {})", a.identifier(), a.search_depth());
    println!("Player B: {} (depth {})", b.identifier(), b.search_depth());
    println!();

    let pb = ProgressBar::new(total_games as u64);
    pb.set_style(
        ProgressStyle::default_bar()
            .template("{spinner:.cyan} [{bar:40.cyan/blue}] {pos}/{len} games │ {msg}")
            .unwrap()
            .progress_chars("━╸─"),
    );

    let (wins_a, wins_b, draws) = std::thread::scope(|s| {
        let (tx, rx) = mpsc::channel::<GameResult>();

        s.spawn(move || {
            (0..num_games).into_par_iter().for_each_with(tx, |tx, i| {
                // Game with A as first player
                let (outcome1, moves1) = play_match(a, b);
                tx.send(GameResult {
                    game_id: i * 2,
                    outcome: outcome1,
                    num_moves: moves1,
                })
                .unwrap();

                // Game with B as first player (invert outcome so A's perspective is consistent)
                let (outcome2, moves2) = play_match(b, a);
                tx.send(GameResult {
                    game_id: i * 2 + 1,
                    outcome: outcome2.invert(),
                    num_moves: moves2,
                })
                .unwrap();
            });
        });

        // Collect results on main thread
        let mut wins_a = 0;
        let mut wins_b = 0;
        let mut draws = 0;

        for result in rx {
            match result.outcome {
                Outcome::Draw => draws += 1,
                Outcome::Winner(Player::A) => wins_a += 1,
                Outcome::Winner(Player::B) => wins_b += 1,
            }

            debug!(
                game_id = result.game_id,
                outcome = ?result.outcome,
                num_moves = result.num_moves,
                wins_a,
                wins_b,
                draws,
                "Game finished"
            );

            pb.set_message(format!(
                "Score {:>3}:{:<3} ({} draws)",
                wins_a, wins_b, draws
            ));
            pb.inc(1);
        }

        (wins_a, wins_b, draws)
    });

    pb.finish_and_clear();

    let percentage_a = wins_a as f64 / total_games as f64 * 100.0;
    let percentage_b = wins_b as f64 / total_games as f64 * 100.0;
    let percentage_draws = draws as f64 / total_games as f64 * 100.0;

    println!();
    println!("========== Tournament Results ==========");
    println!("  Wins A:  {:>3} ({:>5.1}%)", wins_a, percentage_a);
    println!("  Wins B:  {:>3} ({:>5.1}%)", wins_b, percentage_b);
    println!("  Draws:   {:>3} ({:>5.1}%)", draws, percentage_draws);
    println!("=========================================");

    percentage_a
}

fn main() {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    let num_games: usize = std::env::args()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(50);

    let search_depth_a = 6;
    let player_a = YinshAi::new(
        SimpleHeuristic {
            f_points: 10_000,
            f_markers: 100,
            f_controlled_markers_own: 5,
            f_controlled_markers_opponent: 10,
            f_accessible_fields: 1,
        },
        search_depth_a,
    );

    let search_depth_b = 6;
    let player_b = YinshAi::new(
        SimpleHeuristic {
            f_points: 10_000,
            f_markers: 100,
            f_controlled_markers_own: 5,
            f_controlled_markers_opponent: 10,
            f_accessible_fields: 1,
        },
        search_depth_b,
    );

    play_matches(&player_a, &player_b, num_games);
}
