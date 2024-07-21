use rand::seq::SliceRandom;
use rayon::prelude::*;

use yinsh::{possible_moves, GameState, Move, Player, SimpleHeuristic, YinshAi, YinshAiPlayer};

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

fn play_match(a: &impl YinshAiPlayer, b: &impl YinshAiPlayer) -> Outcome {
    let mut game_state = GameState::initial();

    // Make two random ring moves for both players to improve the statistics.
    for _ in 0..4 {
        let mut free_coords: Vec<_> = game_state.board.free_coords().collect();
        free_coords.shuffle(&mut rand::thread_rng());
        game_state.perform_move(&Move::PlaceRing(free_coords[0]));
    }

    while game_state.winner().is_none() {
        if possible_moves(&game_state).count() == 0 {
            return Outcome::Draw;
        }

        let player_move = if game_state.active_player == Player::A {
            a.choose_move(&game_state)
        } else {
            b.choose_move(&game_state)
        };
        game_state.perform_move(&player_move);
    }

    Outcome::Winner(game_state.winner().unwrap())
}

fn play_matches(a: &impl YinshAiPlayer, b: &impl YinshAiPlayer, num_games: usize) -> f64 {
    println!("Playing {num_games} games between:");
    println!(
        "  Player A (depth = {:>2}): {}",
        a.search_depth(),
        a.identifier()
    );
    println!(
        "  Player B (depth = {:>2}): {}",
        b.search_depth(),
        b.identifier()
    );

    let mut wins_a = 0;
    let mut wins_b = 0;
    let mut draws = 0;

    let outcomes: Vec<_> = (0..num_games)
        .into_par_iter()
        .flat_map(|_| {
            let outcome1 = play_match(a, b);
            let outcome2 = play_match(b, a).invert();

            [outcome1, outcome2]
        })
        .collect();

    for outcome in outcomes {
        match outcome {
            Outcome::Draw => draws += 1,
            Outcome::Winner(Player::A) => wins_a += 1,
            Outcome::Winner(Player::B) => wins_b += 1,
        }
    }

    let percentage_a = wins_a as f64 / (2 * num_games) as f64 * 100.0;
    let percentage_b = wins_b as f64 / (2 * num_games) as f64 * 100.0;
    let percentage_draws = draws as f64 / (2 * num_games) as f64 * 100.0;

    println!("Wins A: {wins_a:>3} ({percentage_a:.0}%)");
    println!("Wins B: {wins_b:>3} ({percentage_b:.0}%)");
    println!("Draws:  {draws:>3} ({percentage_draws:.0}%)");
    println!();

    percentage_a
}

fn main() {
    let num_games = 12;

    let search_depth_a = 8;
    let player_a = YinshAi::new(
        SimpleHeuristic {
            f_points: 10_000,
            f_markers: 100,
            f_controlled_markers_own: 5,
            f_controlled_markers_opponent: 10,
            f_accessible_fields: 0,
        },
        search_depth_a,
    );

    let search_depth_b = 10;
    let player_b = YinshAi::new(
        SimpleHeuristic {
            f_points: 10_000,
            f_markers: 100,
            f_controlled_markers_own: 0,
            f_controlled_markers_opponent: 0,
            f_accessible_fields: 0,
        },
        search_depth_b,
    );

    play_matches(&player_a, &player_b, num_games);
}
