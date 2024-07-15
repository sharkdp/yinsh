use yinsh::{get_ai_player_action, possible_actions, GameState, Player};

enum Outcome {
    Draw,
    Winner(Player),
}

fn play_match(search_depth_a: usize, search_depth_b: usize) -> Outcome {
    let mut game_state = GameState::initial();

    println!(
        "Playing game with search depths: A={}, B={}",
        search_depth_a, search_depth_b
    );

    while game_state.winner().is_none() {
        if possible_actions(&game_state).count() == 0 {
            println!("The game ends in a draw!");
            game_state.save_to("draw.yml");
            return Outcome::Draw;
        }
        let action = if game_state.active_player == Player::A {
            get_ai_player_action(search_depth_a, &game_state)
        } else {
            get_ai_player_action(search_depth_b, &game_state)
        };
        game_state.transition(&action);
    }

    println!("Game over! Winner: {:?}", game_state.winner());

    Outcome::Winner(game_state.winner().unwrap())
}

fn main() {
    let num_games = std::env::args()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .unwrap();

    // Read search depth from command line arguments:
    let search_depth_a = std::env::args()
        .nth(2)
        .and_then(|s| s.parse().ok())
        .unwrap();
    let search_depth_b = std::env::args()
        .nth(3)
        .and_then(|s| s.parse().ok())
        .unwrap();

    let mut wins_a = 0;
    let mut wins_b = 0;
    let mut draws = 0;

    for _ in 0..num_games {
        match play_match(search_depth_a, search_depth_b) {
            Outcome::Draw => draws += 1,
            Outcome::Winner(Player::A) => wins_a += 1,
            Outcome::Winner(Player::B) => wins_b += 1,
        }
    }

    println!("Results: A={} B={} Draw={}", wins_a, wins_b, draws);
}
