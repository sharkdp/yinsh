use yinsh::{possible_actions, GameState, Player, SimpleHeuristic, YinshAi, YinshAiPlayer};

enum Outcome {
    Draw,
    Winner(Player),
}

fn play_match(a: &impl YinshAiPlayer, b: &impl YinshAiPlayer) -> Outcome {
    let mut game_state = GameState::initial();

    // println!("Players:\nA={}\nB={}", a.identifier(), b.identifier());

    while game_state.winner().is_none() {
        if possible_actions(&game_state).count() == 0 {
            println!("The game ends in a draw!");
            game_state.save_to("draw.yml");
            return Outcome::Draw;
        }
        let action = if game_state.active_player == Player::A {
            a.choose_action(&game_state)
        } else {
            b.choose_action(&game_state)
        };
        game_state.transition(&action);

        // println!("Action: {:?}", action);
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

    let mut player_a = YinshAi::new(
        SimpleHeuristic {
            f_points: 10_000,
            f_markers: 100,
            f_controlled_markers_own: 0,
            f_controlled_markers_opponent: 10,
            f_accessible_fields: 0,
        },
        search_depth_a,
    );
    let mut player_b = YinshAi::new(
        SimpleHeuristic {
            f_points: 10_000,
            f_markers: 100,
            f_controlled_markers_own: 0,
            f_controlled_markers_opponent: 10,
            f_accessible_fields: 0,
        },
        search_depth_b,
    );

    let mut wins_a = 0;
    let mut wins_b = 0;
    let mut draws = 0;

    for _ in 0..num_games {
        match play_match(&mut player_a, &mut player_b) {
            Outcome::Draw => draws += 1,
            Outcome::Winner(Player::A) => wins_a += 1,
            Outcome::Winner(Player::B) => wins_b += 1,
        }
        match play_match(&mut player_b, &mut player_a) {
            Outcome::Draw => draws += 1,
            Outcome::Winner(Player::A) => wins_b += 1,
            Outcome::Winner(Player::B) => wins_a += 1,
        }
    }

    println!("Results: A={} B={} Draw={}", wins_a, wins_b, draws);
}
