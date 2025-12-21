use yinsh::{Coord, GameState, Move, Player};
use yinsh_ai::{Heuristic, SimpleHeuristic};

#[test]
fn midgame_1() {
    let heuristic = SimpleHeuristic {
        f_points: 1_000,
        f_markers: 10,
        f_controlled_markers_own: 0,
        f_controlled_markers_opponent: 0,
        f_accessible_fields: 0,
    };

    let mut game_state = GameState::load_from("tests/midgame_1.yml");

    assert_eq!(game_state.active_player, Player::A);

    game_state.perform_move(&Move::PlaceMarker(Coord::new(2, 1)));
    game_state.perform_move(&Move::Wait);
    game_state.perform_move(&Move::MoveRing(Coord::new(2, 1), Coord::new(2, 2)));

    assert_eq!(game_state.active_player, Player::B);

    game_state.perform_move(&Move::PlaceMarker(Coord::new(-3, -4)));
    game_state.perform_move(&Move::Wait);
    game_state.perform_move(&Move::MoveRing(Coord::new(-3, -4), Coord::new(-4, -4)));

    assert_eq!(game_state.active_player, Player::A);

    game_state.perform_move(&Move::PlaceMarker(Coord::new(2, 2)));
    game_state.perform_move(&Move::Wait);
    game_state.perform_move(&Move::MoveRing(Coord::new(2, 2), Coord::new(2, 3)));

    assert_eq!(game_state.active_player, Player::B);

    game_state.perform_move(&Move::Wait);

    assert_eq!(heuristic.evaluate_for_player_a(&game_state), -1080);

    game_state.perform_move(&Move::RemoveRun(Coord::new(2, 2)));
    game_state.perform_move(&Move::Wait);

    assert_eq!(
        heuristic.evaluate_for_player_a(&game_state),
        -1080 + 1000 - 5 * 10
    );

    game_state.perform_move(&Move::RemoveRing(Coord::new(0, -3)));
    game_state.perform_move(&Move::PlaceMarker(Coord::new(-1, 1)));
    game_state.perform_move(&Move::Wait);
    game_state.perform_move(&Move::MoveRing(Coord::new(-1, 1), Coord::new(-1, 2)));

    assert_eq!(
        heuristic.evaluate_for_player_a(&game_state),
        -1080 + 1000 - 5 * 10 - 10
    );
}
