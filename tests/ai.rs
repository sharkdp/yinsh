use minimax::Evaluator;
use yinsh::{Action, Coord, GameState, MarkerCountHeuristic, Player};

#[test]
fn midgame_1() {
    let heuristic = MarkerCountHeuristic {};

    let mut game_state = GameState::load_from("tests/midgame_1.yml");

    assert_eq!(game_state.active_player, Player::A);

    game_state.transition(&Action::PlaceMarker(Coord::new(2, 1)));
    game_state.transition(&Action::Wait);
    game_state.transition(&Action::MoveRing(Coord::new(2, 1), Coord::new(2, 2)));

    assert_eq!(game_state.active_player, Player::B);

    game_state.transition(&Action::PlaceMarker(Coord::new(-3, -4)));
    game_state.transition(&Action::Wait);
    game_state.transition(&Action::MoveRing(Coord::new(-3, -4), Coord::new(-4, -4)));

    assert_eq!(game_state.active_player, Player::A);

    game_state.transition(&Action::PlaceMarker(Coord::new(2, 2)));
    game_state.transition(&Action::Wait);
    game_state.transition(&Action::MoveRing(Coord::new(2, 2), Coord::new(2, 3)));

    assert_eq!(game_state.active_player, Player::B);

    game_state.transition(&Action::Wait);

    assert_eq!(heuristic.evaluate(&game_state), -1080);

    game_state.transition(&Action::RemoveRun(Coord::new(2, 2)));
    game_state.transition(&Action::Wait);

    assert_eq!(heuristic.evaluate(&game_state), -1080 + 1000 - 5 * 10);

    game_state.transition(&Action::RemoveRing(Coord::new(0, -3)));
    game_state.transition(&Action::PlaceMarker(Coord::new(-1, 1)));
    game_state.transition(&Action::Wait);
    game_state.transition(&Action::MoveRing(Coord::new(-1, 1), Coord::new(-1, 2)));

    assert_eq!(heuristic.evaluate(&game_state), -1080 + 1000 - 5 * 10 - 10);
}
