use crate::yinsh::{Action, GameState};

fn possible_next_game_states<'a>(state: &'a GameState) -> Box<dyn Iterator<Item = Action> + 'a> {
    match state.turn_mode {
        crate::yinsh::TurnMode::RingPlacement => {
            Box::new(state.board.free_coords().map(Action::PlaceRing))
        }
        crate::yinsh::TurnMode::MarkerPlacement => Box::new(
            state
                .board
                .ring_coords(state.active_player)
                .map(Action::PlaceMarker),
        ),
        crate::yinsh::TurnMode::RingMovement(start) => Box::new(
            state
                .board
                .ring_moves(start)
                .into_iter()
                .map(move |end| Action::MoveRing(start, end)),
        ),
        crate::yinsh::TurnMode::RunRemoval(_) => todo!(),
        crate::yinsh::TurnMode::RingRemoval(_) => todo!(),
        crate::yinsh::TurnMode::RunRemovalFiller(_) => todo!(),
        crate::yinsh::TurnMode::MarkerPlacementFiller => todo!(),
    }
}

pub fn get_ai_player_action(state: &GameState) -> Action {
    let game_states: Vec<_> = possible_next_game_states(&state).collect();

    assert!(game_states.len() > 0);

    let random_index = rand::random::<usize>() % game_states.len();
    game_states[random_index].clone()
}
