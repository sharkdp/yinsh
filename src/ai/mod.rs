use std::iter;

use crate::yinsh::{Action, GameState, TurnMode};

fn possible_next_game_states<'a>(state: &'a GameState) -> Box<dyn Iterator<Item = Action> + 'a> {
    match state.turn_mode {
        TurnMode::RingPlacement => Box::new(state.board.free_coords().map(Action::PlaceRing)),
        TurnMode::MarkerPlacement => Box::new(
            state
                .board
                .ring_coords(state.active_player)
                .map(Action::PlaceMarker),
        ),
        TurnMode::RingMovement(start) => Box::new(
            state
                .board
                .ring_moves(start)
                .into_iter()
                .map(move |end| Action::MoveRing(start, end)),
        ),
        TurnMode::RunRemoval(_) => todo!(),
        TurnMode::RingRemoval(_) => todo!(),
        TurnMode::RunRemovalFiller(_) => Box::new(iter::once(Action::Wait)),
        TurnMode::MarkerPlacementFiller => todo!(),
    }
}

pub fn get_ai_player_action(state: &GameState) -> Action {
    let game_states: Vec<_> = possible_next_game_states(&state).collect();

    assert!(game_states.len() > 0); // TODO: handle draws

    let random_index = rand::random::<usize>() % game_states.len();
    game_states[random_index].clone()
}
