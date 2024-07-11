use crate::yinsh::{Action, GameState};

fn possible_next_gamestates<'a>(state: &'a GameState) -> Box<dyn Iterator<Item = Action> + 'a> {
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
        crate::yinsh::TurnMode::RingMovement(ref start) => {
            Box::new(
                state
                    .board
                    .free_coords()
                    // .filter(|c| c != start)
                    .map(|end| Action::MoveRing(start.clone(), end)),
            ) // TODO
        }
        crate::yinsh::TurnMode::RunRemoval(_) => todo!(),
        crate::yinsh::TurnMode::RingRemoval(_) => todo!(),
        crate::yinsh::TurnMode::RunRemovalFiller(_) => todo!(),
        crate::yinsh::TurnMode::MarkerPlacementFiller => todo!(),
    }
}

pub fn get_ai_player_action(state: &GameState) -> Action {
    let mut gamestates = possible_next_gamestates(&state);
    let first = gamestates.next();

    assert!(first.is_some());

    first.unwrap()
}
