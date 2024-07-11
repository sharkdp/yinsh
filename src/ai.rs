use crate::yinsh::{Action, GameState};

pub fn gamestates<'a>(state: &'a GameState) -> Box<dyn Iterator<Item = Action> + 'a> {
    match state.turn_mode {
        crate::yinsh::TurnMode::PlaceRing => {
            Box::new(state.board.free_coords().map(Action::PlaceRing))
        }
        crate::yinsh::TurnMode::PlaceMarker => Box::new(
            state
                .board
                .ring_coords(state.active_player)
                .map(Action::PlaceMarker),
        ),
        crate::yinsh::TurnMode::MoveRing(ref start) => {
            Box::new(
                state
                    .board
                    .free_coords()
                    // .filter(|c| c != start)
                    .map(|end| Action::MoveRing(start.clone(), end)),
            ) // TODO
        }
        crate::yinsh::TurnMode::RemoveRun(_) => todo!(),
        crate::yinsh::TurnMode::RemoveRing(_) => todo!(),
        crate::yinsh::TurnMode::WaitRemoveRun(_) => todo!(),
        crate::yinsh::TurnMode::WaitPlaceMarker => todo!(),
    }
}
