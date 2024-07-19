use std::iter;

use minimax::{Game, Winner};

use crate::{Action, GameState, TurnMode};

pub struct Yinsh;

impl Game for Yinsh {
    type S = GameState;

    type M = Action;

    fn generate_moves(state: &Self::S, moves: &mut Vec<Self::M>) {
        moves.extend(possible_actions(state));
    }

    fn apply(state: &mut Self::S, m: Self::M) -> Option<Self::S> {
        let mut new_state = state.clone(); // TODO: is this necessary?
        new_state.transition(&m);
        Some(new_state) // TODO: we can avoid cloning here by returning None and implementing undo
    }

    fn get_winner(state: &Self::S) -> Option<Winner> {
        match state.winner() {
            Some(p) if p == state.active_player => Some(Winner::PlayerToMove),
            Some(_) => Some(Winner::PlayerJustMoved),
            None => None,
        }
    }
}

pub fn possible_actions<'a>(state: &'a GameState) -> Box<dyn Iterator<Item = Action> + 'a> {
    match state.turn_mode {
        TurnMode::RingPlacement => Box::new(state.board.free_coords().map(Action::PlaceRing)),
        TurnMode::MarkerPlacement => Box::new(
            state
                .board
                .marker_moves(state.active_player)
                .map(Action::PlaceMarker),
        ),
        TurnMode::RingMovement(start) => Box::new(
            state
                .board
                .ring_moves(start)
                .into_iter()
                .map(move |end| Action::MoveRing(start, end)),
        ),
        TurnMode::RunRemoval(_) => Box::new(
            state
                .board
                .run_coords(state.active_player)
                .into_iter()
                .map(Action::RemoveRun), // TODO: this produces too many moves
        ),
        TurnMode::RingRemoval(_) => Box::new(
            state
                .board
                .ring_coords(state.active_player)
                .map(Action::RemoveRing),
        ),
        TurnMode::WaitForRunRemoval(_)
        | TurnMode::WaitForMarkerPlacement
        | TurnMode::WaitForRingMovement(_)
        | TurnMode::WaitForRingRemoval(_) => Box::new(iter::once(Action::Wait)),
    }
}
