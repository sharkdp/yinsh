use std::iter;

use minimax::{Evaluation, Evaluator, Game, Negamax, Strategy, Winner};

use crate::{
    yinsh::{Action, GameState, TurnMode},
    Player,
};

struct Yinsh;

impl Game for Yinsh {
    type S = GameState;

    type M = Action;

    fn generate_moves(state: &Self::S, moves: &mut Vec<Self::M>) {
        moves.extend(possible_actions(state));
    }

    fn apply(state: &mut Self::S, m: Self::M) -> Option<Self::S> {
        let mut new_state = state.clone();
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

struct MarkerCountHeuristic;

impl Evaluator for MarkerCountHeuristic {
    type G = Yinsh;

    fn evaluate(&self, state: &GameState) -> Evaluation {
        type Score = Evaluation;

        // Evaluate position from perspective of player A. If the active
        // player is B instead, we negate in the end.
        let score_points = (state.points_a as Score) - (state.points_b as Score);

        let score_markers = (state.board.num_markers(Player::A) as Score)
            - (state.board.num_markers(Player::B) as Score);

        let score = 1000 * score_points + 10 * score_markers;

        if state.active_player == Player::A {
            score
        } else {
            -score
        }
    }
}

fn possible_actions<'a>(state: &'a GameState) -> Box<dyn Iterator<Item = Action> + 'a> {
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

pub fn get_ai_player_action(search_depth: usize, state: &GameState) -> Action {
    // Early return if the only thing we can do is wait. Would be great
    // if this could be handled by 'minimax' itself (if there is only one
    // possible mobe in choose_move, return that immediately).
    match state.turn_mode {
        TurnMode::WaitForRunRemoval(_)
        | TurnMode::WaitForRingMovement(_)
        | TurnMode::WaitForRingRemoval(_)
        | TurnMode::WaitForMarkerPlacement => {
            return Action::Wait;
        }
        _ => {}
    }

    let depth: u8 = if matches!(state.turn_mode, TurnMode::RingPlacement) {
        3
    } else {
        search_depth.try_into().unwrap()
    };

    let mut strategy = Negamax::new(MarkerCountHeuristic {}, depth);
    assert!(state.active_player == Player::B);
    strategy.choose_move(&state).unwrap()
}
