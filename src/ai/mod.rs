use std::iter;

use minimax::{IterativeOptions, MCTSOptions, MonteCarloTreeSearch, Strategy};

use crate::yinsh::{self, Action, GameState, TurnMode};

struct Yinsh;

impl minimax::Game for Yinsh {
    type S = yinsh::GameState;

    type M = yinsh::Action;

    fn generate_moves(state: &Self::S, moves: &mut Vec<Self::M>) {
        for m in possible_next_game_states(state) {
            moves.push(m);
        }
    }

    fn apply(state: &mut Self::S, m: Self::M) -> Option<Self::S> {
        let mut new_state = state.clone();
        new_state.transition(&m);
        Some(new_state) // TODO: we can avoid cloning here by returning None and implementing undo
    }

    fn get_winner(state: &Self::S) -> Option<minimax::Winner> {
        if state.points_a >= 3 {
            if state.active_player == yinsh::Player::A {
                Some(minimax::Winner::PlayerToMove)
            } else {
                Some(minimax::Winner::PlayerJustMoved)
            }
        } else if state.points_b >= 3 {
            if state.active_player == yinsh::Player::B {
                Some(minimax::Winner::PlayerToMove)
            } else {
                Some(minimax::Winner::PlayerJustMoved)
            }
        } else {
            None
        }
    }
}

struct MarkerCountHeuristic;

impl minimax::Evaluator for MarkerCountHeuristic {
    type G = Yinsh;

    fn evaluate(&self, state: &GameState) -> minimax::Evaluation {
        type Score = minimax::Evaluation;

        // Evaluate position from perspective of player B (AI). If the active
        // player is the human player, we negate in the end.
        let score_points = (state.points_b as Score) - (state.points_a as Score);

        let score_markers = (state.board.num_markers(yinsh::Player::B) as Score)
            - (state.board.num_markers(yinsh::Player::A) as Score);

        let score = 1000 * score_points + score_markers;

        if state.active_player == yinsh::Player::B {
            score
        } else {
            -score
        }
    }
}

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
        TurnMode::RunRemoval(_) => Box::new(
            state
                .board
                .run_coords(state.active_player)
                .into_iter()
                .map(Action::RemoveRun),
        ),
        TurnMode::RingRemoval(_) => Box::new(
            state
                .board
                .ring_coords(state.active_player)
                .map(Action::RemoveRing),
        ),
        TurnMode::RunRemovalFiller(_) | TurnMode::MarkerPlacementFiller => {
            Box::new(iter::once(Action::Wait))
        }
    }
}

pub fn get_ai_player_action(state: &GameState) -> Action {
    // let mut strategy = minimax::Negamax::new(MarkerCountHeuristic {}, 7);
    let mut strategy = minimax::Negamax::new(MarkerCountHeuristic {}, 7);
    strategy.choose_move(&state).unwrap()
}
