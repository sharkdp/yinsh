use std::iter;

use minimax::Strategy;

use crate::yinsh::{self, Action, GameState, TurnMode};

struct Yinsh;

impl minimax::Game for Yinsh {
    type S = yinsh::GameState;

    type M = yinsh::Action;

    fn generate_moves(state: &Self::S, moves: &mut Vec<Self::M>) {
        for a in possible_actions(state) {
            moves.push(a);
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

        assert!(state.active_player == yinsh::Player::A);

        // Evaluate position from perspective of player B (AI). If the active
        // player is the human player, we negate in the end.
        let score_points = (state.points_b as Score) - (state.points_a as Score);

        let score_markers = (state.board.num_markers(yinsh::Player::B) as Score)
            - (state.board.num_markers(yinsh::Player::A) as Score);

        let score = 1000 * score_points + 10 * score_markers;

        -score
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

// pub struct PlainNegamax<E: Evaluator> {
//     depth: u8,
//     root_value: Evaluation,
//     // All moves tied with the best valuation.
//     best_moves: Vec<<E::G as Game>::M>,
//     eval: E,
// }

// impl<E: Evaluator> PlainNegamax<E> {
//     pub fn new(eval: E, depth: u8) -> PlainNegamax<E> {
//         PlainNegamax {
//             depth: depth,
//             root_value: 0,
//             best_moves: Vec::new(),
//             eval,
//         }
//     }

//     fn negamax(&self, s: &mut <E::G as Game>::S, depth: u8) -> Evaluation
//     where
//         <<E as Evaluator>::G as Game>::M: Copy,
//     {
//         use std::cmp::max;

//         if let Some(winner) = E::G::get_winner(s) {
//             return winner.evaluate();
//         }
//         if depth == 0 {
//             return self.eval.evaluate(s);
//         }
//         let mut moves = Vec::new();
//         E::G::generate_moves(s, &mut moves);
//         let mut best = WORST_EVAL;
//         for &m in moves.iter() {
//             let mut new = E::G::apply(s, m).unwrap();
//             let value = -self.negamax(&mut new, depth - 1);
//             best = max(best, value);
//         }
//         best
//     }
// }

// impl<E: Evaluator> Strategy<E::G> for PlainNegamax<E>
// where
//     <E::G as Game>::S: Clone,
//     <E::G as Game>::M: Copy,
// {
//     fn choose_move(&mut self, s: &<E::G as Game>::S) -> Option<<E::G as Game>::M> {
//         let mut moves = Vec::new();
//         E::G::generate_moves(s, &mut moves);

//         self.best_moves.clear();
//         let mut best_value = WORST_EVAL;
//         let mut s = s.clone();
//         for &m in moves.iter() {
//             let mut new = E::G::apply(&mut s, m).unwrap();
//             let value = -self.negamax(&mut new, self.depth - 1);
//             if value == best_value {
//                 self.best_moves.push(m);
//             } else if value > best_value {
//                 best_value = value;
//                 self.best_moves.clear();
//                 self.best_moves.push(m);
//             }
//         }
//         self.root_value = best_value;
//         self.best_moves.first().map(|m| *m)
//     }
// }

pub fn get_ai_player_action(state: &GameState) -> Action {
    // Early return if the only thing we can do is wait. Would be great
    // if this could be handled by 'minimax' itself (if there is only one
    // possible mobe in best_move, return that immediately).
    match state.turn_mode {
        TurnMode::WaitForRunRemoval(_)
        | TurnMode::WaitForRingMovement(_)
        | TurnMode::WaitForRingRemoval(_)
        | TurnMode::WaitForMarkerPlacement => {
            return Action::Wait;
        }
        _ => {}
    }

    let depth = if matches!(state.turn_mode, TurnMode::RingPlacement) {
        1
    } else {
        17
    };

    // let mut strategy = PlainNegamax::new(MarkerCountHeuristic {}, depth);
    let mut strategy = minimax::Negamax::new(MarkerCountHeuristic {}, depth);
    assert!(state.active_player == yinsh::Player::B);
    strategy.choose_move(&state).unwrap()
}
