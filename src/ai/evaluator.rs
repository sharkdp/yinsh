use minimax::{Evaluation, Evaluator};

use crate::{GameState, Move, Player, TurnMode};

use super::game::Yinsh;

pub trait Heuristic {
    fn identifier(&self) -> String {
        "Unknown Heuristic".to_string()
    }

    fn evaluate_for_player_a(&self, state: &GameState) -> Evaluation;
}

pub struct YinshEvaluator<'a, H: Heuristic> {
    heuristic: &'a H,
}

impl<'a, H: Heuristic> YinshEvaluator<'a, H> {
    pub fn new(heuristic: &'a H) -> Self {
        Self { heuristic }
    }
}

impl<'a, H: Heuristic> Evaluator for YinshEvaluator<'a, H> {
    type G = Yinsh;

    fn evaluate(&self, state: &GameState) -> Evaluation {
        match state.turn_mode {
            TurnMode::WaitForRingMovement(_)
            | TurnMode::WaitForRunRemoval(_)
            | TurnMode::WaitForRingRemoval(_)
            | TurnMode::WaitForMarkerPlacement => {
                // Look one move ahead if we are in a waiting state.
                let mut state_copy = state.clone();
                state_copy.perform_move(&Move::Wait);
                return -self.evaluate(&state_copy);
            }
            TurnMode::RingPlacement
            | TurnMode::MarkerPlacement
            | TurnMode::RingMovement(_)
            | TurnMode::RunRemoval(_)
            | TurnMode::RingRemoval(_) => {}
        }

        let score = self.heuristic.evaluate_for_player_a(state);

        match state.active_player {
            Player::A => score,
            Player::B => -score,
        }
    }
}
