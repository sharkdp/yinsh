mod evaluator;
mod game;
mod heuristics;

pub use evaluator::Heuristic;
pub use game::possible_actions;
pub use heuristics::SimpleHeuristic;

use evaluator::YinshEvaluator;
use minimax::{Negamax, Strategy};

use crate::yinsh::{Action, GameState, TurnMode};

pub struct YinshAi<H: Heuristic> {
    heuristic: H,
    search_depth: usize,
}

impl<H: Heuristic> YinshAi<H> {
    pub fn new(heuristic: H, search_depth: usize) -> Self {
        Self {
            heuristic,
            search_depth,
        }
    }
}

pub trait YinshAiPlayer {
    fn identifier(&self) -> String;

    fn choose_action(&self, state: &GameState) -> Action;
}

impl<H: Heuristic> YinshAiPlayer for YinshAi<H> {
    fn identifier(&self) -> String {
        self.heuristic.identifier()
    }

    fn choose_action(&self, state: &GameState) -> Action {
        // Early return if the only thing we can do is wait. Would be great
        // if this could be handled by 'minimax' itself (if there is only one
        // possible move in choose_move, return that immediately).
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
            self.search_depth.try_into().unwrap()
        };

        let mut strategy = Negamax::new(YinshEvaluator::new(&self.heuristic), depth);
        let action = strategy.choose_move(&state).unwrap();

        // dbg!(strategy.root_value());

        action
    }
}

pub fn get_ai_player_action(search_depth: usize, state: &GameState) -> Action {
    YinshAi::new(SimpleHeuristic::default(), search_depth).choose_action(state)
}
