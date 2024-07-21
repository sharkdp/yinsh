mod evaluator;
mod game;
mod heuristics;

pub use evaluator::Heuristic;
pub use game::possible_moves;
pub use heuristics::SimpleHeuristic;

use evaluator::YinshEvaluator;
use minimax::{Negamax, Strategy};

use crate::yinsh::{GameState, Move, TurnMode};

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

pub trait YinshAiPlayer: Sync {
    fn identifier(&self) -> String;

    fn search_depth(&self) -> usize;

    fn choose_move(&self, state: &GameState) -> Move;
}

impl<H: Heuristic + Sync> YinshAiPlayer for YinshAi<H> {
    fn identifier(&self) -> String {
        self.heuristic.identifier()
    }

    fn search_depth(&self) -> usize {
        self.search_depth
    }

    fn choose_move(&self, state: &GameState) -> Move {
        // Early return if the only thing we can do is wait. Would be great
        // if this could be handled by 'minimax' itself (if there is only one
        // possible move in choose_move, return that immediately).
        match state.turn_mode {
            TurnMode::WaitForRunRemoval(_)
            | TurnMode::WaitForRingMovement(_)
            | TurnMode::WaitForRingRemoval(_)
            | TurnMode::WaitForMarkerPlacement => {
                return Move::Wait;
            }
            _ => {}
        }

        let depth: u8 = if matches!(state.turn_mode, TurnMode::RingPlacement) {
            3
        } else {
            self.search_depth.try_into().unwrap()
        };

        let mut strategy = Negamax::new(YinshEvaluator::new(&self.heuristic), depth);
        let player_move = strategy.choose_move(&state).unwrap();

        // dbg!(strategy.root_value());

        player_move
    }
}

pub fn get_ai_move(search_depth: usize, state: &GameState) -> Move {
    YinshAi::new(SimpleHeuristic::default(), search_depth).choose_move(state)
}
