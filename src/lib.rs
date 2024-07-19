mod ai;
mod yinsh;

pub use ai::{get_ai_move, possible_moves, Heuristic, SimpleHeuristic, YinshAi, YinshAiPlayer};
pub use yinsh::*;
