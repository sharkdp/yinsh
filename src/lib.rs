mod ai;
mod yinsh;

pub use ai::{
    get_ai_player_action, possible_actions, Heuristic, SimpleHeuristic, YinshAi, YinshAiPlayer,
};
pub use yinsh::*;
