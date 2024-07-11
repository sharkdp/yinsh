use yinsh::{Coord, Player, TurnMode};

use bevy::prelude::*;

use crate::gui::PLAYER_HUMAN;

#[derive(Resource)]
pub enum InteractionState {
    RingPlacement,
    MarkerPlacement,
    RingMovement(Coord),
    RunRemoval { run_coords: Vec<Coord> },
    RingRemoval,
    AutoMove,
    WaitForAI,
    Winner(Player),
}

impl InteractionState {
    pub fn from_turn_mode(game_state: &yinsh::GameState) -> Self {
        assert!(game_state.active_player == PLAYER_HUMAN);

        match game_state.turn_mode {
            TurnMode::RingPlacement => Self::RingPlacement,
            TurnMode::MarkerPlacement => Self::MarkerPlacement,
            TurnMode::RingMovement(start) => Self::RingMovement(start),
            TurnMode::RunRemoval(_) => Self::RunRemoval {
                run_coords: game_state.board.run_coords(PLAYER_HUMAN),
            },
            TurnMode::RingRemoval(_) => Self::RingRemoval,
            TurnMode::WaitForRunRemoval(_)
            | TurnMode::WaitForMarkerPlacement
            | TurnMode::WaitForRingMovement(_)
            | TurnMode::WaitForRingRemoval(_) => Self::AutoMove,
        }
    }
}
