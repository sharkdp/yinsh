use std::ops::{Deref, DerefMut};

use yinsh::{Action, Coord, Player, TurnMode};

use bevy::{prelude::*, tasks::AsyncComputeTaskPool};

use crate::gui::{graphics::ANIMATION_DURATION, PLAYER_AI, PLAYER_HUMAN};

use super::ai::{AiPlayerStrength, AiTask};

#[derive(Event)]
pub struct PlayerActionEvent(pub Player, pub Action);

#[derive(Resource)]
pub struct GameState(yinsh::GameState);

impl Deref for GameState {
    type Target = yinsh::GameState;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for GameState {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl GameState {
    pub fn initial() -> Self {
        Self(yinsh::GameState::initial())
    }
}

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

pub fn update_game_state(
    mut game_state: ResMut<GameState>,
    mut player_action_events: EventReader<PlayerActionEvent>,
    mut interaction_state: ResMut<InteractionState>,
    mut task: ResMut<AiTask>,
    ai_player_strength: Res<AiPlayerStrength>,
) {
    for PlayerActionEvent(player, action) in player_action_events.read() {
        assert!(player == &game_state.0.active_player);

        game_state.0.transition(action);

        if let Some(winner) = game_state.0.winner() {
            *interaction_state = InteractionState::Winner(winner);
            return;
        }

        if game_state.0.active_player == PLAYER_AI {
            let task_pool = AsyncComputeTaskPool::get();

            let game_state = game_state.0.clone();
            let search_depth = ai_player_strength.0;
            task.start(task_pool.spawn(async move {
                // TODO! This is a hack to make sure the AI takes at least as long as
                // the animation.
                if matches!(game_state.turn_mode, TurnMode::MarkerPlacement) {
                    std::thread::sleep(ANIMATION_DURATION);
                }

                yinsh::get_ai_player_action(search_depth, &game_state)
            }));

            *interaction_state = InteractionState::WaitForAI;
        } else {
            *interaction_state = InteractionState::from_turn_mode(&game_state.0);
        }
    }
}
