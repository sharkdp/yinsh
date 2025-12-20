use bevy::prelude::*;

use bevy_async_task::{AsyncTaskRunner, AsyncTaskStatus};
use yinsh::{GameState, Move, Player};

use super::state_update::{PlayerMoveEvent, StateUpdateSet};

#[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
pub struct AiSet;

#[derive(Resource)]
pub struct AiPlayerStrength(pub usize);

#[derive(Event)]
pub enum AiComputationEvent {
    Start(Player, GameState),

    #[allow(unused)]
    Cancel,
}

fn perform_ai_moves(
    mut task_runner: AsyncTaskRunner<Option<(Player, Move)>>,
    mut events: EventReader<AiComputationEvent>,
    strength: Res<AiPlayerStrength>,
    mut player_move_events: EventWriter<PlayerMoveEvent>,
) {
    for event in events.read() {
        match event {
            AiComputationEvent::Start(player, game_state) => {
                let player = *player;
                let game_state = game_state.clone();
                let search_depth = strength.0;
                task_runner.start(async move {
                    // TODO! This is a hack to make sure the AI takes at least as long as
                    // the animation.
                    #[cfg(not(target_arch = "wasm32"))]
                    {
                        use super::graphics::ANIMATION_DURATION;
                        use yinsh::TurnMode;

                        if matches!(game_state.turn_mode, TurnMode::MarkerPlacement) {
                            std::thread::sleep(ANIMATION_DURATION);
                        }
                    }

                    Some((player, yinsh::get_ai_move(search_depth, &game_state)))
                });
            }
            AiComputationEvent::Cancel => {
                // Replace current computation with dummy task
                task_runner.start(async move { None });
            }
        }
    }

    match task_runner.poll() {
        AsyncTaskStatus::Idle | AsyncTaskStatus::Pending | AsyncTaskStatus::Finished(None) => {}
        AsyncTaskStatus::Finished(Some((player, player_move))) => {
            player_move_events.send(PlayerMoveEvent(player, player_move));
        }
    }
}

pub fn plugin(app: &mut App) {
    app.insert_resource(AiPlayerStrength(if cfg!(debug_assertions) {
        9
    } else {
        15
    }))
    .add_event::<AiComputationEvent>()
    .add_systems(
        Update,
        (perform_ai_moves).in_set(AiSet).after(StateUpdateSet),
    );
}
