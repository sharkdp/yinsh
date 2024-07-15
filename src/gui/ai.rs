use bevy::prelude::*;

use bevy_async_task::{AsyncTaskRunner, AsyncTaskStatus};
use yinsh::{Action, GameState, Player};

use super::state_update::{PlayerActionEvent, StateUpdateSet};

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

fn perform_ai_actions(
    mut task_runner: AsyncTaskRunner<Option<(Player, Action)>>,
    mut events: EventReader<AiComputationEvent>,
    strength: Res<AiPlayerStrength>,
    mut player_action_events: EventWriter<PlayerActionEvent>,
) {
    for event in events.read() {
        match event {
            AiComputationEvent::Start(player, ref game_state) => {
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

                    Some((
                        player,
                        yinsh::get_ai_player_action(search_depth, &game_state),
                    ))
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
        AsyncTaskStatus::Finished(Some((player, action))) => {
            player_action_events.send(PlayerActionEvent(player, action));
        }
    }
}

pub fn plugin(app: &mut App) {
    app.insert_resource(AiPlayerStrength(9))
        .add_event::<AiComputationEvent>()
        .add_systems(
            Update,
            (perform_ai_actions).in_set(AiSet).after(StateUpdateSet),
        );
}
