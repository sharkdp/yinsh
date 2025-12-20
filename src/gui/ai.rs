use std::task::Poll;

use bevy::prelude::*;

use bevy_async_task::TaskRunner;
use yinsh::{GameState, Move, Player};
use bevy::ecs::message::{MessageReader, MessageWriter};

use super::state_update::{PlayerMoveEvent, StateUpdateSet};

#[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
pub struct AiSet;

#[derive(Resource)]
pub struct AiPlayerStrength(pub usize);

#[derive(Message)]
pub enum AiComputationEvent {
    Start(Player, GameState),

    #[allow(unused)]
    Cancel,
}

fn perform_ai_moves(
    mut task_runner: TaskRunner<Option<(Player, Move)>>,
    mut events: MessageReader<AiComputationEvent>,
    strength: Res<AiPlayerStrength>,
    mut player_move_events: MessageWriter<PlayerMoveEvent>,
) {
    for event in events.read() {
        match event {
            AiComputationEvent::Start(player, game_state) => {
                let player = *player;
                let game_state = game_state.clone();
                let search_depth = strength.0;
                task_runner.start(async move {
                    use super::graphics::ANIMATION_DURATION;
                    use yinsh::TurnMode;

                    // Make sure the AI takes at least as long as the animation
                    if matches!(game_state.turn_mode, TurnMode::MarkerPlacement) {
                        #[cfg(not(target_arch = "wasm32"))]
                        std::thread::sleep(ANIMATION_DURATION);

                        #[cfg(target_arch = "wasm32")]
                        gloo_timers::future::TimeoutFuture::new(
                            ANIMATION_DURATION.as_millis() as u32,
                        )
                        .await;
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
        Poll::Pending => {}
        Poll::Ready(None) => {}
        Poll::Ready(Some((player, player_move))) => {
            player_move_events.write(PlayerMoveEvent(player, player_move));
        }
    }
}

pub fn plugin(app: &mut App) {
    app.insert_resource(AiPlayerStrength(if cfg!(debug_assertions) {
        6
    } else {
        12
    }))
    .add_message::<AiComputationEvent>()
    .add_systems(
        Update,
        (perform_ai_moves).in_set(AiSet).after(StateUpdateSet),
    );
}
