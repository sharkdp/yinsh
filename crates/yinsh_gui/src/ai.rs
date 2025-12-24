use std::task::Poll;

use bevy::prelude::*;

use bevy::ecs::message::{MessageReader, MessageWriter};
use bevy_async_task::TaskRunner;
use yinsh::{GameState, Move, Player};
use yinsh_ai::{SimpleHeuristic, YinshAi, YinshAiPlayer};
use yinsh_nn::NNHeuristic;

use crate::state_update::{PlayerMoveEvent, StateUpdateSet};

#[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
pub struct AiSet;

#[derive(Resource)]
pub struct AiPlayerStrength(pub usize);

/// Which heuristic the AI uses
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum AiHeuristicType {
    #[default]
    Simple,
    NeuralNetwork,
}

#[derive(Resource)]
pub struct AiHeuristic(pub AiHeuristicType);

#[derive(Message)]
pub enum AiComputationEvent {
    Start(Player, GameState),

    #[allow(unused)]
    Cancel,
}

fn get_ai_move_with_heuristic(
    search_depth: usize,
    state: &GameState,
    heuristic_type: AiHeuristicType,
) -> Move {
    match heuristic_type {
        AiHeuristicType::Simple => {
            YinshAi::new(SimpleHeuristic::default(), search_depth).choose_move(state)
        }
        AiHeuristicType::NeuralNetwork => {
            let nn_heuristic = NNHeuristic::load("crates/yinsh_nn/model.bin", 10_000)
                .unwrap_or_else(|_| {
                    tracing::warn!("Could not load model.bin, using untrained network");
                    NNHeuristic::new_untrained(10_000)
                });
            YinshAi::new(nn_heuristic, search_depth).choose_move(state)
        }
    }
}

fn perform_ai_moves(
    mut task_runner: TaskRunner<Option<(Player, Move)>>,
    mut events: MessageReader<AiComputationEvent>,
    strength: Res<AiPlayerStrength>,
    heuristic: Res<AiHeuristic>,
    mut player_move_events: MessageWriter<PlayerMoveEvent>,
) {
    for event in events.read() {
        match event {
            AiComputationEvent::Start(player, game_state) => {
                let player = *player;
                let game_state = game_state.clone();
                let search_depth = strength.0;
                let heuristic_type = heuristic.0;
                task_runner.start(async move {
                    use crate::graphics::ANIMATION_DURATION;
                    use yinsh::TurnMode;

                    // Make sure the AI takes at least as long as the animation
                    if matches!(game_state.turn_mode, TurnMode::MarkerPlacement) {
                        #[cfg(not(target_arch = "wasm32"))]
                        std::thread::sleep(ANIMATION_DURATION);

                        #[cfg(target_arch = "wasm32")]
                        gloo_timers::future::TimeoutFuture::new(
                            ANIMATION_DURATION.as_millis() as u32
                        )
                        .await;
                    }

                    Some((
                        player,
                        get_ai_move_with_heuristic(search_depth, &game_state, heuristic_type),
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
        Poll::Pending => {}
        Poll::Ready(None) => {}
        Poll::Ready(Some((player, player_move))) => {
            player_move_events.write(PlayerMoveEvent(player, player_move));
        }
    }
}

pub fn plugin(app: &mut App) {
    #[cfg(target_arch = "wasm32")]
    let default_strength = 10;
    #[cfg(not(target_arch = "wasm32"))]
    let default_strength = if cfg!(debug_assertions) { 6 } else { 12 };

    // Check YINSH_AI env var: "nn" for neural network, anything else for simple
    let heuristic_type = std::env::var("YINSH_AI")
        .map(|v| {
            if v.eq_ignore_ascii_case("nn") {
                tracing::info!("Using neural network heuristic");
                AiHeuristicType::NeuralNetwork
            } else {
                tracing::info!("Using simple heuristic");
                AiHeuristicType::Simple
            }
        })
        .unwrap_or_else(|_| {
            tracing::info!("YINSH_AI not set, using simple heuristic");
            AiHeuristicType::Simple
        });

    app.insert_resource(AiPlayerStrength(default_strength))
        .insert_resource(AiHeuristic(heuristic_type))
        .add_message::<AiComputationEvent>()
        .add_systems(
            Update,
            (perform_ai_moves).in_set(AiSet).after(StateUpdateSet),
        );
}
