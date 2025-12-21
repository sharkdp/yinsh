use bevy::ecs::message::MessageWriter;
use bevy::prelude::*;
use yinsh::TurnMode;

use super::{
    ai::AiComputationEvent,
    board::BoardElement,
    board_update_event::BoardUpdateEvent,
    graphics::ScaleFactorSet,
    interaction::CursorElement,
    state_update::{GameState, StateUpdateSet, restore_board_from_game_state},
};

pub fn save_and_load_game_state(
    keyboard: Res<ButtonInput<KeyCode>>,
    mut commands: Commands,
    mut game_state: ResMut<GameState>,
    mut ai_computation_events: MessageWriter<AiComputationEvent>,
    mut board_update_events: MessageWriter<BoardUpdateEvent>,
    q_board_elements: Query<Entity, (With<BoardElement>, Without<CursorElement>)>,
) {
    let ctrl_pressed =
        keyboard.pressed(KeyCode::ControlLeft) || keyboard.pressed(KeyCode::ControlRight);

    let filename = "gamestate.yml";

    if ctrl_pressed && keyboard.just_pressed(KeyCode::KeyS) {
        if matches!(
            game_state.turn_mode,
            TurnMode::RingPlacement | TurnMode::MarkerPlacement
        ) {
            info!("Saving game state to {}", filename);
            game_state.save_to(filename);
        } else {
            warn!(
                "Cannot save game state in turn mode {:?}",
                game_state.turn_mode
            );
        }
    } else if ctrl_pressed && keyboard.just_pressed(KeyCode::KeyL) {
        info!("Loading game state from {}", filename);
        *game_state.as_deref_mut() = yinsh::GameState::load_from(filename);

        assert!(matches!(
            game_state.turn_mode,
            TurnMode::RingPlacement | TurnMode::MarkerPlacement
        ));

        ai_computation_events.write(AiComputationEvent::Cancel);
        restore_board_from_game_state(
            &game_state,
            &mut commands,
            &mut board_update_events,
            &q_board_elements,
        );
    }
}

pub fn plugin(app: &mut App) {
    app.add_systems(
        Update,
        save_and_load_game_state
            .before(StateUpdateSet)
            .after(ScaleFactorSet),
    );
}
