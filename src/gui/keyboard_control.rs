use bevy::ecs::message::MessageWriter;
use bevy::prelude::*;

use super::{
    ai::{AiComputationEvent, AiPlayerStrength, AiSet},
    board::BoardElement,
    board_update_event::BoardUpdateEvent,
    graphics::ScaleFactorSet,
    interaction::CursorElement,
    state_update::{GameState, StateUpdateSet, UndoHistory, restore_board_from_game_state},
};

fn keyboard_control(
    keyboard: Res<ButtonInput<KeyCode>>,
    mut commands: Commands,
    mut exit: MessageWriter<AppExit>,
    mut ai_player_strength: ResMut<AiPlayerStrength>,
    mut ai_computation_events: MessageWriter<AiComputationEvent>,
    mut board_update_events: MessageWriter<BoardUpdateEvent>,
    mut game_state: ResMut<GameState>,
    mut undo_history: ResMut<UndoHistory>,
    q_board_elements: Query<Entity, (With<BoardElement>, Without<CursorElement>)>,
) {
    let ctrl_pressed =
        keyboard.pressed(KeyCode::ControlLeft) || keyboard.pressed(KeyCode::ControlRight);

    if keyboard.just_pressed(KeyCode::Escape) || keyboard.just_pressed(KeyCode::KeyQ) {
        // Exit the application
        exit.write(AppExit::Success);
    } else if keyboard.just_pressed(KeyCode::PageUp) {
        // Increase AI strength
        ai_player_strength.0 += 1;
    } else if keyboard.just_pressed(KeyCode::PageDown) {
        // Decrease AI strength
        ai_player_strength.0 = (ai_player_strength.0 - 1).max(1);
    } else if keyboard.just_pressed(KeyCode::KeyU)
        || (ctrl_pressed && keyboard.just_pressed(KeyCode::KeyZ))
    {
        // Undo last human move
        if let Some(previous_state) = undo_history.0.pop() {
            ai_computation_events.write(AiComputationEvent::Cancel);
            *game_state.as_deref_mut() = previous_state;
            restore_board_from_game_state(
                &game_state,
                &mut commands,
                &mut board_update_events,
                &q_board_elements,
            );
        }
    } else if ctrl_pressed && keyboard.just_pressed(KeyCode::KeyR) {
        // Reset game
        ai_computation_events.write(AiComputationEvent::Cancel);
        *game_state.as_deref_mut() = yinsh::GameState::initial();
        undo_history.0.clear();
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
        keyboard_control
            .ambiguous_with(AiSet)
            .before(StateUpdateSet)
            .after(ScaleFactorSet),
    );
}
