use bevy::prelude::*;
use bevy::ecs::message::MessageWriter;

use super::{
    ai::{AiComputationEvent, AiPlayerStrength, AiSet},
    board::BoardElement,
    board_update_event::BoardUpdateEvent,
    interaction::CursorElement,
    state_update::{restore_board_from_game_state, GameState, UndoHistory},
    PLAYER_HUMAN,
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
    if keyboard.just_pressed(KeyCode::Escape) || keyboard.just_pressed(KeyCode::KeyQ) {
        exit.write(AppExit::Success);
    } else if keyboard.just_pressed(KeyCode::KeyK) {
        ai_player_strength.0 += 1;
    } else if keyboard.just_pressed(KeyCode::KeyJ) {
        ai_player_strength.0 = (ai_player_strength.0 - 1).max(1);
    } else if keyboard.just_pressed(KeyCode::KeyA) {
        ai_computation_events.write(AiComputationEvent::Start(PLAYER_HUMAN, game_state.clone()));
        // TODO: remove this feature, or implement it properly
    } else if keyboard.just_pressed(KeyCode::KeyU)
        || (keyboard.just_pressed(KeyCode::KeyZ)
            && (keyboard.pressed(KeyCode::ControlLeft) || keyboard.pressed(KeyCode::ControlRight)))
    {
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
    } else if keyboard.just_pressed(KeyCode::KeyR)
        && (keyboard.pressed(KeyCode::ControlLeft) || keyboard.pressed(KeyCode::ControlRight))
    {
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
    app.add_systems(Update, keyboard_control.ambiguous_with(AiSet));
}
