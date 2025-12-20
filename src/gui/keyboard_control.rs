use bevy::prelude::*;

use super::{
    ai::{AiComputationEvent, AiPlayerStrength, AiSet},
    state_update::GameState,
    PLAYER_HUMAN,
};

fn keyboard_control(
    keyboard: Res<ButtonInput<KeyCode>>,
    mut exit: EventWriter<AppExit>,
    mut ai_player_strength: ResMut<AiPlayerStrength>,
    mut ai_computation_events: EventWriter<AiComputationEvent>,
    game_state: Res<GameState>,
) {
    if keyboard.just_pressed(KeyCode::Escape) || keyboard.just_pressed(KeyCode::KeyQ) {
        exit.send(AppExit::Success);
    } else if keyboard.just_pressed(KeyCode::KeyK) {
        ai_player_strength.0 += 1;
    } else if keyboard.just_pressed(KeyCode::KeyJ) {
        ai_player_strength.0 = (ai_player_strength.0 - 1).max(1);
    } else if keyboard.just_pressed(KeyCode::KeyA) {
        ai_computation_events.send(AiComputationEvent::Start(PLAYER_HUMAN, game_state.clone()));
        // TODO: remove this feature, or implement it properly
    }
}

pub fn plugin(app: &mut App) {
    app.add_systems(Update, keyboard_control.ambiguous_with(AiSet));
}
