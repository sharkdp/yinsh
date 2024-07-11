use bevy::prelude::*;

use super::ai::AiPlayerStrength;

pub fn keyboard_control(
    keyboard: Res<ButtonInput<KeyCode>>,
    mut exit: EventWriter<AppExit>,
    mut ai_player_strength: ResMut<AiPlayerStrength>,
) {
    if keyboard.just_pressed(KeyCode::Escape) || keyboard.just_pressed(KeyCode::KeyQ) {
        exit.send(AppExit::Success);
    } else if keyboard.just_pressed(KeyCode::KeyK) {
        ai_player_strength.0 = ai_player_strength.0 + 1;
    } else if keyboard.just_pressed(KeyCode::KeyJ) {
        ai_player_strength.0 = (ai_player_strength.0 - 1).max(1);
    }
}
