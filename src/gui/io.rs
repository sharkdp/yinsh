use bevy::prelude::*;
use yinsh::Player;

use crate::gui::{
    graphics::{spawn_marker, spawn_ring},
    state::InteractionState,
};

use super::{
    ai::AiTask, board::BoardElement, graphics::PlayerColors, interaction::CursorElement,
    state::GameState,
};

pub fn save_and_load_game_state(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    player_colors: Res<PlayerColors>,
    keyboard: Res<ButtonInput<KeyCode>>,
    mut game_state: ResMut<GameState>,
    mut interaction_state: ResMut<InteractionState>,
    mut ai_task: ResMut<AiTask>,
    q_board_elements: Query<Entity, (With<BoardElement>, Without<CursorElement>)>,
) {
    let filename = "gamestate.yml";

    if keyboard.just_pressed(KeyCode::KeyS) {
        println!("Saving game state to {}", filename);
        game_state.save_to(filename);
    } else if keyboard.just_pressed(KeyCode::KeyL) || keyboard.just_pressed(KeyCode::KeyR) {
        println!("Loading game state from {}", filename);
        *game_state.as_deref_mut() = yinsh::GameState::load_from(filename);

        *interaction_state = InteractionState::from_turn_mode(&game_state);
        ai_task.cancel();

        // Despawn all board elements
        for entity in q_board_elements.iter() {
            commands.entity(entity).despawn();
        }

        // Respawn board elements
        for p in [Player::A, Player::B] {
            for coord in game_state.board.ring_coords(p) {
                spawn_ring(&mut commands, &mut meshes, &player_colors, coord, p);
            }

            for coord in game_state.board.marker_coords(p) {
                spawn_marker(&mut commands, &mut meshes, &player_colors, coord, p);
            }
        }
    }
}
