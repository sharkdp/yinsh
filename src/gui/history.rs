use bevy::prelude::*;
use yinsh::Player;

use crate::gui::graphics::{spawn_marker, spawn_ring};

use super::{
    ai::AiComputationEvent,
    board::BoardElement,
    graphics::{PlayerColors, ScaleFactorSet},
    interaction::CursorElement,
    state_update::{GameState, StateUpdateSet},
};

pub fn save_and_load_game_state(
    keyboard: Res<ButtonInput<KeyCode>>,
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    player_colors: Res<PlayerColors>,
    mut game_state: ResMut<GameState>,
    mut ai_computation_events: EventWriter<AiComputationEvent>,
    q_board_elements: Query<Entity, (With<BoardElement>, Without<CursorElement>)>,
) {
    let filename = "gamestate.yml";

    if keyboard.just_pressed(KeyCode::KeyS) {
        println!("Saving game state to {}", filename);
        game_state.save_to(filename);
    } else if keyboard.just_pressed(KeyCode::KeyL) || keyboard.just_pressed(KeyCode::KeyR) {
        println!("Loading game state from {}", filename);
        *game_state.as_deref_mut() = yinsh::GameState::load_from(filename);

        ai_computation_events.send(AiComputationEvent::Cancel);

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

pub fn plugin(app: &mut App) {
    app.add_systems(
        Update,
        save_and_load_game_state
            .before(StateUpdateSet)
            .after(ScaleFactorSet),
    );
}
