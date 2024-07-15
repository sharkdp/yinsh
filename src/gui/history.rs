use bevy::prelude::*;
use yinsh::{Player, TurnMode};

use super::{
    ai::AiComputationEvent,
    board::BoardElement,
    board_update_event::BoardUpdateEvent,
    graphics::ScaleFactorSet,
    interaction::CursorElement,
    state_update::{GameState, StateUpdateSet},
};

pub fn save_and_load_game_state(
    keyboard: Res<ButtonInput<KeyCode>>,
    mut commands: Commands,
    mut game_state: ResMut<GameState>,
    mut ai_computation_events: EventWriter<AiComputationEvent>,
    mut board_update_events: EventWriter<BoardUpdateEvent>,
    q_board_elements: Query<Entity, (With<BoardElement>, Without<CursorElement>)>,
) {
    let filename = "gamestate.yml";

    if keyboard.just_pressed(KeyCode::KeyS) {
        println!("Saving game state to {}", filename);
        if matches!(
            game_state.turn_mode,
            TurnMode::RingPlacement | TurnMode::MarkerPlacement
        ) {
            game_state.save_to(filename);
        } else {
            println!(
                "Cannot save game state in turn mode {:?}",
                game_state.turn_mode
            );
        }
    } else if keyboard.just_pressed(KeyCode::KeyL) || keyboard.just_pressed(KeyCode::KeyR) {
        println!("Loading game state from {}", filename);
        *game_state.as_deref_mut() = yinsh::GameState::load_from(filename);

        assert!(matches!(
            game_state.turn_mode,
            TurnMode::RingPlacement | TurnMode::MarkerPlacement
        ));

        ai_computation_events.send(AiComputationEvent::Cancel);

        // Despawn all board elements
        for entity in q_board_elements.iter() {
            commands.entity(entity).despawn();
        }

        // Respawn board elements
        for p in [Player::A, Player::B] {
            for coord in game_state.board.ring_coords(p) {
                board_update_events.send(BoardUpdateEvent::AddRing(coord, p));
            }

            for coord in game_state.board.marker_coords(p) {
                board_update_events.send(BoardUpdateEvent::AddMarker(coord, p));
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
