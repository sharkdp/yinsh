use bevy::prelude::*;

use super::{
    graphics::{screen_point, COLOR_RING_MOVEMENT_INDICATOR, SPACING},
    state::InteractionState,
};

fn draw_ring_move_indicators(mut gizmos: Gizmos, interaction_state: Res<InteractionState>) {
    let indicator_color = COLOR_RING_MOVEMENT_INDICATOR;

    if let InteractionState::RingMovement(_, ref possible_moves) = *interaction_state {
        for coord in possible_moves {
            let screen_pos = screen_point(*coord);
            gizmos.circle(screen_pos, Dir3::Z, SPACING / 8., indicator_color);
        }
    }
}

pub fn interaction_plugin(app: &mut App) {
    app.add_systems(Update, draw_ring_move_indicators);
}
