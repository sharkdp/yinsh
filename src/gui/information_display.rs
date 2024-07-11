use bevy::prelude::*;

use yinsh::Player;

use super::{
    ai::AiPlayerStrength,
    graphics::BACKGROUND_RENDER_LAYER,
    interaction::CursorCoord,
    state_update::{GameState, InteractionState},
};

#[derive(Component)]
struct GameStateInformation;

fn setup_information_display(mut commands: Commands) {
    commands.spawn((
        TextBundle::from_section(
            "",
            TextStyle {
                font_size: 18.0,
                color: Color::hsl(0., 0., 0.3),
                ..default()
            },
        )
        .with_style(Style {
            position_type: PositionType::Absolute,
            top: Val::Px(10.),
            left: Val::Px(10.),
            ..default()
        }),
        GameStateInformation,
        BACKGROUND_RENDER_LAYER,
    ));
}

fn update_information_display(
    game_state: Res<GameState>,
    mut q_text: Query<&mut Text, With<GameStateInformation>>,
    cursor_coord: Res<CursorCoord>,
    interaction_state: Res<InteractionState>,
    ai_player_strength: Res<AiPlayerStrength>,
) {
    q_text.single_mut().sections[0].value =
        format!(
        "Score: {points_a}:{points_b}\nMode: {mode}\nAI strength: {strength} [weaker: J, stronger: K]\n{coord}",
        points_a=game_state.points_a,
        points_b=game_state.points_b,
        mode=match *interaction_state {
            InteractionState::RingPlacement(_) => "Place a ring on the board",
            InteractionState::MarkerPlacement(_) => "Place a marker in one of your rings",
            InteractionState::RingMovement(_, _) => "Move the selected ring",
            InteractionState::RunRemoval { .. } => "Select a run of five markers to remove",
            InteractionState::RingRemoval(_) => "Select one of your rings to remove it",
            InteractionState::AutoMove | InteractionState::WaitForAI => "Floyd is thinking...",
            InteractionState::Winner(Player::A) => "Game over. You win!",
            InteractionState::Winner(Player::B) => "Game over. Floyd wins!",
        },
        strength=ai_player_strength.0,
        coord=if let Some (coord) = cursor_coord.0 { format!("({x}, {y})", x=coord.x, y=coord.y) } else { "".to_string() },
    );
}

pub fn plugin(app: &mut App) {
    app.add_systems(Startup, setup_information_display)
        .add_systems(Update, update_information_display.ambiguous_with_all());
}
