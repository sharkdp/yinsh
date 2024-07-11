mod gui;

use bevy::{
    prelude::*,
    window::{PresentMode, WindowMode},
};

use gui::{
    ai::{wait_for_ai_move, AiPlayerStrength, AiTask},
    graphics::{graphics_plugin, COLOR_BACKGROUND},
    grid::draw_grid,
    information_display::information_display_plugin,
    interaction::{interaction_plugin, CursorCoord},
    io::save_and_load_game_state,
    keyboard::keyboard_control,
    state::{update_game_state, GameState, InteractionState, PlayerActionEvent},
};

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(WindowPlugin {
                primary_window: Some(Window {
                    title: "Yinsh".into(),
                    name: Some("yinsh".into()),
                    resolution: (960., 960.).into(),
                    mode: WindowMode::Windowed,
                    present_mode: PresentMode::Immediate,
                    ..default()
                }),
                ..default()
            }),
            graphics_plugin,
            information_display_plugin,
            interaction_plugin,
        ))
        .add_systems(
            Update,
            (
                save_and_load_game_state,
                wait_for_ai_move,
                update_game_state,
                draw_grid,
                keyboard_control,
            )
                .chain(),
        )
        .insert_resource(ClearColor(COLOR_BACKGROUND))
        .insert_resource(Msaa::Sample8)
        .insert_resource(InteractionState::RingPlacement)
        .insert_resource(AiTask::new())
        .insert_resource(AiPlayerStrength(11))
        .insert_resource(CursorCoord(None))
        .insert_resource(GameState::initial())
        .add_event::<PlayerActionEvent>()
        .run();
}
