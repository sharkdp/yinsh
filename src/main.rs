mod gui;

use bevy::{
    ecs::schedule::{LogLevel, ScheduleBuildSettings},
    prelude::*,
    window::WindowMode,
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
                    ..default()
                }),
                ..default()
            }),
            gui::state_update::plugin,
            gui::ai::plugin,
            gui::graphics::plugin,
            gui::interaction::plugin,
            gui::information_display::plugin,
            gui::keyboard_control::plugin,
            gui::history::plugin,
        ))
        .edit_schedule(Update, |schedule| {
            schedule.set_build_settings(ScheduleBuildSettings {
                ambiguity_detection: LogLevel::Warn,
                ..default()
            });
        })
        .run();
}
