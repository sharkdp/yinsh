mod gui;

use bevy::{
    prelude::*,
    window::{WindowMode, WindowResolution},
};

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(WindowPlugin {
                primary_window: Some(Window {
                    title: "Yinsh".into(),
                    name: Some("yinsh".into()),
                    resolution: WindowResolution::new(760, 860),
                    mode: WindowMode::Windowed,
                    canvas: Some("#yinsh-canvas".into()),
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
            #[cfg(not(target_arch = "wasm32"))]
            gui::history::plugin,
        ))
        // .edit_schedule(Update, |schedule| {
        //     schedule.set_build_settings(ScheduleBuildSettings {
        //         ambiguity_detection: LogLevel::Warn,
        //         ..default()
        //     });
        // })
        .run();
}
