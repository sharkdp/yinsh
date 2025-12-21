mod ai;
mod board;
mod board_update_event;
mod graphics;
mod grid;
#[cfg(not(target_arch = "wasm32"))]
mod history;
mod information_display;
mod interaction;
mod keyboard_control;
mod state_update;

use yinsh::Player;

pub const PLAYER_HUMAN: Player = Player::A;
pub const PLAYER_AI: Player = Player::B;

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
                    resolution: WindowResolution::new(860, 960),
                    mode: WindowMode::Windowed,
                    canvas: Some("#yinsh-canvas".into()),
                    ..default()
                }),
                ..default()
            }),
            state_update::plugin,
            ai::plugin,
            graphics::plugin,
            interaction::plugin,
            information_display::plugin,
            keyboard_control::plugin,
            #[cfg(not(target_arch = "wasm32"))]
            history::plugin,
        ))
        .run();
}
