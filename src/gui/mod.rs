use yinsh::Player;

pub mod ai;
pub mod board;
pub mod board_update_event;
pub mod graphics;
pub mod grid;
pub mod history;
pub mod information_display;
pub mod interaction;
pub mod keyboard_control;
pub mod resources;
pub mod state_update;

pub const PLAYER_HUMAN: Player = Player::A;
pub const PLAYER_AI: Player = Player::B;
