use yinsh::Player;

pub mod ai;
pub mod board;
pub mod graphics;
pub mod grid;
pub mod information_display;
pub mod io;
pub mod keyboard;
pub mod resources;
pub mod state;

pub const PLAYER_HUMAN: Player = Player::A;
pub const PLAYER_AI: Player = Player::B;
