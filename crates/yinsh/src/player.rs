use serde::{Deserialize, Serialize};

/// Player types (white and black)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Player {
    A,
    B,
}

impl Player {
    pub fn next(self) -> Self {
        match self {
            Player::A => Player::B,
            Player::B => Player::A,
        }
    }

    pub fn flip(&mut self) {
        *self = self.next();
    }
}
