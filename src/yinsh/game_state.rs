use std::path::Path;

use serde::{Deserialize, Serialize};
use tracing::trace;

use super::{Board, Coord, Player};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TurnMode {
    /// place a ring on a free field
    RingPlacement,

    /// place a marker in one of your rings
    MarkerPlacement,

    /// wait for ring movement
    WaitForRingMovement(Coord),

    /// move the ring at the given position
    RingMovement(Coord),

    /// Remove (one of your) run(s). The parameter
    /// holds the last player who moved a ring.
    RunRemoval(Player),

    /// Wait for run removal
    WaitForRunRemoval(Player),

    /// Remove one of your rings
    RingRemoval(Player),

    WaitForRingRemoval(Player),

    /// Do nothing
    WaitForMarkerPlacement,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Move {
    PlaceRing(Coord),
    PlaceMarker(Coord),
    MoveRing(Coord, Coord),
    RemoveRun(Coord),
    RemoveRing(Coord),
    Wait,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GameState {
    pub active_player: Player,
    pub turn_mode: TurnMode,
    pub board: Board,
    pub points_a: usize,
    pub points_b: usize,
}

impl GameState {
    pub fn initial() -> Self {
        Self {
            active_player: Player::A,
            turn_mode: TurnMode::RingPlacement,
            board: Board::empty(),
            points_a: 0,
            points_b: 0,
        }
    }

    pub fn perform_move(&mut self, player_move: &Move) {
        trace!(
            turn_mode = ?self.turn_mode,
            active_player = ?self.active_player,
            player_move = ?player_move,
            "Performing move"
        );

        match (&self.turn_mode, player_move) {
            (TurnMode::RingPlacement, Move::PlaceRing(coord)) => {
                self.board.add_ring(self.active_player, *coord);

                self.turn_mode = if self.board.num_rings() <= 9 {
                    TurnMode::RingPlacement
                } else {
                    TurnMode::MarkerPlacement
                };
            }
            (TurnMode::MarkerPlacement, Move::PlaceMarker(coord)) => {
                self.board.remove_ring(*coord);
                self.board.add_marker(self.active_player, *coord);

                self.turn_mode = TurnMode::WaitForRingMovement(*coord);
            }
            (TurnMode::WaitForRingMovement(start), Move::Wait) => {
                self.turn_mode = TurnMode::RingMovement(*start);
            }
            (TurnMode::RingMovement(_), Move::MoveRing(start, end)) => {
                self.board.add_ring(self.active_player, *end);

                self.board.flip_markers_between(*start, *end);

                let result = self.board.check_run();

                if result.has_run(self.active_player) {
                    self.turn_mode = TurnMode::WaitForRunRemoval(self.active_player);
                } else if result.has_run(self.active_player.next()) {
                    self.turn_mode = TurnMode::RunRemoval(self.active_player);
                } else {
                    self.turn_mode = TurnMode::MarkerPlacement;
                }
            }
            (TurnMode::WaitForRunRemoval(player_last_ring_move), Move::Wait) => {
                self.turn_mode = TurnMode::RunRemoval(*player_last_ring_move);
            }
            (TurnMode::RunRemoval(player_last_ring_move), Move::RemoveRun(coord)) => {
                self.board.remove_run(*coord);

                if self.active_player == Player::A {
                    self.points_a += 1;
                } else {
                    self.points_b += 1;
                }

                self.turn_mode = TurnMode::WaitForRingRemoval(*player_last_ring_move);
            }
            (TurnMode::WaitForRingRemoval(player_last_ring_move), Move::Wait) => {
                self.turn_mode = TurnMode::RingRemoval(*player_last_ring_move);
            }
            (TurnMode::RingRemoval(player_last_ring_move), Move::RemoveRing(coord)) => {
                self.board.remove_ring(*coord);

                let result = self.board.check_run();

                self.turn_mode = if result.has_run(self.active_player) {
                    // Active player has a second run, other player needs to wait
                    TurnMode::WaitForRunRemoval(*player_last_ring_move)
                } else if result.has_run(self.active_player.next()) {
                    // Other player has a run, active player needs to remove it
                    TurnMode::RunRemoval(*player_last_ring_move)
                } else if self.active_player == *player_last_ring_move {
                    TurnMode::MarkerPlacement
                } else {
                    TurnMode::WaitForMarkerPlacement
                };
            }
            (TurnMode::WaitForMarkerPlacement, Move::Wait) => {
                self.turn_mode = TurnMode::MarkerPlacement;
            }
            (turn_mode, player_move) => {
                unreachable!(
                    "Received unexpected player move {player_move:?} in mode {turn_mode:?}"
                )
            }
        }

        self.active_player.flip();

        trace!(
            new_turn_mode = ?self.turn_mode,
            new_active_player = ?self.active_player,
            "Move completed"
        );
    }

    pub fn winner(&self) -> Option<Player> {
        if self.points_a >= 3 {
            Some(Player::A)
        } else if self.points_b >= 3 {
            Some(Player::B)
        } else {
            None
        }
    }

    pub fn save_to<P: AsRef<Path>>(&self, path: P) {
        let file = std::fs::File::create(path).unwrap();
        serde_yaml::to_writer(file, &self).unwrap();
    }

    pub fn load_from<P: AsRef<Path>>(path: P) -> Self {
        let file = std::fs::File::open(path).unwrap();
        let mut game_state: GameState = serde_yaml::from_reader(file).unwrap();

        // Patch up the 2D board, which is not serialized/deserialized
        game_state.board.fill_board_from_lists();

        game_state
    }
}
