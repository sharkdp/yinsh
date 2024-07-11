use super::{Board, Coord, Player};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TurnMode {
    /// place a ring on a free field
    RingPlacement,

    /// place a marker in one of your rings
    MarkerPlacement,

    /// move the ring at the given position
    RingMovement(Coord),

    /// Remove (one of your) run(s). The parameter
    /// holds the last player who moved a ring.
    RunRemoval(Player),

    /// Remove one of your rings
    RingRemoval(Player),

    /// Do nothing
    RunRemovalFiller(Player),

    /// Do nothing
    MarkerPlacementFiller,
}

#[derive(Debug, Clone)]
pub enum Action {
    PlaceRing(Coord),
    PlaceMarker(Coord),
    MoveRing(Coord, Coord),
    RemoveRun(Coord),
    RemoveRing(Coord),
    Wait,
}

#[derive(Debug, Clone)]
pub struct GameState {
    pub active_player: Player,
    pub turn_mode: TurnMode,
    pub board: Board,
    points_a: usize,
    points_b: usize,
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

    pub fn transition(&mut self, action: &Action) {
        match (&self.turn_mode, action) {
            (TurnMode::RingPlacement, Action::PlaceRing(coord)) => {
                self.board.add_ring(self.active_player, *coord);

                self.turn_mode = if self.board.num_rings() <= 9 {
                    TurnMode::RingPlacement
                } else {
                    TurnMode::MarkerPlacement
                };

                self.active_player = self.active_player.next();
            }
            (TurnMode::MarkerPlacement, Action::PlaceMarker(coord)) => {
                self.board.remove_ring(*coord);
                self.board.add_marker(self.active_player, *coord);

                self.turn_mode = TurnMode::RingMovement(*coord);
            }
            (TurnMode::RingMovement(_), Action::MoveRing(start, end)) => {
                self.board.remove_ring(*start);
                self.board.add_ring(self.active_player, *end);

                self.board.flip_markers_between(*start, *end);

                // TODO
                // self.turn_mode = if self.board.has_run(self.active_player) {
                //     TurnMode::RunRemovalFiller(self.active_player)
                // } else if self.board.has_run(self.active_player.next()) {
                //     TurnMode::RunRemoval(self.active_player)
                // } else {
                //     TurnMode::MarkerPlacement
                // };
                self.turn_mode = TurnMode::MarkerPlacement;

                self.active_player = self.active_player.next();
            }
            (TurnMode::RunRemoval(_), _) => todo!(),
            (TurnMode::RingRemoval(_), _) => todo!(),
            (TurnMode::RunRemovalFiller(_), _) => todo!(),
            (TurnMode::MarkerPlacementFiller, _) => todo!(),
            (turn_mode, action) => {
                unreachable!("Received unexpected player action {action:?} in mode {turn_mode:?}")
            }
        }
    }
}
