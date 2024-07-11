use std::{
    collections::HashMap,
    ops::{Add, Sub},
};

/// All Yinsh coordinates lie on a hexagonal grid within a circle of radius 4.6.
const BOARD_RADIUS_SQUARED: f32 = 4.6_f32 * 4.6_f32;

/// Yinsh hex coordinates
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Coord {
    pub x: i8,
    pub y: i8,
}

impl Coord {
    /// Check if the point lies within the boundaries of the board.
    pub fn is_inside_board(&self) -> bool {
        let sqrt_3 = 3.0_f32.sqrt();
        let x = self.x as f32;
        let y = self.y as f32;
        (0.5 * sqrt_3 * x).powi(2) + (0.5 * x - y).powi(2) <= BOARD_RADIUS_SQUARED
    }

    pub fn is_on_same_line_as(&self, other: Coord) -> bool {
        (self.x == other.x) || (self.y == other.y) || ((self.x - self.y) == (other.x - other.y))
    }

    fn shorten(&self) -> Coord {
        Coord {
            x: self.x.max(-1).min(1),
            y: self.y.max(-1).min(1),
        }
    }

    pub fn between(a: Coord, b: Coord) -> Vec<Coord> {
        let mut coords = Vec::new();
        let delta = (b - a).shorten();
        let mut current = a + delta;
        while current != b {
            coords.push(current);
            current = current + delta;
        }
        coords
    }
}

impl Add<Coord> for Coord {
    type Output = Coord;

    fn add(self, other: Coord) -> Coord {
        Coord {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl Sub<Coord> for Coord {
    type Output = Coord;

    fn sub(self, rhs: Coord) -> Self::Output {
        Coord {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

/// The six hex directions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    N,
    NE,
    SE,
    S,
    SW,
    NW,
}

impl Direction {
    fn delta(&self) -> Coord {
        match self {
            Direction::N => Coord { x: -1, y: 0 },
            Direction::S => Coord { x: 1, y: 0 },
            Direction::NE => Coord { x: -1, y: -1 },
            Direction::SW => Coord { x: 1, y: 1 },
            Direction::NW => Coord { x: 0, y: 1 },
            Direction::SE => Coord { x: 0, y: -1 },
        }
    }
}

const DIRECTIONS: &'static [Direction; 6] = &[
    Direction::N,
    Direction::NE,
    Direction::SE,
    Direction::S,
    Direction::SW,
    Direction::NW,
];

/// Player types (white and black)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Player {
    A,
    B,
}

impl Player {
    fn next(self) -> Self {
        match self {
            Player::A => Player::B,
            Player::B => Player::A,
        }
    }

    pub fn flip(&mut self) {
        *self = self.next();
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ElementKind {
    Ring,
    Marker,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Element {
    kind: ElementKind,
    player: Player,
}

impl Element {
    fn is_marker(&self) -> bool {
        self.kind == ElementKind::Marker
    }

    fn is_ring(&self) -> bool {
        self.kind == ElementKind::Ring
    }
}

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

#[derive(Debug, Clone, Default)]
pub struct Board {
    map: HashMap<Coord, Element>,
    rings_a: Vec<Coord>,
    rings_b: Vec<Coord>,
    markers_a: Vec<Coord>,
    markers_b: Vec<Coord>,
}

impl Board {
    pub fn empty() -> Self {
        Self::default()
    }

    /// Returns the element at a certain position or None if the coordinate is free (or invalid)
    fn element_at(&self, coord: Coord) -> Option<Element> {
        self.map.get(&coord).copied()
    }

    /// Returns true if the element at the given point is a marker of any color.
    fn is_marker_at(&self, coord: Coord) -> bool {
        self.map.get(&coord).map_or(false, Element::is_marker)
    }

    /// Returns true if a certain point on the board is free. Does not check for validity.
    pub fn is_free(&self, coord: Coord) -> bool {
        self.map.get(&coord).is_none()
    }

    pub fn add_ring(&mut self, player: Player, coord: Coord) {
        self.map.insert(
            coord,
            Element {
                kind: ElementKind::Ring,
                player,
            },
        );
        match player {
            Player::A => self.rings_a.push(coord),
            Player::B => self.rings_b.push(coord),
        }
    }

    fn num_rings(&self) -> usize {
        self.rings_a.len() + self.rings_b.len()
    }

    fn remove_ring(&mut self, coord: Coord) {
        self.map
            .retain(|&k, e| k != coord || e.kind != ElementKind::Ring);
        self.rings_a.retain(|&x| x != coord);
        self.rings_b.retain(|&x| x != coord);
    }

    fn add_marker(&mut self, active_player: Player, coord: Coord) {
        self.map.insert(
            coord,
            Element {
                kind: ElementKind::Marker,
                player: active_player,
            },
        );
        match active_player {
            Player::A => self.markers_a.push(coord),
            Player::B => self.markers_b.push(coord),
        }
    }

    pub fn free_coords(&self) -> impl Iterator<Item = Coord> + '_ {
        all_coords()
            .into_iter()
            .filter(|&coord| self.is_free(coord))
    }

    pub fn ring_coords(&self, player: Player) -> impl Iterator<Item = Coord> + '_ {
        match player {
            Player::A => self.rings_a.iter().copied(),
            Player::B => self.rings_b.iter().copied(),
        }
    }

    pub fn ring_moves(&self, start: Coord) -> Vec<Coord> {
        let mut moves = Vec::new();
        for d in DIRECTIONS {
            let mut current = start + d.delta();

            // Skip over arbirary many free fields
            while current.is_inside_board() && self.is_free(current) {
                moves.push(current);
                current = current + d.delta();
            }

            // Skip over arbitrary many markers, but stop immediately after
            while current.is_inside_board() && self.is_marker_at(current) {
                current = current + d.delta();
            }

            if current.is_inside_board() && self.is_free(current) {
                moves.push(current);
            }
        }
        moves
    }

    pub fn is_valid_ring_move(&self, start: Coord, end: Coord) -> bool {
        self.ring_moves(start).contains(&end)
    }

    pub fn can_place_marker_at(&self, coord: Coord, player: Player) -> bool {
        self.element_at(coord)
            .map_or(false, |e| e.is_ring() && e.player == player)
            && !self.ring_moves(coord).is_empty()
    }

    pub fn has_run(&self, player: Player) -> bool {
        let markers = match player {
            Player::A => &self.rings_a,
            Player::B => &self.rings_b,
        };

        for &start in markers {
            for d in DIRECTIONS {
                let mut current = start + d.delta();
                let mut count = 0;
                while current.is_inside_board() && self.is_marker_at(current) {
                    count += 1;
                    current = current + d.delta();
                }
                if count >= 5 {
                    return true;
                }
            }
        }

        false
    }
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

                // Flip markers in between
                for coord in Coord::between(*start, *end) {
                    self.board.map.get_mut(&coord).map(|e| e.player.flip());
                }

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

pub fn all_coords() -> Vec<Coord> {
    let mut coords = Vec::new();
    for x in -5..=5 {
        for y in -5..=5 {
            let coord = Coord { x, y };
            if coord.is_inside_board() {
                coords.push(coord);
            }
        }
    }
    coords
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn number_of_grid_intersections() {
        assert_eq!(all_coords().len(), 85);
    }
}
