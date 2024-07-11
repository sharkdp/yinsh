use std::collections::HashMap;

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

/// Player types (white and black)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Player {
    A,
    B,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ElementKind {
    Ring,
    Marker,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Element {
    pub kind: ElementKind,
    pub player: Player,
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
    AddRing,

    /// place a marker in one of your rings
    AddMarker,

    /// move the ring at the given position
    MoveRing(Coord),

    /// Remove (one of your) run(s). The parameter
    /// holds the last player who moved a ring.
    RemoveRun(Player),

    /// Remove one of your rings
    RemoveRing(Player),

    /// Do nothing
    WaitRemoveRun(Player),

    /// Do nothing
    WaitAddMarker,
}

#[derive(Debug, Clone, Default)]
pub struct Board {
    map: HashMap<Coord, Element>,
    rings_black: Vec<Coord>,
    rings_white: Vec<Coord>,
    markers_black: Vec<Coord>,
    markers_white: Vec<Coord>,
}

impl Board {
    /// Returns the element at a certain position or None if the coordinate is free (or invalid)
    pub fn element_at(&self, coord: Coord) -> Option<Element> {
        self.map.get(&coord).copied()
    }

    /// Returns true if the element at the given point is a marker of any color.
    pub fn is_marker(&self, coord: Coord) -> bool {
        self.map.get(&coord).map_or(false, Element::is_marker)
    }

    /// Returns true if the element at the given point is a ring of any color.
    pub fn is_ring(&self, coord: Coord) -> bool {
        self.map.get(&coord).map_or(false, Element::is_ring)
    }

    /// Returns true if a certain point on the board is free. Does not check for validity.
    pub fn is_free(&self, coord: Coord) -> bool {
        self.map.get(&coord).is_none()
    }
}

pub struct GameState {
    active_player: Player,
    turn_mode: TurnMode,
    board: Board,
    points_black: usize,
    points_white: usize,
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
