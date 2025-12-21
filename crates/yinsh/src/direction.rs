use crate::coord::Coord;

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
    pub fn direction(&self) -> Coord {
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

pub const DIRECTIONS: [Direction; 6] = [
    Direction::N,
    Direction::NE,
    Direction::SE,
    Direction::S,
    Direction::SW,
    Direction::NW,
];

pub const AXES: [Direction; 3] = [Direction::N, Direction::NE, Direction::NW];
