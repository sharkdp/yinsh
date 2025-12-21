use std::ops::{Add, Mul, Sub};

use serde::{Deserialize, Serialize};

/// All Yinsh coordinates lie on a hexagonal grid within a circle of radius 4.6.
const BOARD_RADIUS_SQUARED: f32 = 4.6_f32 * 4.6_f32;

/// Yinsh hex coordinates
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Coord {
    pub x: i8,
    pub y: i8,
}

impl Coord {
    pub fn new(x: i8, y: i8) -> Self {
        Coord { x, y }
    }

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
            x: self.x.clamp(-1, 1),
            y: self.y.clamp(-1, 1),
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

    pub fn norm(&self) -> f32 {
        ((self.x as f32).powi(2) + (self.y as f32).powi(2)).sqrt()
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

impl Mul<i8> for Coord {
    type Output = Coord;

    fn mul(self, rhs: i8) -> Self::Output {
        Coord {
            x: self.x * rhs,
            y: self.y * rhs,
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
