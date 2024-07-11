use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::yinsh::core::AXES;

use super::{core::all_coords, Coord, Player, DIRECTIONS};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ElementKind {
    Ring,
    Marker,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
struct Element {
    kind: ElementKind,
    player: Player,
}

impl Element {
    pub fn is_marker(&self) -> bool {
        self.kind == ElementKind::Marker
    }

    fn is_ring(&self) -> bool {
        self.kind == ElementKind::Ring
    }

    pub fn flip_player(&mut self) {
        self.player.flip();
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
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

    /// Returns the element at a certain position or None if the coordinate is free (or invalid)
    fn element_at_mut(&mut self, coord: Coord) -> Option<&mut Element> {
        self.map.get_mut(&coord)
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

    pub fn num_rings(&self) -> usize {
        self.rings_a.len() + self.rings_b.len()
    }

    pub fn remove_ring(&mut self, coord: Coord) {
        self.map
            .retain(|&k, e| k != coord || e.kind != ElementKind::Ring);
        self.rings_a.retain(|&x| x != coord);
        self.rings_b.retain(|&x| x != coord);
    }

    pub fn remove_marker(&mut self, coord: Coord) {
        self.map
            .retain(|&k, e| k != coord || e.kind != ElementKind::Marker);
        self.markers_a.retain(|&x| x != coord);
        self.markers_b.retain(|&x| x != coord);
    }

    pub fn add_marker(&mut self, active_player: Player, coord: Coord) {
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

    pub fn marker_coords(&self, player: Player) -> impl Iterator<Item = Coord> + '_ {
        match player {
            Player::A => self.markers_a.iter().copied(),
            Player::B => self.markers_b.iter().copied(),
        }
    }

    pub fn ring_moves(&self, start: Coord) -> Vec<Coord> {
        let mut moves = Vec::new();
        for d in DIRECTIONS {
            let mut current = start + d.direction();

            // Skip over arbirary many free fields
            while current.is_inside_board() && self.is_free(current) {
                moves.push(current);
                current = current + d.direction();
            }

            // Skip over arbitrary many markers, but stop immediately after
            while current.is_inside_board() && self.is_marker_at(current) {
                current = current + d.direction();
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
            Player::A => &self.markers_a,
            Player::B => &self.markers_b,
        };

        for &start in markers {
            for d in DIRECTIONS {
                let mut current = start + d.direction();
                let mut count = 1;
                while current.is_inside_board() && self.is_marker_at(current) {
                    count += 1;
                    current = current + d.direction();
                }
                if count >= 5 {
                    return true;
                }
            }
        }

        false
    }

    pub fn flip_markers_between(&mut self, start: Coord, end: Coord) {
        for coord in Coord::between(start, end) {
            self.element_at_mut(coord).map(|e| {
                debug_assert!(e.is_marker());
                e.flip_player()
            });
        }
    }

    pub fn run_coords_from(&self, start: Coord) -> Vec<Coord> {
        let seed = self.element_at(start).unwrap();

        debug_assert!(seed.is_marker());

        for a in AXES {
            let mut run_coords = vec![start];

            for i in 1i8..=10i8 {
                // TODO
                let coord_left = start + a.direction() * i;
                let coord_right = start - a.direction() * i;

                for &coord in &[coord_left, coord_right] {
                    if self
                        .element_at(coord)
                        .map_or(false, |e| e.is_marker() && e.player == seed.player)
                    {
                        run_coords.push(coord);
                        if run_coords.len() == 5 {
                            return run_coords;
                        }
                    }
                }
            }
        }

        unreachable!();
    }

    /// Return all coordinates that belong to a run
    pub fn run_coords(&self, player: Player) -> Vec<Coord> {
        let markers = match player {
            Player::A => &self.markers_a,
            Player::B => &self.markers_b,
        };

        let mut run_coords = Vec::new();

        for &start in markers {
            for d in DIRECTIONS {
                let mut current = start + d.direction();
                let mut count = 1;
                while current.is_inside_board() && self.is_marker_at(current) {
                    count += 1;
                    current = current + d.direction();
                }
                if count >= 5 {
                    let mut current = start;
                    for _ in 0..5 {
                        run_coords.push(current);
                        current = current + d.direction();
                    }
                }
            }
        }

        run_coords
    }

    pub fn remove_run(&mut self, seed: Coord) {
        let run_coords = self.run_coords_from(seed);
        for coord in run_coords {
            self.remove_marker(coord);
        }
    }
}
