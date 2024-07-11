use std::{collections::HashMap, iter};

use itertools::Itertools;
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

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
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
        self.check_invariants();

        self.map.get(&coord).copied()
    }

    /// Returns the element at a certain position or None if the coordinate is free (or invalid)
    fn element_at_mut(&mut self, coord: Coord) -> Option<&mut Element> {
        self.check_invariants();

        self.map.get_mut(&coord)
    }

    /// Returns true if a certain point on the board is free. Does not check for validity.
    pub fn is_free(&self, coord: Coord) -> bool {
        self.check_invariants();

        self.map.get(&coord).is_none()
    }

    /// Returns true if the element at the given point is a ring of any color.
    pub fn has_ring_at(&self, coord: Coord, player: Player) -> bool {
        self.check_invariants();

        self.map
            .get(&coord)
            .map_or(false, |e| e.is_ring() && e.player == player)
    }

    /// Returns true if the element at the given point is a marker of any color.
    fn has_marker_at(&self, coord: Coord) -> bool {
        self.check_invariants();

        self.map.get(&coord).map_or(false, Element::is_marker)
    }

    pub fn add_ring(&mut self, player: Player, coord: Coord) {
        self.check_invariants();
        debug_assert!(self.is_free(coord));

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

    pub fn remove_ring(&mut self, coord: Coord) {
        self.check_invariants();
        debug_assert!(self.element_at(coord).map_or(false, |e| e.is_ring()));

        self.map.remove(&coord);
        self.rings_a.retain(|&x| x != coord);
        self.rings_b.retain(|&x| x != coord);
    }

    pub fn num_rings(&self) -> usize {
        self.check_invariants();

        self.rings_a.len() + self.rings_b.len()
    }

    pub fn add_marker(&mut self, active_player: Player, coord: Coord) {
        self.check_invariants();
        debug_assert!(self.is_free(coord));

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

    pub fn remove_marker(&mut self, coord: Coord) {
        self.check_invariants();
        debug_assert!(self.has_marker_at(coord));

        self.map
            .retain(|&k, e| k != coord || e.kind != ElementKind::Marker);
        self.markers_a.retain(|&x| x != coord);
        self.markers_b.retain(|&x| x != coord);
    }

    pub fn num_markers(&self, player: Player) -> usize {
        self.check_invariants();

        match player {
            Player::A => self.markers_a.len(),
            Player::B => self.markers_b.len(),
        }
    }

    pub fn free_coords(&self) -> impl Iterator<Item = Coord> + '_ {
        self.check_invariants();

        all_coords()
            .into_iter()
            .filter(|&coord| self.is_free(coord))
    }

    pub fn ring_coords(&self, player: Player) -> impl Iterator<Item = Coord> + '_ {
        self.check_invariants();

        match player {
            Player::A => self.rings_a.iter().copied(),
            Player::B => self.rings_b.iter().copied(),
        }
    }

    pub fn marker_coords(&self, player: Player) -> impl Iterator<Item = Coord> + '_ {
        self.check_invariants();

        match player {
            Player::A => self.markers_a.iter().copied(),
            Player::B => self.markers_b.iter().copied(),
        }
    }

    pub fn ring_moves(&self, start: Coord) -> Vec<Coord> {
        self.check_invariants();

        let mut moves = Vec::new();

        for d in DIRECTIONS {
            let mut current = start + d.direction();

            // Skip over arbirary many free fields
            while current.is_inside_board() && self.is_free(current) {
                moves.push(current);
                current = current + d.direction();
            }

            // Skip over arbitrarily many markers, but stop immediately after
            while self.has_marker_at(current) && current.is_inside_board() {
                current = current + d.direction();
            }

            if current.is_inside_board() && self.is_free(current) {
                moves.push(current);
            }
        }
        moves
    }

    pub fn is_valid_ring_move(&self, start: Coord, end: Coord) -> bool {
        self.check_invariants();

        self.ring_moves(start).contains(&end)
    }

    pub fn can_place_marker_at(&self, coord: Coord, player: Player) -> bool {
        self.check_invariants();

        // TODO: is this logic correct?
        self.element_at(coord)
            .map_or(false, |e| e.is_ring() && e.player == player)
            && !self.ring_moves(coord).is_empty()
    }

    pub fn flip_markers_between(&mut self, start: Coord, end: Coord) {
        self.check_invariants();

        for coord in Coord::between(start, end) {
            if let Some(e) = self.element_at_mut(coord) {
                debug_assert!(e.is_marker());

                e.flip_player();

                if e.player == Player::A {
                    self.markers_a.push(coord);
                    self.markers_b.retain(|&x| x != coord);
                } else {
                    self.markers_b.push(coord);
                    self.markers_a.retain(|&x| x != coord);
                }
            }
        }
    }

    /// Returns the coordinates of a run (if one exists), starting at the given seed.
    pub fn run_coords_from(&self, start: Coord) -> Option<Vec<Coord>> {
        self.check_invariants();

        let seed = self.element_at(start).unwrap();

        debug_assert!(seed.is_marker());

        for a in AXES {
            let markers_positive = (1i8..=4i8)
                .map(|i| start + a.direction() * i)
                .take_while(|c| {
                    self.element_at(*c)
                        .map_or(false, |e| e.is_marker() && e.player == seed.player)
                });
            let markers_negative = (1i8..=4i8)
                .map(|i| start - a.direction() * i)
                .take_while(|c| {
                    self.element_at(*c)
                        .map_or(false, |e| e.is_marker() && e.player == seed.player)
                });

            let run_coords: Vec<_> = iter::once(start)
                .chain(markers_positive.interleave(markers_negative))
                .take(5)
                .collect();

            if run_coords.len() == 5 {
                return Some(run_coords);
            }
        }

        None
    }

    /// Returns true if the player has a run
    pub fn has_run(&self, player: Player) -> bool {
        self.check_invariants();

        for coord in self.marker_coords(player) {
            if self.run_coords_from(coord).is_some() {
                return true;
            }
        }
        false
    }

    /// Return coordinates that belong to a run. Multiple runs can exist at the same time.
    pub fn run_coords(&self, player: Player) -> Vec<Coord> {
        self.check_invariants();

        let mut run_coords = Vec::new();
        for coord in self.marker_coords(player) {
            if let Some(coords) = self.run_coords_from(coord) {
                run_coords.extend(coords);
            }
        }
        run_coords
    }

    pub fn remove_run(&mut self, seed: Coord) {
        self.check_invariants();

        let run_coords = self
            .run_coords_from(seed)
            .expect("remove_run called with invalid seed");
        for coord in run_coords {
            self.remove_marker(coord);
        }
    }

    #[cfg(debug_assertions)]
    fn check_invariants(&self) {
        for (coord, element) in &self.map {
            debug_assert!(coord.is_inside_board());

            match (element.kind, element.player) {
                (ElementKind::Ring, Player::A) => {
                    debug_assert!(self.rings_a.contains(coord));
                    debug_assert!(!self.rings_b.contains(coord));
                }
                (ElementKind::Ring, Player::B) => {
                    debug_assert!(self.rings_b.contains(coord));
                    debug_assert!(!self.rings_a.contains(coord));
                }
                (ElementKind::Marker, Player::A) => {
                    debug_assert!(self.markers_a.contains(coord));
                    debug_assert!(!self.markers_b.contains(coord));
                }
                (ElementKind::Marker, Player::B) => {
                    debug_assert!(self.markers_b.contains(coord));
                    debug_assert!(!self.markers_a.contains(coord));
                }
            }
        }

        for coord in self.rings_a.iter().copied() {
            debug_assert!(self
                .map
                .get(&coord)
                .map_or(false, |e| e.is_ring() && e.player == Player::A));
        }

        for coord in self.rings_b.iter().copied() {
            debug_assert!(self
                .map
                .get(&coord)
                .map_or(false, |e| e.is_ring() && e.player == Player::B));
        }

        for coord in self.markers_a.iter().copied() {
            debug_assert!(self
                .map
                .get(&coord)
                .map_or(false, |e| e.is_marker() && e.player == Player::A));
        }

        for coord in self.markers_b.iter().copied() {
            debug_assert!(self
                .map
                .get(&coord)
                .map_or(false, |e| e.is_marker() && e.player == Player::B));
        }
    }

    #[cfg(not(debug_assertions))]
    fn check_invariants(&self) {}
}
