use std::iter;

use itertools::Itertools;
use serde::{Deserialize, Serialize};

use crate::yinsh::core::AXES;

use super::{core::all_coords, Coord, Player, DIRECTIONS};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Element {
    Ring(Player),
    Marker(Player),
    Empty,
}

impl Element {
    pub fn is_marker(&self) -> bool {
        matches!(self, Element::Marker(_))
    }

    fn is_ring(&self) -> bool {
        matches!(self, Element::Ring(_))
    }

    fn is_empty(&self) -> bool {
        matches!(self, Element::Empty)
    }

    fn player(&self) -> Option<Player> {
        match self {
            Element::Ring(player) => Some(*player),
            Element::Marker(player) => Some(*player),
            Element::Empty => None,
        }
    }
}

impl Default for Element {
    fn default() -> Self {
        Element::Empty
    }
}

#[derive(Debug, Clone, Default)]
pub struct CheckRunResult {
    a_has_run: bool,
    b_has_run: bool,
}

impl CheckRunResult {
    fn or(&mut self, other: &Self) {
        self.a_has_run |= other.a_has_run;
        self.b_has_run |= other.b_has_run;
    }

    pub fn has_run(&self, player: Player) -> bool {
        match player {
            Player::A => self.a_has_run,
            Player::B => self.b_has_run,
        }
    }

    pub fn no_runs(&self) -> bool {
        !self.a_has_run && !self.b_has_run
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Board {
    #[serde(skip)]
    board: [[Element; 11]; 11],

    rings_a: Vec<Coord>,
    rings_b: Vec<Coord>,
    markers_a: Vec<Coord>,
    markers_b: Vec<Coord>,
}

impl Board {
    pub fn empty() -> Self {
        Self::default()
    }

    /// Returns the element at a certain position or None if the coordinate is free (or invalid).
    /// Does not check for validity.
    fn element_at_unchecked(&self, coord: Coord) -> Element {
        self.board[(coord.y + 5) as usize][(coord.x + 5) as usize]
    }

    /// Returns the element at a certain position or None if the coordinate is free (or invalid)
    fn element_at(&self, coord: Coord) -> Element {
        if coord.is_inside_board() {
            self.board[(coord.y + 5) as usize][(coord.x + 5) as usize]
        } else {
            Element::Empty
        }
    }

    /// Returns the element at a certain position or None if the coordinate is free. Does not
    /// check for validity.
    fn element_at_mut_unchecked(&mut self, coord: Coord) -> &mut Element {
        &mut self.board[(coord.y + 5) as usize][(coord.x + 5) as usize]
    }

    fn insert_board_element_at(&mut self, coord: Coord, element: Element) {
        debug_assert!(coord.is_inside_board());

        *self.element_at_mut_unchecked(coord) = element;
    }

    fn remove_board_element_at(&mut self, coord: &Coord) {
        debug_assert!(coord.is_inside_board());

        *self.element_at_mut_unchecked(*coord) = Element::Empty;
    }

    /// Returns true if a certain point on the board is free. Does not check for validity.
    pub fn is_free(&self, coord: Coord) -> bool {
        self.element_at(coord).is_empty()
    }

    /// Returns true if the element at the given point is a ring of any color.
    pub fn has_ring_at(&self, coord: Coord, player: Player) -> bool {
        self.check_invariants();

        self.element_at(coord) == Element::Ring(player)
    }

    /// Returns true if the element at the given point is a marker of any color.
    pub fn has_marker_at(&self, coord: Coord) -> bool {
        self.check_invariants();

        self.element_at(coord).is_marker()
    }

    /// Returns the color/player of the board element at the given coordinate.
    pub fn element_color_at(&self, coord: Coord) -> Option<Player> {
        self.check_invariants();

        self.element_at(coord).player()
    }

    pub fn add_ring(&mut self, player: Player, coord: Coord) {
        self.check_invariants();
        debug_assert!(self.is_free(coord));

        self.insert_board_element_at(coord, Element::Ring(player));
        match player {
            Player::A => self.rings_a.push(coord),
            Player::B => self.rings_b.push(coord),
        }
    }

    pub fn remove_ring(&mut self, coord: Coord) {
        self.check_invariants();
        debug_assert!(self.element_at(coord).is_ring());

        self.remove_board_element_at(&coord);
        self.rings_a.retain(|&x| x != coord);
        self.rings_b.retain(|&x| x != coord);
    }

    pub fn num_rings(&self) -> usize {
        self.check_invariants();

        self.rings_a.len() + self.rings_b.len()
    }

    pub fn add_marker(&mut self, player: Player, coord: Coord) {
        self.check_invariants();
        debug_assert!(self.is_free(coord));

        self.insert_board_element_at(coord, Element::Marker(player));
        match player {
            Player::A => self.markers_a.push(coord),
            Player::B => self.markers_b.push(coord),
        }
    }

    pub fn remove_marker(&mut self, coord: Coord) {
        self.check_invariants();
        debug_assert!(self.has_marker_at(coord));

        self.remove_board_element_at(&coord);
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

            // Skip over arbitrarily many free spaces
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

    fn can_place_marker_at(&self, coord: Coord, player: Player) -> bool {
        self.check_invariants();

        // TODO: is this logic correct?
        (self.element_at(coord) == Element::Ring(player)) && !self.ring_moves(coord).is_empty()
    }

    pub fn marker_moves(&self, player: Player) -> impl Iterator<Item = Coord> + '_ {
        self.check_invariants();

        self.ring_coords(player)
            .filter(move |c| self.can_place_marker_at(*c, player))
    }

    pub fn flip_markers_between(&mut self, start: Coord, end: Coord) {
        self.check_invariants();

        debug_assert!(start.is_inside_board());
        debug_assert!(end.is_inside_board());

        for coord in Coord::between(start, end) {
            if let Element::Marker(player) = self.element_at_mut_unchecked(coord) {
                player.flip();

                if *player == Player::A {
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

        let seed = self.element_at(start);
        let player = seed.player().unwrap();

        debug_assert!(seed.is_marker());

        for a in AXES {
            let markers_positive = (1i8..=4i8)
                .map(|i| start + a.direction() * i)
                .take_while(|c| self.element_at(*c) == Element::Marker(player));
            let markers_negative = (1i8..=4i8)
                .map(|i| start - a.direction() * i)
                .take_while(|c| self.element_at(*c) == Element::Marker(player));

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

    fn check_run_along_line(&self, start: Coord, direction: Coord, steps: i8) -> CheckRunResult {
        let mut num_consecutive = 0;
        let mut player = None;

        let mut a_has_run = false;
        let mut b_has_run = false;

        for n in 0..steps {
            let coord = start + direction * n;

            match self.element_at_unchecked(coord) {
                Element::Marker(p) => {
                    if Some(p) == player {
                        num_consecutive += 1;

                        if num_consecutive == 5 {
                            if p == Player::A {
                                a_has_run = true;
                            } else {
                                b_has_run = true;
                            }
                        }
                    } else {
                        player = Some(p);
                        num_consecutive = 1;
                    }
                }
                _ => {
                    player = None;
                    num_consecutive = 0;
                }
            }
        }

        CheckRunResult {
            a_has_run,
            b_has_run,
        }
    }

    /// Returns true if the player has a run
    pub fn check_run(&self) -> CheckRunResult {
        self.check_invariants();

        let mut result = CheckRunResult {
            a_has_run: false,
            b_has_run: false,
        };

        // This is not pretty, but it's 10x faster than the naive implementation using run_coords_from
        // and 5x faster than running across the [-5..5] x [-5..5] grid and checking for valid coordinates.

        // x direction
        let x = Coord::new(1, 0);
        result.or(&self.check_run_along_line(Coord::new(-1, 4), x, 7));
        result.or(&self.check_run_along_line(Coord::new(-2, 3), x, 8));
        result.or(&self.check_run_along_line(Coord::new(-3, 2), x, 9));
        result.or(&self.check_run_along_line(Coord::new(-4, 1), x, 10));
        result.or(&self.check_run_along_line(Coord::new(-4, 0), x, 9));
        result.or(&self.check_run_along_line(Coord::new(-5, -1), x, 10));
        result.or(&self.check_run_along_line(Coord::new(-5, -2), x, 9));
        result.or(&self.check_run_along_line(Coord::new(-5, -3), x, 8));
        result.or(&self.check_run_along_line(Coord::new(-5, -4), x, 7));

        // y direction
        let y = Coord::new(0, 1);
        result.or(&self.check_run_along_line(Coord::new(-4, -5), y, 7));
        result.or(&self.check_run_along_line(Coord::new(-3, -5), y, 8));
        result.or(&self.check_run_along_line(Coord::new(-2, -5), y, 9));
        result.or(&self.check_run_along_line(Coord::new(-1, -5), y, 10));
        result.or(&self.check_run_along_line(Coord::new(0, -4), y, 9));
        result.or(&self.check_run_along_line(Coord::new(1, -4), y, 10));
        result.or(&self.check_run_along_line(Coord::new(2, -3), y, 9));
        result.or(&self.check_run_along_line(Coord::new(3, -2), y, 8));
        result.or(&self.check_run_along_line(Coord::new(4, -1), y, 7));

        // diagonal direction
        let d = Coord::new(1, 1);
        result.or(&self.check_run_along_line(Coord::new(-5, -1), d, 7));
        result.or(&self.check_run_along_line(Coord::new(-5, -2), d, 8));
        result.or(&self.check_run_along_line(Coord::new(-5, -3), d, 9));
        result.or(&self.check_run_along_line(Coord::new(-5, -4), d, 10));
        result.or(&self.check_run_along_line(Coord::new(-4, -4), d, 9));
        result.or(&self.check_run_along_line(Coord::new(-4, -5), d, 10));
        result.or(&self.check_run_along_line(Coord::new(-3, -5), d, 9));
        result.or(&self.check_run_along_line(Coord::new(-2, -5), d, 8));
        result.or(&self.check_run_along_line(Coord::new(-1, -5), d, 7));

        result
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
        for coord in all_coords() {
            match self.element_at(coord) {
                Element::Ring(Player::A) => {
                    debug_assert!(self.rings_a.contains(&coord));
                    debug_assert!(!self.rings_b.contains(&coord));
                }
                Element::Ring(Player::B) => {
                    debug_assert!(self.rings_b.contains(&coord));
                    debug_assert!(!self.rings_a.contains(&coord));
                }
                Element::Marker(Player::A) => {
                    debug_assert!(self.markers_a.contains(&coord));
                    debug_assert!(!self.markers_b.contains(&coord));
                }
                Element::Marker(Player::B) => {
                    debug_assert!(self.markers_b.contains(&coord));
                    debug_assert!(!self.markers_a.contains(&coord));
                }
                Element::Empty => {
                    debug_assert!(!self.rings_a.contains(&coord));
                    debug_assert!(!self.rings_b.contains(&coord));
                    debug_assert!(!self.markers_a.contains(&coord));
                    debug_assert!(!self.markers_b.contains(&coord));
                }
            }
        }

        for coord in self.rings_a.iter().copied() {
            debug_assert!(self.element_at(coord) == Element::Ring(Player::A));
        }

        for coord in self.rings_b.iter().copied() {
            debug_assert!(self.element_at(coord) == Element::Ring(Player::B));
        }

        for coord in self.markers_a.iter().copied() {
            debug_assert!(self.element_at(coord) == Element::Marker(Player::A));
        }

        for coord in self.markers_b.iter().copied() {
            debug_assert!(self.element_at(coord) == Element::Marker(Player::B));
        }
    }

    pub fn fill_board_from_lists(&mut self) {
        let rings_a = self.rings_a.clone();
        let rings_b = self.rings_b.clone();
        let markers_a = self.markers_a.clone();
        let markers_b = self.markers_b.clone();

        for coord in rings_a {
            self.insert_board_element_at(coord, Element::Ring(Player::A));
        }

        for coord in rings_b {
            self.insert_board_element_at(coord, Element::Ring(Player::B));
        }

        for coord in markers_a {
            self.insert_board_element_at(coord, Element::Marker(Player::A));
        }

        for coord in markers_b {
            self.insert_board_element_at(coord, Element::Marker(Player::B));
        }

        self.check_invariants();
    }

    #[cfg(not(debug_assertions))]
    fn check_invariants(&self) {}
}
