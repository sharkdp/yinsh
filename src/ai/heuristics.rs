use minimax::Evaluation;

use super::evaluator::Heuristic;

use crate::{yinsh::GameState, Board, Coord, Player};

#[derive(Debug, Clone, Copy, Default)]
struct RingPositionStatistics {
    controlled_markers_own: usize,
    controlled_markers_opponent: usize,
    accessible_fields: usize,
}

fn ring_position_statistics(board: &Board, player: Player) -> RingPositionStatistics {
    let mut statistics = RingPositionStatistics::default();

    for ring in board.ring_coords(player) {
        for move_end in board.ring_moves(ring) {
            statistics.accessible_fields += 1;

            Coord::between(ring, move_end).iter().for_each(|coord| {
                match board.element_color_at(*coord) {
                    Some(p) if p == player => statistics.controlled_markers_own += 1, // TODO: We double-count here, but maybe that's not a problem (since it's good to control a marker with multiple rings?)
                    Some(_) => statistics.controlled_markers_opponent += 1,
                    None => {}
                }
            });
        }
    }

    statistics
}

#[derive(Debug, Clone, Copy)]
pub struct SimpleHeuristic {
    pub f_points: Evaluation,
    pub f_markers: Evaluation,
    pub f_controlled_markers_own: Evaluation,
    pub f_controlled_markers_opponent: Evaluation,
    pub f_accessible_fields: Evaluation,
}

impl Default for SimpleHeuristic {
    fn default() -> Self {
        Self {
            f_points: 10_000,
            f_markers: 100,
            f_controlled_markers_own: 0,
            f_controlled_markers_opponent: 10,
            f_accessible_fields: 0,
            // f_controlled_markers_own: 3,
            // f_controlled_markers_opponent: 10,
            // f_accessible_fields: 1,
        }
    }
}

impl Heuristic for SimpleHeuristic {
    fn evaluate_for_player_a(&self, state: &GameState) -> Evaluation {
        type Score = Evaluation;

        let score_points = (state.points_a as Score) - (state.points_b as Score);

        let score_markers = (state.board.num_markers(Player::A) as Score)
            - (state.board.num_markers(Player::B) as Score);

        let rps = ring_position_statistics(&state.board, Player::A);

        let score_rings = self.f_controlled_markers_own * rps.controlled_markers_own as Score
            + self.f_controlled_markers_opponent * rps.controlled_markers_opponent as Score
            + self.f_accessible_fields * rps.accessible_fields as Score;

        let score = self.f_points * score_points + self.f_markers * score_markers + score_rings;

        score
    }
}
