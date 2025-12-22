use std::sync::LazyLock;

use yinsh::{Coord, GameState, Player, all_coords};

/// Number of valid board coordinates
const NUM_COORDS: usize = 85;

/// Total feature size: markers (85) + rings (85) + points (2)
pub const FEATURE_SIZE: usize = NUM_COORDS * 2 + 2;

/// Mapping from Coord to feature index (0-84)
/// Uses a sorted list of valid coordinates for consistent ordering.
static COORD_TO_INDEX: LazyLock<std::collections::HashMap<Coord, usize>> = LazyLock::new(|| {
    let mut coords = all_coords();
    // Sort by (x, y) for consistent ordering
    coords.sort_by_key(|c| (c.x, c.y));
    coords
        .into_iter()
        .enumerate()
        .map(|(i, c)| (c, i))
        .collect()
});

/// Convert a coordinate to its feature index (0-84)
fn coord_to_index(coord: Coord) -> usize {
    COORD_TO_INDEX[&coord]
}

/// Extract feature vector from game state.
///
/// Features (172 total):
/// - [0..85]: Marker layer - for each coord: -1 (B marker), 0 (empty), +1 (A marker)
/// - [85..170]: Ring layer - for each coord: -1 (B ring), 0 (empty), +1 (A ring)
/// - [170]: points_a / 3.0 (normalized to [0, 1])
/// - [171]: points_b / 3.0 (normalized to [0, 1])
pub fn extract_features(state: &GameState) -> Vec<f32> {
    let mut features = vec![0.0; FEATURE_SIZE];

    // Marker layer (indices 0..85)
    for coord in state.board.marker_coords(Player::A) {
        features[coord_to_index(coord)] = 1.0;
    }
    for coord in state.board.marker_coords(Player::B) {
        features[coord_to_index(coord)] = -1.0;
    }

    // Ring layer (indices 85..170)
    let ring_offset = NUM_COORDS;
    for coord in state.board.ring_coords(Player::A) {
        features[ring_offset + coord_to_index(coord)] = 1.0;
    }
    for coord in state.board.ring_coords(Player::B) {
        features[ring_offset + coord_to_index(coord)] = -1.0;
    }

    // Points (indices 170, 171)
    let points_offset = NUM_COORDS * 2;
    features[points_offset] = state.points_a as f32 / 3.0;
    features[points_offset + 1] = state.points_b as f32 / 3.0;

    features
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_feature_size() {
        assert_eq!(FEATURE_SIZE, 172);
    }

    #[test]
    fn test_coord_mapping_complete() {
        assert_eq!(COORD_TO_INDEX.len(), 85);
    }

    #[test]
    fn test_coord_indices_unique() {
        let indices: std::collections::HashSet<_> = COORD_TO_INDEX.values().collect();
        assert_eq!(indices.len(), 85);
    }

    #[test]
    fn test_extract_features_empty_board() {
        let state = GameState::initial();
        let features = extract_features(&state);

        assert_eq!(features.len(), FEATURE_SIZE);
        // All positions should be 0 (empty) for a new game
        for &f in &features[..170] {
            assert_eq!(f, 0.0);
        }
        // Points should be 0
        assert_eq!(features[170], 0.0);
        assert_eq!(features[171], 0.0);
    }
}
