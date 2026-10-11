use std::sync::LazyLock;

use yinsh::{Coord, GameState, Player, all_coords};

/// Number of valid board coordinates
const NUM_COORDS: usize = 85;

/// Number of hand-crafted aggregate features
const NUM_AGGREGATE_FEATURES: usize = 4;

/// Total feature size: markers (85) + rings (85) + points (2) + aggregate (4)
pub const FEATURE_SIZE: usize = NUM_COORDS * 2 + 2 + NUM_AGGREGATE_FEATURES;

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
/// Features (176 total):
/// - [0..85]: Marker layer - for each coord: -1 (B marker), 0 (empty), +1 (A marker)
/// - [85..170]: Ring layer - for each coord: -1 (B ring), 0 (empty), +1 (A ring)
/// - [170]: points_a / 3.0 (normalized to [0, 1])
/// - [171]: points_b / 3.0 (normalized to [0, 1])
/// - [172]: markers_a / NUM_COORDS (normalized marker count for A)
/// - [173]: markers_b / NUM_COORDS (normalized marker count for B)
/// - [174]: rings_a / 5.0 (normalized ring count for A)
/// - [175]: rings_b / 5.0 (normalized ring count for B)
pub fn extract_features(state: &GameState) -> Vec<f32> {
    let mut features = vec![0.0; FEATURE_SIZE];

    // Marker layer (indices 0..85)
    let mut markers_a = 0;
    let mut markers_b = 0;
    for coord in state.board.marker_coords(Player::A) {
        features[coord_to_index(coord)] = 1.0;
        markers_a += 1;
    }
    for coord in state.board.marker_coords(Player::B) {
        features[coord_to_index(coord)] = -1.0;
        markers_b += 1;
    }

    // Ring layer (indices 85..170)
    let ring_offset = NUM_COORDS;
    let mut rings_a = 0;
    let mut rings_b = 0;
    for coord in state.board.ring_coords(Player::A) {
        features[ring_offset + coord_to_index(coord)] = 1.0;
        rings_a += 1;
    }
    for coord in state.board.ring_coords(Player::B) {
        features[ring_offset + coord_to_index(coord)] = -1.0;
        rings_b += 1;
    }

    // Points (indices 170, 171)
    let points_offset = NUM_COORDS * 2;
    features[points_offset] = state.points_a as f32 / 3.0;
    features[points_offset + 1] = state.points_b as f32 / 3.0;

    // Aggregate features (indices 172..176)
    let agg_offset = points_offset + 2;
    features[agg_offset] = markers_a as f32 / NUM_COORDS as f32;
    features[agg_offset + 1] = markers_b as f32 / NUM_COORDS as f32;
    features[agg_offset + 2] = rings_a as f32 / 5.0;
    features[agg_offset + 3] = rings_b as f32 / 5.0;

    features
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_feature_size() {
        assert_eq!(FEATURE_SIZE, 176);
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
