use yinsh::{Board, Coord, DIRECTIONS, Player};

#[test]
fn basic_marker_placement() {
    let mut board = Board::empty();

    for c in yinsh::all_coords() {
        assert!(board.is_free(c));
    }

    let marker_coord = Coord::new(3, -2);
    board.add_marker(Player::A, marker_coord);

    assert!(!board.is_free(marker_coord));
    assert_eq!(board.num_markers(Player::A), 1);
    assert_eq!(board.num_markers(Player::B), 0);

    board.remove_marker(marker_coord);

    assert!(board.is_free(marker_coord));

    assert_eq!(board, Board::empty());
}

#[test]
fn basic_ring_placement() {
    let mut board = Board::empty();

    let ring_coord = Coord::new(3, -2);
    board.add_ring(Player::A, ring_coord);

    assert!(!board.is_free(ring_coord));

    assert!(board.has_ring_at(ring_coord, Player::A));

    board.remove_ring(ring_coord);

    assert!(board.is_free(ring_coord));

    assert_eq!(board, Board::empty());
}

#[test]
fn check_run_basic() {
    let mut board = Board::empty();

    assert!(board.check_run().no_runs());

    board.add_marker(Player::A, Coord::new(0, -2));
    board.add_marker(Player::A, Coord::new(0, -1));
    board.add_marker(Player::A, Coord::new(0, 0));
    board.add_marker(Player::A, Coord::new(0, 1));

    assert!(board.check_run().no_runs());

    board.add_marker(Player::A, Coord::new(0, 2));

    assert!(board.check_run().has_run(Player::A));
    assert!(!board.check_run().has_run(Player::B));

    board.add_marker(Player::B, Coord::new(-1, -1));
    board.add_marker(Player::B, Coord::new(-1, 0));
    board.add_marker(Player::B, Coord::new(-1, 1));
    board.add_marker(Player::B, Coord::new(-1, 2));

    assert!(!board.check_run().has_run(Player::B));

    board.add_marker(Player::B, Coord::new(-1, -2));

    assert!(board.check_run().has_run(Player::A));
    assert!(board.check_run().has_run(Player::B));
}

#[test]
fn check_run_exhaustive() {
    for c in yinsh::all_coords() {
        for d in DIRECTIONS {
            if (c + d.direction() * 4).is_inside_board() {
                let mut board = Board::empty();

                for i in 0..=4i8 {
                    board.add_marker(Player::A, c + d.direction() * i);
                }

                assert!(board.check_run().has_run(Player::A));
            }
        }
    }
}
