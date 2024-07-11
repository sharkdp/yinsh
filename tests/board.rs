use yinsh::{Board, Coord, Player};

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
fn has_run() {
    {
        let mut board = Board::empty();

        board.add_marker(Player::A, Coord::new(0, -2));
        board.add_marker(Player::A, Coord::new(0, -1));
        board.add_marker(Player::A, Coord::new(0, 0));
        board.add_marker(Player::A, Coord::new(0, 1));

        assert!(!board.has_run(Player::A));

        board.add_marker(Player::A, Coord::new(0, 2));

        assert!(board.has_run(Player::A));
    }

    {
        let mut board = Board::empty();

        board.add_marker(Player::A, Coord::new(-2, 0));
        board.add_marker(Player::A, Coord::new(-1, 0));
        board.add_marker(Player::A, Coord::new(0, 0));
        board.add_marker(Player::A, Coord::new(1, 0));

        assert!(!board.has_run(Player::A));

        board.add_marker(Player::A, Coord::new(2, 0));

        assert!(board.has_run(Player::A));
    }

    {
        let mut board = Board::empty();

        board.add_marker(Player::A, Coord::new(-2, -2));
        board.add_marker(Player::A, Coord::new(-1, -1));
        board.add_marker(Player::A, Coord::new(0, 0));
        board.add_marker(Player::A, Coord::new(1, 1));

        assert!(!board.has_run(Player::A));

        board.add_marker(Player::A, Coord::new(2, 2));

        assert!(board.has_run(Player::A));
    }
}
