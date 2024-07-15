use yinsh::Player;

use criterion::{criterion_group, criterion_main, Criterion};
use yinsh::GameState;

pub fn criterion_benchmark(c: &mut Criterion) {
    let game_state = GameState::load_from("tests/midgame_1.yml");
    let board = game_state.board.clone();

    c.bench_function("Board::has_run", |b| b.iter(|| board.has_run(Player::A)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
