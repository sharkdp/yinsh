# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Yinsh is a Rust implementation of the board game [Yinsh](http://en.wikipedia.org/wiki/Yinsh) with a minimax-based AI player and Bevy-powered GUI. Supports both native desktop and WebAssembly (playable at http://david-peter.de/yinsh).

## Build and Development Commands

```bash
# Run native application
cargo run

# Build release
cargo build --release

# Run tests
cargo test

# Run tests with output
cargo test -- --nocapture

# Run benchmarks
cargo bench

# Run AI tournament
cargo run -p yinsh_ai_eval --release

# Build for WebAssembly and deploy
./build_web_version.sh

# Code quality
cargo clippy
cargo fmt --check
```

## Architecture

The project is organized as a Cargo workspace with four crates:

### `yinsh` - Core Game Logic
Pure game state and rules with no GUI dependencies. Highly testable.

- **`coord.rs`**: Hexagonal coordinate system using axial coords. Board radius 4.6. `Coord` type with arithmetic ops.
- **`direction.rs`**: `Direction` enum for the six hex directions, `DIRECTIONS` and `AXES` constants.
- **`player.rs`**: `Player` enum (A/B).
- **`board.rs`**: Board state with dual storage (2D array for O(1) lookup + Vec for iteration). Ring/marker operations, run detection via `check_run()`.
- **`game_state.rs`**: State machine with `TurnMode` enum (RingPlacement, MarkerPlacement, RingMovement, RunRemoval, RingRemoval, Wait variants). `Move` enum for all actions. YAML serialization support.

### `yinsh_ai` - AI Player
Minimax strategy implementation. Depends only on `yinsh`.

- **`game.rs`**: Implements `minimax::Game` trait. `possible_moves()` generates legal moves per turn mode.
- **`evaluator.rs`**: `Heuristic` trait and `YinshEvaluator` wrapper for minimax.
- **`heuristics.rs`**: `SimpleHeuristic` with weighted evaluation factors (points, markers, controlled markers, accessible fields).

### `yinsh_gui` - Bevy GUI
Bevy plugin architecture for interactive UI. Produces the `yinsh` binary.

- **`state_update.rs`**: Game state mutations and `InteractionState` management
- **`interaction.rs`**: Mouse input, selection, move execution
- **`graphics.rs`**: Rendering, colors (human=yellow, AI=magenta), mesh generation
- **`ai.rs`**: Async AI computation via bevy_async_task
- **`history.rs`**: Move history (desktop only, uses `#[cfg(not(target_arch = "wasm32"))]`)

### `yinsh_ai_eval` - AI Tournament Runner
Runs AI vs AI matches for heuristic evaluation. Produces the `yinsh-ai-eval` binary.

### Key Patterns

- **State Machine**: Game phases tracked via `TurnMode` enum. Each phase has corresponding `Move` variants.
- **Interaction State**: Separate `InteractionState` resource tracks UI state (available moves, selections, waiting states).
- **Pluggable Heuristics**: AI evaluation uses trait-based heuristics for easy experimentation.
- **Conditional Compilation**: WASM builds exclude history features and modify event loop behavior.

## Testing

Tests in `crates/yinsh/tests/` cover board operations. Tests in `crates/yinsh_ai/tests/` cover AI evaluation. Test game states are serialized as YAML files.
