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

# Run AI tournament example
cargo run --example ai_game --release

# Build for WebAssembly and deploy
./build_web_version.sh

# Code quality
cargo clippy
cargo fmt --check
```

## Architecture

The codebase is organized into three cleanly separated layers:

### Game Logic (`src/yinsh/`)
Pure game state and rules with no GUI dependencies. Highly testable.

- **`core.rs`**: Hexagonal coordinate system using cube/axial coords. Board radius 4.6. `Coord`, `Direction`, `Player` types.
- **`board.rs`**: Board state with dual storage (2D array for O(1) lookup + Vec for iteration). Ring/marker operations, run detection via `check_run()`.
- **`game_state.rs`**: State machine with `TurnMode` enum (RingPlacement, MarkerPlacement, RingMovement, RunRemoval, RingRemoval, Wait variants). `Move` enum for all actions. YAML serialization support.

### AI (`src/ai/`)
Minimax strategy implementation. Depends only on game logic.

- **`game.rs`**: Implements `minimax::Game` trait. `possible_moves()` generates legal moves per turn mode.
- **`evaluator.rs`**: `Heuristic` trait and `YinshEvaluator` wrapper for minimax.
- **`heuristics.rs`**: `SimpleHeuristic` with weighted evaluation factors (points, markers, controlled markers, accessible fields).

### GUI (`src/gui/`)
Bevy plugin architecture for interactive UI.

- **`state_update.rs`**: Game state mutations and `InteractionState` management
- **`interaction.rs`**: Mouse input, selection, move execution
- **`graphics.rs`**: Rendering, colors (human=yellow, AI=magenta), mesh generation
- **`ai.rs`**: Async AI computation via bevy_async_task
- **`history.rs`**: Move history (desktop only, uses `#[cfg(not(target_arch = "wasm32"))]`)

### Key Patterns

- **State Machine**: Game phases tracked via `TurnMode` enum. Each phase has corresponding `Move` variants.
- **Interaction State**: Separate `InteractionState` resource tracks UI state (available moves, selections, waiting states).
- **Pluggable Heuristics**: AI evaluation uses trait-based heuristics for easy experimentation.
- **Conditional Compilation**: WASM builds exclude history features and modify event loop behavior.

## Testing

Tests in `tests/` cover board operations and AI move generation. Test game states are serialized as YAML files (e.g., `tests/midgame_1.yml`).
