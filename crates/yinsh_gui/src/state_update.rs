use std::ops::{Deref, DerefMut};

use std::collections::HashMap;

use yinsh::{Coord, Move, Player, TurnMode};

use bevy::ecs::message::{MessageReader, MessageWriter};
use bevy::prelude::*;

use crate::PLAYER_HUMAN;

use crate::{
    PLAYER_AI, ai::AiComputationEvent, board::BoardElement, board_update_event::BoardUpdateEvent,
    interaction::CursorElement,
};

#[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
pub struct StateUpdateSet;

#[derive(Resource)]
pub struct GameState(yinsh::GameState);

impl Deref for GameState {
    type Target = yinsh::GameState;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for GameState {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl GameState {
    pub fn initial() -> Self {
        Self(yinsh::GameState::initial())
    }
}

#[derive(Resource, Default)]
pub struct UndoHistory(pub Vec<yinsh::GameState>);

#[derive(Resource)]
pub enum InteractionState {
    RingPlacement(Vec<Coord>),
    MarkerPlacement(Vec<Coord>),
    RingMovement(Coord, Vec<Coord>),
    RunRemoval {
        all_run_coords: Vec<Coord>,
        run_from_seed: HashMap<Coord, Vec<Coord>>,
    },
    RingRemoval(Vec<Coord>),
    AutoMove,
    WaitForAI,
    Winner(Player),
}

impl InteractionState {
    pub fn from_game_state(game_state: &yinsh::GameState) -> Self {
        if let Some(winner) = game_state.winner() {
            Self::Winner(winner)
        } else if game_state.active_player == PLAYER_AI {
            Self::WaitForAI
        } else {
            match game_state.turn_mode {
                TurnMode::RingPlacement => {
                    Self::RingPlacement(game_state.board.free_coords().collect())
                }
                TurnMode::MarkerPlacement => {
                    Self::MarkerPlacement(game_state.board.marker_moves(PLAYER_HUMAN).collect())
                }
                TurnMode::RingMovement(start) => {
                    Self::RingMovement(start, game_state.board.ring_moves(start))
                }
                TurnMode::RunRemoval(_) => {
                    let all_run_coords = game_state.board.run_coords(PLAYER_HUMAN);
                    Self::RunRemoval {
                        all_run_coords: all_run_coords.clone(),
                        run_from_seed: all_run_coords
                            .into_iter()
                            .map(|seed| (seed, game_state.board.run_coords_from(seed).unwrap()))
                            .collect(),
                    }
                }
                TurnMode::RingRemoval(_) => {
                    Self::RingRemoval(game_state.board.ring_coords(PLAYER_HUMAN).collect())
                }
                TurnMode::WaitForRunRemoval(_)
                | TurnMode::WaitForMarkerPlacement
                | TurnMode::WaitForRingMovement(_)
                | TurnMode::WaitForRingRemoval(_) => Self::AutoMove,
            }
        }
    }
}

pub fn restore_board_from_game_state(
    game_state: &yinsh::GameState,
    commands: &mut Commands,
    board_update_events: &mut MessageWriter<BoardUpdateEvent>,
    q_board_elements: &Query<Entity, (With<BoardElement>, Without<CursorElement>)>,
) {
    // Despawn all board elements
    for entity in q_board_elements.iter() {
        commands.entity(entity).despawn();
    }

    // Respawn board elements from game state
    for p in [Player::A, Player::B] {
        for coord in game_state.board.ring_coords(p) {
            board_update_events.write(BoardUpdateEvent::AddRing(coord, p));
        }
        for coord in game_state.board.marker_coords(p) {
            board_update_events.write(BoardUpdateEvent::AddMarker(coord, p));
        }
    }
}

#[derive(Message)]
pub struct PlayerMoveEvent(pub Player, pub Move);

fn state_update(
    mut player_move_events: MessageReader<PlayerMoveEvent>,
    mut game_state: ResMut<GameState>,
    mut interaction_state: ResMut<InteractionState>,
    mut undo_history: ResMut<UndoHistory>,
    mut ai_computation_events: MessageWriter<AiComputationEvent>,
    mut board_update_events: MessageWriter<BoardUpdateEvent>,
) {
    for PlayerMoveEvent(player, player_move) in player_move_events.read() {
        let player = *player;
        let player_move = player_move.clone();
        assert_eq!(player, game_state.active_player);

        // Save state before human's turn-starting moves (for undo)
        if player == PLAYER_HUMAN
            && matches!(player_move, Move::PlaceMarker(_) | Move::PlaceRing(_))
        {
            undo_history.0.push(game_state.0.clone());
        }

        match &player_move {
            Move::PlaceRing(coord) => {
                board_update_events.write(BoardUpdateEvent::AddRing(*coord, player));
            }
            Move::PlaceMarker(coord) => {
                board_update_events.write(BoardUpdateEvent::AddMarker(*coord, player));
            }
            Move::MoveRing(start, end) => {
                board_update_events.write(BoardUpdateEvent::MoveRing(*start, *end));
                board_update_events.write(BoardUpdateEvent::FlipMarkers(
                    *start,
                    *end,
                    Coord::between(*start, *end)
                        .into_iter()
                        .filter(|&coord| game_state.board.has_marker_at(coord))
                        .collect(),
                ));
            }
            Move::RemoveRun(seed) => {
                board_update_events.write(BoardUpdateEvent::RemoveRun(
                    game_state.board.run_coords_from(*seed).unwrap(),
                ));
            }
            Move::RemoveRing(coord) => {
                board_update_events.write(BoardUpdateEvent::RemoveRing(*coord));
            }
            Move::Wait => {}
        }

        game_state.perform_move(&player_move);

        if game_state.active_player == PLAYER_AI && game_state.winner().is_none() {
            ai_computation_events.write(AiComputationEvent::Start(PLAYER_AI, game_state.clone()));
        }
    }

    *interaction_state = InteractionState::from_game_state(&game_state);
}

pub fn plugin(app: &mut App) {
    let initial_game_state = GameState::initial();
    app.insert_resource(InteractionState::from_game_state(&initial_game_state))
        .insert_resource(initial_game_state)
        .init_resource::<UndoHistory>()
        .add_message::<PlayerMoveEvent>()
        .add_message::<BoardUpdateEvent>()
        .add_systems(Update, state_update.in_set(StateUpdateSet));
}
