use std::ops::{Deref, DerefMut};

use yinsh::{Action, Coord, Player, TurnMode};

use bevy::{prelude::*, utils::HashMap};

use crate::gui::PLAYER_HUMAN;

use super::{ai::AiComputationEvent, board_update_event::BoardUpdateEvent, PLAYER_AI};

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

#[derive(Event)]
pub struct PlayerActionEvent(pub Player, pub Action);

fn state_update(
    mut player_action_events: EventReader<PlayerActionEvent>,
    mut game_state: ResMut<GameState>,
    mut interaction_state: ResMut<InteractionState>,
    mut ai_computation_events: EventWriter<AiComputationEvent>,
    mut board_update_events: EventWriter<BoardUpdateEvent>,
) {
    for PlayerActionEvent(player, action) in player_action_events.read() {
        assert!(player == &game_state.active_player);

        match action {
            Action::PlaceRing(coord) => {
                board_update_events.send(BoardUpdateEvent::AddRing(*coord, *player));
            }
            Action::PlaceMarker(coord) => {
                board_update_events.send(BoardUpdateEvent::AddMarker(*coord, *player));
            }
            Action::MoveRing(start, end) => {
                board_update_events.send(BoardUpdateEvent::MoveRing(*start, *end));
                board_update_events.send(BoardUpdateEvent::FlipMarkers(
                    *start,
                    *end,
                    Coord::between(*start, *end)
                        .into_iter()
                        .filter(|&coord| game_state.board.has_marker_at(coord))
                        .collect(),
                ));
            }
            Action::RemoveRun(seed) => {
                board_update_events.send(BoardUpdateEvent::RemoveRun(
                    game_state.board.run_coords_from(*seed).unwrap(),
                ));
            }
            Action::RemoveRing(coord) => {
                board_update_events.send(BoardUpdateEvent::RemoveRing(*coord));
            }
            Action::Wait => {}
        }

        game_state.transition(action);

        if game_state.active_player == PLAYER_AI && game_state.winner().is_none() {
            ai_computation_events.send(AiComputationEvent::Start(PLAYER_AI, game_state.clone()));
        }
    }

    *interaction_state = InteractionState::from_game_state(&game_state);
}

pub fn plugin(app: &mut App) {
    let initial_game_state = GameState::initial();
    app.insert_resource(InteractionState::from_game_state(&initial_game_state))
        .insert_resource(initial_game_state)
        .add_event::<PlayerActionEvent>()
        .add_event::<BoardUpdateEvent>()
        .add_systems(Update, state_update.in_set(StateUpdateSet));
}
