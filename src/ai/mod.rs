use std::iter;

use minimax::{Evaluation, Evaluator, Game, Negamax, Strategy, Winner};

use crate::{
    yinsh::{Action, GameState, TurnMode},
    Player,
};

pub struct Yinsh;

impl Game for Yinsh {
    type S = GameState;

    type M = Action;

    fn generate_moves(state: &Self::S, moves: &mut Vec<Self::M>) {
        moves.extend(possible_actions(state));
    }

    fn apply(state: &mut Self::S, m: Self::M) -> Option<Self::S> {
        let mut new_state = state.clone(); // TODO: is this necessary?
        new_state.transition(&m);
        Some(new_state) // TODO: we can avoid cloning here by returning None and implementing undo
    }

    fn get_winner(state: &Self::S) -> Option<Winner> {
        match state.winner() {
            Some(p) if p == state.active_player => Some(Winner::PlayerToMove),
            Some(_) => Some(Winner::PlayerJustMoved),
            None => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SimpleHeuristic;

impl Evaluator for SimpleHeuristic {
    type G = Yinsh;

    fn evaluate(&self, state: &GameState) -> Evaluation {
        // TODO: here we could match on the turn mode and potentially skip ahead if we are in a waiting state.

        type Score = Evaluation;

        // Evaluate position from perspective of player A. If the active
        // player is B instead, we negate in the end.
        let score_points = (state.points_a as Score) - (state.points_b as Score);

        let score_markers = (state.board.num_markers(Player::A) as Score)
            - (state.board.num_markers(Player::B) as Score);

        let score = 1000 * score_points + 10 * score_markers;

        if state.active_player == Player::A {
            score
        } else {
            -score
        }
    }
}

pub fn possible_actions<'a>(state: &'a GameState) -> Box<dyn Iterator<Item = Action> + 'a> {
    match state.turn_mode {
        TurnMode::RingPlacement => Box::new(state.board.free_coords().map(Action::PlaceRing)),
        TurnMode::MarkerPlacement => Box::new(
            state
                .board
                .marker_moves(state.active_player)
                .map(Action::PlaceMarker),
        ),
        TurnMode::RingMovement(start) => Box::new(
            state
                .board
                .ring_moves(start)
                .into_iter()
                .map(move |end| Action::MoveRing(start, end)),
        ),
        TurnMode::RunRemoval(_) => Box::new(
            state
                .board
                .run_coords(state.active_player)
                .into_iter()
                .map(Action::RemoveRun), // TODO: this produces too many moves
        ),
        TurnMode::RingRemoval(_) => Box::new(
            state
                .board
                .ring_coords(state.active_player)
                .map(Action::RemoveRing),
        ),
        TurnMode::WaitForRunRemoval(_)
        | TurnMode::WaitForMarkerPlacement
        | TurnMode::WaitForRingMovement(_)
        | TurnMode::WaitForRingRemoval(_) => Box::new(iter::once(Action::Wait)),
    }
}

trait IsYinshGameState {
    fn from_gamestate(s: &GameState) -> &Self;
}

impl IsYinshGameState for GameState {
    fn from_gamestate(s: &GameState) -> &Self {
        s
    }
}

trait IsYinshAction {
    fn to_action(s: Self) -> Action;
}

impl IsYinshAction for Action {
    fn to_action(s: Self) -> Action {
        s
    }
}

struct YinshAi<Heuristic: Evaluator + Copy> {
    search_depth: usize,
    heuristic: Heuristic,
}

impl<Heuristic: Evaluator + Copy> YinshAi<Heuristic>
where
    <Heuristic::G as Game>::S: Clone,
    <Heuristic::G as Game>::S: IsYinshGameState,
    <Heuristic::G as Game>::M: IsYinshAction,
{
    pub fn new(search_depth: usize, heuristic: Heuristic) -> Self {
        Self {
            search_depth,
            heuristic,
        }
    }

    pub fn choose_action(&mut self, state: &GameState) -> Option<Action> {
        let depth: u8 = if matches!(state.turn_mode, TurnMode::RingPlacement) {
            4
        } else {
            self.search_depth.try_into().unwrap()
        };

        let mut strategy = Negamax::new(self.heuristic, depth);
        let action = strategy
            .choose_move(&<<Heuristic::G as Game>::S as IsYinshGameState>::from_gamestate(state))
            .unwrap();

        dbg!(strategy.root_value());

        Some(<<Heuristic as Evaluator>::G as Game>::M::to_action(action))
    }
}

pub fn get_ai_player_action(search_depth: usize, state: &GameState) -> Action {
    // Early return if the only thing we can do is wait. Would be great
    // if this could be handled by 'minimax' itself (if there is only one
    // possible mobe in choose_move, return that immediately).
    match state.turn_mode {
        TurnMode::WaitForRunRemoval(_)
        | TurnMode::WaitForRingMovement(_)
        | TurnMode::WaitForRingRemoval(_)
        | TurnMode::WaitForMarkerPlacement => {
            return Action::Wait;
        }
        _ => {}
    }

    let mut ai = YinshAi::new(search_depth, SimpleHeuristic {});
    ai.choose_action(state).unwrap()
}
