use bevy::prelude::*;
use bevy::tasks::futures_lite::future;
use bevy::tasks::{block_on, AsyncComputeTaskPool, Task};

use yinsh::{Action, GameState, TurnMode};

use super::graphics::ANIMATION_DURATION;
use super::state_update::{PlayerActionEvent, StateUpdateSet};
use super::PLAYER_AI;

#[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
pub struct AiSet;

#[derive(Resource)]
pub struct AiPlayerStrength(pub usize);

#[derive(Event)]
pub enum AiComputationEvent {
    Start(GameState),
    Cancel,
}

#[derive(Resource)]
struct AiTask(Option<Task<Action>>);

impl AiTask {
    fn new() -> Self {
        Self(None)
    }

    fn is_running(&self) -> bool {
        self.0.is_some()
    }

    fn start(&mut self, task: Task<Action>) {
        self.0 = Some(task);
    }

    fn cancel(&mut self) {
        self.0 = None;
    }

    fn get_status(&mut self) -> Option<Action> {
        block_on(future::poll_once(self.0.as_mut().unwrap()))
    }
}

fn manage_ai_tasks(
    mut task: ResMut<AiTask>,
    mut events: EventReader<AiComputationEvent>,
    strength: Res<AiPlayerStrength>,
) {
    for event in events.read() {
        match event {
            AiComputationEvent::Start(ref game_state) => {
                let task_pool = AsyncComputeTaskPool::get();

                let game_state = game_state.clone();
                let search_depth = strength.0;
                task.start(task_pool.spawn(async move {
                    // TODO! This is a hack to make sure the AI takes at least as long as
                    // the animation.
                    if matches!(game_state.turn_mode, TurnMode::MarkerPlacement) {
                        std::thread::sleep(ANIMATION_DURATION);
                    }

                    yinsh::get_ai_player_action(search_depth, &game_state)
                }));
            }
            AiComputationEvent::Cancel => {
                task.cancel();
            }
        }
    }
}

fn perform_ai_actions(
    mut task: ResMut<AiTask>,
    mut player_action_events: EventWriter<PlayerActionEvent>,
) {
    if !task.is_running() {
        return;
    }

    let status = task.get_status();

    if status.is_none() {
        return;
    }

    task.cancel();

    let action = status.unwrap();

    player_action_events.send(PlayerActionEvent(PLAYER_AI, action));
}

pub fn plugin(app: &mut App) {
    app.insert_resource(AiTask::new())
        .insert_resource(AiPlayerStrength(9))
        .add_event::<AiComputationEvent>()
        .add_systems(
            Update,
            (manage_ai_tasks, perform_ai_actions)
                .chain()
                .in_set(AiSet)
                .after(StateUpdateSet),
        );
}
