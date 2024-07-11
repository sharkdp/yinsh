use bevy::prelude::*;
use bevy::tasks::futures_lite::future;
use bevy::tasks::{block_on, Task};

use yinsh::Action;

use crate::PlayerActionEvent;

use super::PLAYER_AI;

#[derive(Resource)]
pub struct AiTask(Option<Task<Action>>);

impl AiTask {
    pub fn new() -> Self {
        Self(None)
    }

    pub fn is_running(&self) -> bool {
        self.0.is_some()
    }

    pub fn start(&mut self, task: Task<Action>) {
        self.0 = Some(task);
    }

    pub fn cancel(&mut self) {
        self.0 = None;
    }

    pub fn get_status(&mut self) -> Option<Action> {
        block_on(future::poll_once(self.0.as_mut().unwrap()))
    }
}

#[derive(Resource)]
pub struct AiPlayerStrength(pub usize);

pub fn wait_for_ai_move(
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
