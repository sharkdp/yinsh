use std::time::Duration;

use bevy::{
    prelude::*,
    sprite::{MaterialMesh2dBundle, Mesh2dHandle},
};
use yinsh::{Coord, Player};

pub const ANIMATION_DURATION: Duration = Duration::from_millis(300);

use crate::FOREGROUND_RENDER_LAYER;

use super::{
    board::{BoardElement, Marker, Ring},
    PLAYER_HUMAN,
};

pub const SPACING: f32 = 90.0;

pub fn screen_point(coord: Coord) -> Vec3 {
    Vec3::new(
        SPACING * (0.5 * 3_f32.sqrt() * coord.x as f32),
        SPACING * (-coord.y as f32 + 0.5 * coord.x as f32),
        0.,
    )
}

#[derive(Resource)]
pub struct PlayerColors {
    pub human: Handle<ColorMaterial>,
    pub human_highlighted: Handle<ColorMaterial>,
    pub human_transparent: Handle<ColorMaterial>,
    pub ai: Handle<ColorMaterial>,
}

pub fn ring_mesh(
    meshes: &mut Assets<Mesh>,
    color_material: Handle<ColorMaterial>,
    visibility: Visibility,
) -> MaterialMesh2dBundle<ColorMaterial> {
    MaterialMesh2dBundle {
        mesh: Mesh2dHandle(meshes.add(Annulus::new(SPACING / 4., SPACING / 3.))),
        material: color_material,
        visibility,
        ..default()
    }
}

pub fn marker_mesh(
    meshes: &mut Assets<Mesh>,
    color_material: Handle<ColorMaterial>,
    visibility: Visibility,
) -> MaterialMesh2dBundle<ColorMaterial> {
    MaterialMesh2dBundle {
        mesh: Mesh2dHandle(meshes.add(Circle::new(SPACING / 5.))),
        material: color_material,
        visibility,
        ..default()
    }
}

pub fn spawn_ring(
    commands: &mut Commands,
    meshes: &mut Assets<Mesh>,
    player_colors: &PlayerColors,
    coord: Coord,
    player: Player,
) {
    let color = if player == PLAYER_HUMAN {
        player_colors.human.clone()
    } else {
        player_colors.ai.clone()
    };

    commands.spawn((
        ring_mesh(meshes, color, Visibility::Visible),
        BoardElement(coord, player),
        Ring,
        FOREGROUND_RENDER_LAYER,
    ));
}

pub fn spawn_marker(
    commands: &mut Commands,
    meshes: &mut Assets<Mesh>,
    player_colors: &PlayerColors,
    coord: Coord,
    player: Player,
) {
    let color = if player == PLAYER_HUMAN {
        player_colors.human.clone()
    } else {
        player_colors.ai.clone()
    };

    commands.spawn((
        marker_mesh(meshes, color, Visibility::Visible),
        BoardElement(coord, player),
        Marker,
        FOREGROUND_RENDER_LAYER,
    ));
}
