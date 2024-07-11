use std::time::Duration;

use bevy::{
    core_pipeline::bloom::BloomSettings,
    prelude::*,
    render::view::RenderLayers,
    sprite::{MaterialMesh2dBundle, Mesh2dHandle},
};

use yinsh::{Coord, Player};

use super::{
    board::{BoardElement, Marker, Ring},
    grid::draw_grid,
    PLAYER_HUMAN,
};

pub const FOREGROUND_RENDER_LAYER: RenderLayers = RenderLayers::layer(2);
pub const BACKGROUND_RENDER_LAYER: RenderLayers = RenderLayers::layer(1);

pub const COLOR_GRID: Color = Color::hsl(0.0, 0.0, 0.3);
pub const COLOR_BACKGROUND: Color = Color::hsl(0.0, 0.0, 0.4);
pub const COLOR_RING_MOVEMENT_INDICATOR: Color = Color::hsla(0.0, 0.0, 1.5, 0.1);

pub const ANIMATION_DURATION: Duration = Duration::from_millis(300);

pub const SPACING: f32 = 90.0;

#[derive(Component)]
pub struct MainCamera;

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

fn setup_graphics(
    mut commands: Commands,
    mut config_store: ResMut<GizmoConfigStore>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    commands.insert_resource(ClearColor(COLOR_BACKGROUND));

    // Render layer 1 is for the grid
    commands.spawn((
        Camera2dBundle {
            camera: Camera {
                hdr: true,
                order: 1,
                ..default()
            },
            ..default()
        },
        BloomSettings::default(),
        BACKGROUND_RENDER_LAYER,
    ));

    // Render layer 2 is for the board elements
    commands.spawn((
        Camera2dBundle {
            camera: Camera {
                hdr: true,
                order: 2,
                ..default()
            },
            ..default()
        },
        BloomSettings::default(),
        FOREGROUND_RENDER_LAYER,
        MainCamera,
    ));

    commands.insert_resource(PlayerColors {
        human: materials.add(Color::srgba(1.5, 1.5, 1.5, 1.0)),
        human_highlighted: materials.add(Color::srgba(4., 4., 4., 1.0)),
        human_transparent: materials.add(Color::srgba(1.5, 1.5, 1.5, 0.1)),
        ai: materials.add(Color::srgba(0.0, 0.0, 0.0, 1.0)),
    });

    let (config, _) = config_store.config_mut::<DefaultGizmoConfigGroup>();
    config.render_layers = BACKGROUND_RENDER_LAYER;
}

pub fn plugin(app: &mut App) {
    app.insert_resource(Msaa::Sample8)
        .add_systems(PreStartup, setup_graphics)
        .add_systems(Update, draw_grid);
}
