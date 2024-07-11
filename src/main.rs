pub mod yinsh;

use bevy::{
    core_pipeline::bloom::BloomSettings,
    diagnostic::{FrameTimeDiagnosticsPlugin, LogDiagnosticsPlugin},
    prelude::*,
    render::view::RenderLayers,
    sprite::{MaterialMesh2dBundle, Mesh2dHandle},
    window::WindowMode,
};
use yinsh::Coord;

#[derive(Component)]
struct BoardElement(Coord);

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(WindowPlugin {
                primary_window: Some(Window {
                    title: "Yinsh".into(),
                    name: Some("yinsh".into()),
                    resolution: (960., 960.).into(),
                    mode: WindowMode::Windowed,
                    ..default()
                }),
                ..default()
            }),
            LogDiagnosticsPlugin::default(),
            FrameTimeDiagnosticsPlugin,
        ))
        .add_systems(Startup, setup)
        .add_systems(
            Update,
            (draw_grid, board_element_positioning, keyboard_control),
        )
        .insert_resource(ClearColor(Color::hsl(0.0, 0.0, 0.1)))
        .run();
}

fn setup(
    mut commands: Commands,
    mut config_store: ResMut<GizmoConfigStore>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
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
        RenderLayers::layer(1),
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
        RenderLayers::layer(2),
    ));

    let (config, _) = config_store.config_mut::<DefaultGizmoConfigGroup>();
    config.render_layers = RenderLayers::layer(1);

    let color = Color::hsl(0., 0., 2.0);

    let coord = Coord { x: 2, y: -1 };

    commands.spawn((
        MaterialMesh2dBundle {
            mesh: Mesh2dHandle(meshes.add(Circle {
                radius: SPACING / 5.,
            })),
            material: materials.add(color),
            ..default()
        },
        BoardElement(coord),
        RenderLayers::layer(2),
    ));

    commands.spawn((
        MaterialMesh2dBundle {
            mesh: Mesh2dHandle(meshes.add(Annulus::new(SPACING / 4., SPACING / 3.))),
            material: materials.add(color),
            ..default()
        },
        BoardElement(coord),
        RenderLayers::layer(2),
    ));
}

const SPACING: f32 = 90.0;

fn screen_point(coord: Coord) -> Vec3 {
    Vec3::new(
        SPACING * (0.5 * 3_f32.sqrt() * coord.x as f32),
        SPACING * (-coord.y as f32 + 0.5 * coord.x as f32),
        0.,
    )
}

fn draw_grid(mut gizmos: Gizmos) {
    let grid_line_color = Color::hsl(0.0, 0.0, 0.6);

    // Draw lines parallel to y-axis
    for x in -5i8..=5i8 {
        let coords: Vec<_> = (-5..=5)
            .map(|y| Coord { x, y })
            .filter(|c| c.is_inside_board())
            .collect();

        let min_y = coords.iter().map(|c| c.y).min().unwrap();
        let max_y = coords.iter().map(|c| c.y).max().unwrap();

        let start = screen_point(Coord { x, y: min_y });
        let end = screen_point(Coord { x, y: max_y });
        gizmos.line(start, end, grid_line_color);
    }

    // Draw lines parallel to x-axis
    for y in -5i8..=5i8 {
        let coords: Vec<_> = (-5..=5)
            .map(|x| Coord { x, y })
            .filter(|c| c.is_inside_board())
            .collect();

        let min_x = coords.iter().map(|c| c.x).min().unwrap();
        let max_x = coords.iter().map(|c| c.x).max().unwrap();

        let start = screen_point(Coord { x: min_x, y });
        let end = screen_point(Coord { x: max_x, y });
        gizmos.line(start, end, grid_line_color);
    }

    // Draw lines parallel to y = x
    for d in -5i8..=5i8 {
        let coords: Vec<_> = (-5..=5)
            .map(|x| Coord { x, y: x + d })
            .filter(|c| c.is_inside_board())
            .collect();

        let min = coords.iter().map(|c| c.x).min().unwrap();
        let max = coords.iter().map(|c| c.x).max().unwrap();

        let start = screen_point(Coord { x: min, y: min + d });
        let end = screen_point(Coord { x: max, y: max + d });
        gizmos.line(start, end, grid_line_color);
    }
}

fn board_element_positioning(mut query: Query<(&BoardElement, &mut Transform)>) {
    for (BoardElement(coord), mut transform) in query.iter_mut() {
        transform.translation = screen_point(*coord);
    }
}

fn keyboard_control(keyboard: Res<ButtonInput<KeyCode>>, mut exit: EventWriter<AppExit>) {
    if keyboard.just_pressed(KeyCode::Escape) || keyboard.just_pressed(KeyCode::KeyQ) {
        exit.send(AppExit::Success);
    }
}
