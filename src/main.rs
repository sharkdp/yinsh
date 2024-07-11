pub mod yinsh;

use bevy::tasks::futures_lite::future;
use bevy::tasks::{block_on, AsyncComputeTaskPool, Task};
use bevy::{
    core_pipeline::bloom::BloomSettings,
    diagnostic::{FrameTimeDiagnosticsPlugin, LogDiagnosticsPlugin},
    prelude::*,
    render::view::RenderLayers,
    sprite::{MaterialMesh2dBundle, Mesh2dHandle},
    window::{PresentMode, PrimaryWindow, WindowMode},
};
use yinsh::{Coord, Player};

#[derive(Component)]
struct BoardElement(Coord, Player);

#[derive(Component)]
struct Ring;

#[derive(Component)]
struct Marker;

#[derive(Component)]
struct MainCamera;

#[derive(Component)]
struct CursorElement;

#[derive(Resource)]
pub enum GameState {
    PlaceRing,
    PlaceMarker,
    WaitForAI,
}

#[derive(Resource)]
pub struct MouseCursorCoord(Option<Coord>);

const PLAYER_HUMAN: Player = Player::A;
const PLAYER_COMPUTER: Player = Player::B;

#[derive(Resource)]
struct PlayerColors {
    human: Handle<ColorMaterial>,
    computer: Handle<ColorMaterial>,
}

const BACKGROUND_RENDER_LAYER: RenderLayers = RenderLayers::layer(1);
const FOREGROUND_RENDER_LAYER: RenderLayers = RenderLayers::layer(2);

#[derive(Event)]
struct BoardChangedEvent;

#[derive(Resource)]
struct AiTask(Option<Task<Coord>>);

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins.set(WindowPlugin {
                primary_window: Some(Window {
                    title: "Yinsh".into(),
                    name: Some("yinsh".into()),
                    resolution: (960., 960.).into(),
                    mode: WindowMode::Windowed,
                    present_mode: PresentMode::Immediate,
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
            (
                start_ai_move,
                wait_for_ai_move,
                draw_grid,
                keyboard_control,
                mouse_cursor_system,
                mouse_interaction_system,
                move_and_colorize_board_elements,
            )
                .chain(),
        )
        .insert_resource(ClearColor(Color::hsl(0.0, 0.0, 0.1)))
        .insert_resource(Msaa::default())
        .insert_resource(GameState::PlaceRing)
        .insert_resource(MouseCursorCoord(None))
        .insert_resource(AiTask(None))
        .add_event::<BoardChangedEvent>()
        .run();
}

fn ring_mesh(
    meshes: &mut Assets<Mesh>,
    color_material: Handle<ColorMaterial>,
) -> MaterialMesh2dBundle<ColorMaterial> {
    MaterialMesh2dBundle {
        mesh: Mesh2dHandle(meshes.add(Annulus::new(SPACING / 4., SPACING / 3.))),
        material: color_material,
        ..default()
    }
}

fn marker_mesh(
    meshes: &mut Assets<Mesh>,
    color_material: Handle<ColorMaterial>,
) -> MaterialMesh2dBundle<ColorMaterial> {
    MaterialMesh2dBundle {
        mesh: Mesh2dHandle(meshes.add(Circle::new(SPACING / 5.))),
        material: color_material,
        ..default()
    }
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

    let human_transparent = materials.add(Color::srgba(2., 2., 2., 0.5));
    commands.insert_resource(PlayerColors {
        human: materials.add(Color::srgba(2., 2., 2., 1.0)),
        computer: materials.add(Color::srgba(0.0, 0.0, 0.0, 1.0)),
    });

    let (config, _) = config_store.config_mut::<DefaultGizmoConfigGroup>();
    config.render_layers = BACKGROUND_RENDER_LAYER;

    commands.spawn((
        ring_mesh(&mut meshes, human_transparent.clone()),
        BoardElement(Coord { x: 0, y: 0 }, PLAYER_HUMAN),
        Ring,
        CursorElement,
        FOREGROUND_RENDER_LAYER,
    ));

    commands.spawn((
        marker_mesh(&mut meshes, human_transparent),
        BoardElement(Coord { x: 0, y: 0 }, PLAYER_HUMAN),
        Marker,
        CursorElement,
        FOREGROUND_RENDER_LAYER,
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

fn start_ai_move(
    mut board_changed_events: EventReader<BoardChangedEvent>,
    mut task: ResMut<AiTask>,
    mut game_state: ResMut<GameState>,
) {
    if board_changed_events.read().count() == 0 {
        return;
    }

    *game_state = GameState::WaitForAI;

    let task_pool = AsyncComputeTaskPool::get();

    task.0 = Some(task_pool.spawn(async move {
        std::thread::sleep(std::time::Duration::from_secs(2));
        Coord { x: 0, y: 0 }
    }));
}

fn wait_for_ai_move(
    mut game_state: ResMut<GameState>,
    q_rings: Query<&BoardElement, (With<Ring>, Without<CursorElement>)>,
    mut task: ResMut<AiTask>,
) {
    if task.0.is_none() {
        return;
    }

    let status = block_on(future::poll_once(task.0.as_mut().unwrap()));

    if status.is_none() {
        return;
    }

    task.0 = None;

    let coord = status.unwrap();

    let rings_human: Vec<_> = q_rings
        .iter()
        .filter(|e| e.1 == PLAYER_HUMAN)
        .map(|e| e.0)
        .collect();
    let rings_computer: Vec<_> = q_rings
        .iter()
        .filter(|e| e.1 == PLAYER_COMPUTER)
        .map(|e| e.0)
        .collect();

    if rings_human.len() < 3 {
        *game_state = GameState::PlaceRing;
    } else {
        *game_state = GameState::PlaceMarker;
    }
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

fn move_and_colorize_board_elements(
    mut query: Query<(&BoardElement, &mut Transform, &mut Handle<ColorMaterial>)>,
    player_colors: Res<PlayerColors>,
) {
    for (BoardElement(coord, player), mut transform, mut color_material) in query.iter_mut() {
        transform.translation = screen_point(*coord);

        *color_material = if player == &PLAYER_HUMAN {
            player_colors.human.clone()
        } else {
            player_colors.computer.clone()
        };
    }
}

fn mouse_cursor_system(
    q_window: Query<&Window, With<PrimaryWindow>>,
    q_camera: Query<(&Camera, &GlobalTransform), With<MainCamera>>,
    mut cursor_ring: Query<
        (&mut BoardElement, &mut Visibility),
        (With<Ring>, Without<Marker>, With<CursorElement>),
    >,
    mut cursor_marker: Query<
        (&mut BoardElement, &mut Visibility),
        (With<Marker>, Without<Ring>, With<CursorElement>),
    >,
    q_rings: Query<&BoardElement, (With<Ring>, Without<CursorElement>)>,
    game_state: Res<GameState>,
    mut mouse_cursor_coord: ResMut<MouseCursorCoord>,
) {
    if let Some(cursor_position) = q_window.single().cursor_position() {
        let (camera, camera_transform) = q_camera.single();

        if let Some(cursor_position) = camera
            .viewport_to_world(camera_transform, cursor_position)
            .map(|ray| ray.origin.truncate())
        {
            let rings_human = q_rings
                .iter()
                .filter(|e| e.1 == PLAYER_HUMAN)
                .map(|e| e.0)
                .collect::<Vec<_>>();

            let cursor_coord = yinsh::all_coords()
                .into_iter()
                .min_by_key(|c| {
                    let screen_pos = screen_point(*c);
                    let cursor_pos = Vec3::new(cursor_position.x, cursor_position.y, 0.0);
                    let diff = screen_pos - cursor_pos;
                    diff.length_squared() as i32
                })
                .unwrap();

            let (mut cursor_ring_coord, mut cursor_ring_visibility) = cursor_ring.single_mut();
            *cursor_ring_visibility = Visibility::Hidden;

            let (mut cursor_marker_coord, mut cursor_marker_visibility) =
                cursor_marker.single_mut();
            *cursor_marker_visibility = Visibility::Hidden;

            match *game_state {
                GameState::PlaceRing => {
                    *cursor_ring_visibility = Visibility::Visible;
                    cursor_ring_coord.0 = cursor_coord;
                }
                GameState::PlaceMarker => {
                    if rings_human.contains(&cursor_coord) {
                        *cursor_marker_visibility = Visibility::Visible;
                        cursor_marker_coord.0 = cursor_coord;
                    }
                }
                GameState::WaitForAI => {}
            }

            mouse_cursor_coord.0 = Some(cursor_coord);
        }
    }
}

fn mouse_interaction_system(
    buttons: Res<ButtonInput<MouseButton>>,
    game_state: Res<GameState>,
    mouse_cursor_coord: Res<MouseCursorCoord>,
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    player_colors: Res<PlayerColors>,
    q_rings: Query<&BoardElement, (With<Ring>, Without<CursorElement>)>,
    mut board_changed_events: EventWriter<BoardChangedEvent>,
) {
    if let Some(mouse_cursor_coord) = mouse_cursor_coord.0 {
        if buttons.just_pressed(MouseButton::Left) {
            match *game_state {
                GameState::PlaceRing => {
                    commands.spawn((
                        ring_mesh(&mut meshes, player_colors.human.clone()),
                        BoardElement(mouse_cursor_coord, PLAYER_HUMAN),
                        Ring,
                        FOREGROUND_RENDER_LAYER,
                    ));
                    board_changed_events.send(BoardChangedEvent);
                }
                GameState::PlaceMarker => {
                    let rings_human = q_rings
                        .iter()
                        .filter(|e| e.1 == PLAYER_HUMAN)
                        .map(|e| e.0)
                        .collect::<Vec<_>>();

                    if rings_human.contains(&mouse_cursor_coord) {
                        commands.spawn((
                            marker_mesh(&mut meshes, player_colors.human.clone()),
                            BoardElement(mouse_cursor_coord, PLAYER_HUMAN),
                            Marker,
                            FOREGROUND_RENDER_LAYER,
                        ));
                        board_changed_events.send(BoardChangedEvent);
                    }
                }
                GameState::WaitForAI => {}
            }
        }
    }
}

fn keyboard_control(keyboard: Res<ButtonInput<KeyCode>>, mut exit: EventWriter<AppExit>) {
    if keyboard.just_pressed(KeyCode::Escape) || keyboard.just_pressed(KeyCode::KeyQ) {
        exit.send(AppExit::Success);
    }
}
