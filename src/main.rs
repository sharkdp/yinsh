mod ai;
mod yinsh;

use bevy::render::view::visibility;
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
use yinsh::{Action, Coord, Player, TurnMode};

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
pub enum InteractionState {
    PlaceRing,
    PlaceMarker,
    MoveRing(Coord),
    WaitForAI,
}

#[derive(Resource)]
pub struct MouseCursorCoord(Option<Coord>);

const PLAYER_HUMAN: Player = Player::A;
const PLAYER_AI: Player = Player::B;

#[derive(Resource)]
struct PlayerColors {
    human: Handle<ColorMaterial>,
    human_highlighted: Handle<ColorMaterial>,
    human_transparent: Handle<ColorMaterial>,
    ai: Handle<ColorMaterial>,
}

const BACKGROUND_RENDER_LAYER: RenderLayers = RenderLayers::layer(1);
const FOREGROUND_RENDER_LAYER: RenderLayers = RenderLayers::layer(2);

#[derive(Event)]
struct PlayerActionEvent(Player, Action);

#[derive(Resource)]
struct AiTask(Option<Task<Action>>);

#[derive(Resource)]
struct GameState(yinsh::GameState);

#[derive(Component)]
enum Appearance {
    Default,
    Highlighted,
    Transparent,
}

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
            // LogDiagnosticsPlugin::default(),
            // FrameTimeDiagnosticsPlugin,
        ))
        .add_systems(Startup, setup)
        .add_systems(
            Update,
            (
                wait_for_ai_move,
                update_game_state,
                draw_grid,
                keyboard_control,
                mouse_cursor_system,
                mouse_interaction_system,
                update_board_elements,
                move_and_colorize_board_elements,
            )
                .chain(),
        )
        .insert_resource(ClearColor(Color::hsl(0.0, 0.0, 0.3)))
        .insert_resource(Msaa::default())
        .insert_resource(InteractionState::PlaceRing)
        .insert_resource(MouseCursorCoord(None))
        .insert_resource(AiTask(None))
        .insert_resource(GameState(yinsh::GameState::initial()))
        .add_event::<PlayerActionEvent>()
        .run();
}

fn ring_mesh(
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

fn marker_mesh(
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

    let human_transparent = materials.add(Color::srgba(1.5, 1.5, 1.5, 0.1));
    commands.insert_resource(PlayerColors {
        human: materials.add(Color::srgba(1.5, 1.5, 1.5, 1.0)),
        human_highlighted: materials.add(Color::srgba(3., 3., 3., 1.0)),
        human_transparent: human_transparent.clone(),
        ai: materials.add(Color::srgba(0.0, 0.0, 0.0, 1.0)),
    });

    let (config, _) = config_store.config_mut::<DefaultGizmoConfigGroup>();
    config.render_layers = BACKGROUND_RENDER_LAYER;

    commands.spawn((
        ring_mesh(&mut meshes, human_transparent.clone(), Visibility::Hidden),
        BoardElement(Coord { x: 0, y: 0 }, PLAYER_HUMAN),
        Ring,
        CursorElement,
        Appearance::Transparent,
        FOREGROUND_RENDER_LAYER,
    ));

    commands.spawn((
        marker_mesh(&mut meshes, human_transparent, Visibility::Hidden),
        BoardElement(Coord { x: 0, y: 0 }, PLAYER_HUMAN),
        Marker,
        CursorElement,
        Appearance::Transparent,
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

fn wait_for_ai_move(
    mut task: ResMut<AiTask>,
    mut player_action_events: EventWriter<PlayerActionEvent>,
) {
    if task.0.is_none() {
        return;
    }

    let status = block_on(future::poll_once(task.0.as_mut().unwrap()));

    if status.is_none() {
        return;
    }

    task.0 = None;

    let action = status.unwrap();

    player_action_events.send(PlayerActionEvent(PLAYER_AI, action));
}

fn update_game_state(
    mut game_state: ResMut<GameState>,
    mut player_action_events: EventReader<PlayerActionEvent>,
    mut interaction_state: ResMut<InteractionState>,
    mut task: ResMut<AiTask>,
) {
    for PlayerActionEvent(player, action) in player_action_events.read() {
        assert!(player == &game_state.0.active_player);
        game_state.0.transition(action);

        if game_state.0.active_player == PLAYER_AI {
            let task_pool = AsyncComputeTaskPool::get();

            let game_state = game_state.0.clone();
            task.0 = Some(task_pool.spawn(async move {
                let mut gamestates = crate::ai::gamestates(&game_state);
                let first = gamestates.next();

                assert!(first.is_some());

                first.unwrap()
            }));

            *interaction_state = InteractionState::WaitForAI;
        } else {
            *interaction_state = match game_state.0.turn_mode {
                TurnMode::PlaceRing => InteractionState::PlaceRing,
                TurnMode::PlaceMarker => InteractionState::PlaceMarker,
                TurnMode::MoveRing(start) => InteractionState::MoveRing(start),
                TurnMode::RemoveRun(_) => todo!(),
                TurnMode::RemoveRing(_) => todo!(),
                TurnMode::WaitRemoveRun(_) => todo!(),
                TurnMode::WaitPlaceMarker => todo!(),
            };
        }
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

fn update_board_elements(
    game_state: Res<GameState>,
    mut player_action_events: EventReader<PlayerActionEvent>,
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut q_rings: Query<(&mut BoardElement, &mut Appearance), With<Ring>>,
    player_colors: Res<PlayerColors>,
) {
    for PlayerActionEvent(player, action) in player_action_events.read() {
        let color = if player == &PLAYER_HUMAN {
            player_colors.human.clone()
        } else {
            player_colors.ai.clone()
        };

        match *action {
            Action::PlaceRing(coord) => {
                commands.spawn((
                    ring_mesh(&mut meshes, color, Visibility::Visible),
                    BoardElement(coord, *player),
                    Ring,
                    Appearance::Default,
                    FOREGROUND_RENDER_LAYER,
                ));
            }
            Action::PlaceMarker(coord) => {
                commands.spawn((
                    marker_mesh(&mut meshes, color, Visibility::Visible),
                    BoardElement(coord, *player),
                    Marker,
                    Appearance::Default,
                    FOREGROUND_RENDER_LAYER,
                ));

                for (ring, mut appearance) in q_rings.iter_mut() {
                    if ring.0 == coord {
                        *appearance = Appearance::Highlighted;
                    }
                }
            }
            Action::MoveRing(old_coord, new_coord) => {
                for (mut ring, mut appearance) in q_rings.iter_mut() {
                    if ring.0 == old_coord {
                        *appearance = Appearance::Default;
                        ring.0 = new_coord;
                        break;
                    }
                }
            }
            Action::RemoveRun(_) => todo!(),
            Action::RemoveRing(_) => todo!(),
            Action::Wait => todo!(),
        }
    }
}

fn move_and_colorize_board_elements(
    mut query: Query<(
        &BoardElement,
        &Appearance,
        &mut Transform,
        &mut Handle<ColorMaterial>,
    )>,
    player_colors: Res<PlayerColors>,
) {
    for (BoardElement(coord, player), appearance, mut transform, mut color_material) in
        query.iter_mut()
    {
        transform.translation = screen_point(*coord);

        *color_material = if player == &PLAYER_HUMAN {
            match appearance {
                Appearance::Default => player_colors.human.clone(),
                Appearance::Highlighted => player_colors.human_highlighted.clone(),
                Appearance::Transparent => player_colors.human_transparent.clone(),
            }
        } else {
            player_colors.ai.clone()
        };
    }
}

fn mouse_cursor_system(
    mut q_window: Query<&mut Window, With<PrimaryWindow>>,
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
    interaction_state: Res<InteractionState>,
    mut mouse_cursor_coord: ResMut<MouseCursorCoord>,
) {
    let mut window = q_window.single_mut();

    window.cursor.icon = match *interaction_state {
        InteractionState::WaitForAI => CursorIcon::Progress,
        _ => CursorIcon::Pointer,
    };

    if let Some(cursor_position) = window.cursor_position() {
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

            match *interaction_state {
                InteractionState::PlaceRing => {
                    *cursor_ring_visibility = Visibility::Visible;
                    cursor_ring_coord.0 = cursor_coord;
                }
                InteractionState::PlaceMarker => {
                    if rings_human.contains(&cursor_coord) {
                        *cursor_marker_visibility = Visibility::Visible;
                        cursor_marker_coord.0 = cursor_coord;
                    }
                }
                InteractionState::MoveRing(_) => {
                    // TODO
                    *cursor_ring_visibility = Visibility::Visible;
                    cursor_ring_coord.0 = cursor_coord;
                }
                InteractionState::WaitForAI => {}
            }

            mouse_cursor_coord.0 = Some(cursor_coord);
        }
    }
}

fn mouse_interaction_system(
    buttons: Res<ButtonInput<MouseButton>>,
    interaction_state: Res<InteractionState>,
    mouse_cursor_coord: Res<MouseCursorCoord>,
    q_rings: Query<&BoardElement, (With<Ring>, Without<CursorElement>)>,
    mut player_action_events: EventWriter<PlayerActionEvent>,
) {
    if let Some(mouse_cursor_coord) = mouse_cursor_coord.0 {
        if buttons.just_pressed(MouseButton::Left) {
            match *interaction_state {
                InteractionState::PlaceRing => {
                    player_action_events.send(PlayerActionEvent(
                        PLAYER_HUMAN,
                        Action::PlaceRing(mouse_cursor_coord),
                    ));
                }
                InteractionState::PlaceMarker => {
                    let rings_human = q_rings
                        .iter()
                        .filter(|e| e.1 == PLAYER_HUMAN)
                        .map(|e| e.0)
                        .collect::<Vec<_>>();

                    if rings_human.contains(&mouse_cursor_coord) {
                        player_action_events.send(PlayerActionEvent(
                            PLAYER_HUMAN,
                            Action::PlaceMarker(mouse_cursor_coord),
                        ));
                    }
                }
                InteractionState::MoveRing(start) => {
                    // TODO
                    player_action_events.send(PlayerActionEvent(
                        PLAYER_HUMAN,
                        Action::MoveRing(start, mouse_cursor_coord),
                    ));
                }
                InteractionState::WaitForAI => {}
            }
        }
    }
}

fn keyboard_control(keyboard: Res<ButtonInput<KeyCode>>, mut exit: EventWriter<AppExit>) {
    if keyboard.just_pressed(KeyCode::Escape) || keyboard.just_pressed(KeyCode::KeyQ) {
        exit.send(AppExit::Success);
    }
}
