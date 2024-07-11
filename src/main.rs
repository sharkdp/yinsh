mod gui;

use std::time::Duration;

use bevy::{
    core_pipeline::bloom::BloomSettings,
    prelude::*,
    render::view::RenderLayers,
    tasks::AsyncComputeTaskPool,
    window::{PresentMode, PrimaryWindow, WindowMode},
};
use bevy_tweening::lens::TransformPositionLens;
use bevy_tweening::{Animator, EaseFunction, Tween, TweeningPlugin};

use gui::{
    ai::{wait_for_ai_move, AiPlayerStrength, AiTask},
    board::{BoardElement, Marker, Ring},
    graphics::{
        marker_mesh, ring_mesh, screen_point, spawn_marker, spawn_ring, PlayerColors, SPACING,
    },
    io::save_and_load_game_state,
    keyboard::keyboard_control,
    resources::InteractionState,
    PLAYER_AI, PLAYER_HUMAN,
};
use yinsh::{Action, Coord, Player, TurnMode};

#[derive(Component)]
struct MainCamera;

#[derive(Component)]
struct CursorElement;

#[derive(Component)]
struct GameStateInformation;

#[derive(Resource)]
pub struct CursorCoord(Option<Coord>);

const BACKGROUND_RENDER_LAYER: RenderLayers = RenderLayers::layer(1);
const FOREGROUND_RENDER_LAYER: RenderLayers = RenderLayers::layer(2);

#[derive(Event)]
struct PlayerActionEvent(Player, Action);

#[derive(Resource)]
struct GameState(yinsh::GameState);

const ANIMATION_DURATION: Duration = Duration::from_millis(300);

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
            TweeningPlugin,
        ))
        .add_systems(Startup, setup)
        .add_systems(
            Update,
            (
                save_and_load_game_state,
                wait_for_ai_move,
                update_board_elements,
                update_game_state,
                draw_grid,
                draw_indicators,
                show_information,
                keyboard_control,
                mouse_cursor_system,
                mouse_interaction_system,
                move_board_elements,
                colorize_board_elements,
            )
                .chain(),
        )
        .insert_resource(ClearColor(Color::hsl(0.0, 0.0, 0.4)))
        .insert_resource(Msaa::Sample8)
        .insert_resource(InteractionState::RingPlacement)
        .insert_resource(AiTask::new())
        .insert_resource(AiPlayerStrength(11))
        .insert_resource(CursorCoord(None))
        .insert_resource(GameState(yinsh::GameState::initial()))
        .add_event::<PlayerActionEvent>()
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
        human_highlighted: materials.add(Color::srgba(4., 4., 4., 1.0)),
        human_transparent: human_transparent.clone(),
        ai: materials.add(Color::srgba(0.0, 0.0, 0.0, 1.0)),
    });

    let (config, _) = config_store.config_mut::<DefaultGizmoConfigGroup>();
    config.render_layers = BACKGROUND_RENDER_LAYER;

    commands.spawn((
        ring_mesh(&mut meshes, human_transparent.clone(), Visibility::Hidden),
        BoardElement(Coord::new(0, 0), PLAYER_HUMAN),
        Ring,
        CursorElement,
        FOREGROUND_RENDER_LAYER,
    ));

    commands.spawn((
        marker_mesh(&mut meshes, human_transparent, Visibility::Hidden),
        BoardElement(Coord::new(0, 0), PLAYER_HUMAN),
        Marker,
        CursorElement,
        FOREGROUND_RENDER_LAYER,
    ));

    commands.spawn((
        TextBundle::from_section(
            "",
            TextStyle {
                font_size: 20.0,
                color: Color::hsl(0., 0., 0.1),
                ..default()
            },
        )
        .with_style(Style {
            position_type: PositionType::Absolute,
            top: Val::Px(10.),
            left: Val::Px(10.),
            ..default()
        }),
        GameStateInformation,
        BACKGROUND_RENDER_LAYER,
    ));
}

fn update_game_state(
    mut game_state: ResMut<GameState>,
    mut player_action_events: EventReader<PlayerActionEvent>,
    mut interaction_state: ResMut<InteractionState>,
    mut task: ResMut<AiTask>,
    ai_player_strength: Res<AiPlayerStrength>,
) {
    for PlayerActionEvent(player, action) in player_action_events.read() {
        assert!(player == &game_state.0.active_player);

        game_state.0.transition(action);

        if let Some(winner) = game_state.0.winner() {
            *interaction_state = InteractionState::Winner(winner);
            return;
        }

        if game_state.0.active_player == PLAYER_AI {
            let task_pool = AsyncComputeTaskPool::get();

            let game_state = game_state.0.clone();
            let search_depth = ai_player_strength.0;
            task.start(task_pool.spawn(async move {
                // TODO! This is a hack to make sure the AI takes at least as long as
                // the animation.
                if matches!(game_state.turn_mode, TurnMode::MarkerPlacement) {
                    std::thread::sleep(ANIMATION_DURATION);
                }

                yinsh::get_ai_player_action(search_depth, &game_state)
            }));

            *interaction_state = InteractionState::WaitForAI;
        } else {
            *interaction_state = InteractionState::from_turn_mode(&game_state.0);
        }
    }
}

fn draw_grid(mut gizmos: Gizmos) {
    let grid_line_color = Color::hsl(0.0, 0.0, 0.3);

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

fn draw_indicators(
    mut gizmos: Gizmos,
    interaction_state: Res<InteractionState>,
    game_state: Res<GameState>,
) {
    let indicator_color = Color::hsla(0.0, 0.0, 1.5, 0.1);

    if let InteractionState::RingMovement(start) = *interaction_state {
        for coord in game_state.0.board.ring_moves(start) {
            let screen_pos = screen_point(coord);
            gizmos.circle(screen_pos, Dir3::Z, SPACING / 8., indicator_color);
        }
    }
}

fn show_information(
    game_state: Res<GameState>,
    mut q_text: Query<&mut Text, With<GameStateInformation>>,
    cursor_coord: Res<CursorCoord>,
    interaction_state: Res<InteractionState>,
    ai_player_strength: Res<AiPlayerStrength>,
) {
    q_text.single_mut().sections[0].value = format!(
        "Active player: {:?}, Score: {}:{}, Grid coord: {:?}\nMode: {}\nAI strength: {} [Weaker: J, Stronger: K]",
        game_state.0.active_player,
        game_state.0.points_a,
        game_state.0.points_b,
        cursor_coord.0,
        match *interaction_state {
            InteractionState::RingPlacement => "Place a ring on the board",
            InteractionState::MarkerPlacement => "Place a marker in one of your rings",
            InteractionState::RingMovement(_) => "Move the selected ring",
            InteractionState::RunRemoval { .. } => "Select a run of five markers to remove",
            InteractionState::RingRemoval => "Select one of your rings to remove it",
            InteractionState::AutoMove | InteractionState::WaitForAI => "AI is thinking...",
            InteractionState::Winner(Player::A) => "Game over. You win!",
            InteractionState::Winner(Player::B) => "Game over. Floyd wins!",
        },
        ai_player_strength.0,
    );
}

fn update_board_elements(
    mut player_action_events: EventReader<PlayerActionEvent>,
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    player_colors: Res<PlayerColors>,
    mut q_rings: Query<
        (Entity, &mut BoardElement),
        (With<Ring>, (Without<Marker>, Without<CursorElement>)),
    >,
    mut q_markers: Query<(Entity, &mut BoardElement), (With<Marker>, Without<CursorElement>)>,
    game_state: Res<GameState>,
) {
    for PlayerActionEvent(player, action) in player_action_events.read() {
        match *action {
            Action::PlaceRing(coord) => {
                spawn_ring(&mut commands, &mut meshes, &player_colors, coord, *player);
            }
            Action::PlaceMarker(coord) => {
                spawn_marker(&mut commands, &mut meshes, &player_colors, coord, *player);
            }
            Action::MoveRing(old_coord, new_coord) => {
                for (entity, mut ring) in q_rings.iter_mut() {
                    if ring.0 == old_coord {
                        ring.0 = new_coord;

                        // Flip markers between old and new coord
                        let coords_between = Coord::between(old_coord, new_coord);
                        for (_, mut element) in q_markers.iter_mut() {
                            if coords_between.contains(&element.0) {
                                element.1.flip();
                            }
                        }

                        let tween = Tween::new(
                            EaseFunction::QuadraticInOut,
                            ANIMATION_DURATION,
                            TransformPositionLens {
                                start: screen_point(old_coord),
                                end: screen_point(new_coord),
                            },
                        );

                        commands.entity(entity).insert(Animator::new(tween));

                        break;
                    }
                }
            }
            Action::RemoveRun(seed) => {
                let run_coords = game_state.0.board.run_coords_from(seed).unwrap();

                for (entity, element) in q_markers.iter_mut() {
                    if run_coords.contains(&element.0) {
                        commands.entity(entity).despawn();
                    }
                }
            }
            Action::RemoveRing(coord) => {
                for (entity, element) in q_rings.iter_mut() {
                    if element.0 == coord {
                        commands.entity(entity).despawn();
                        break;
                    }
                }
            }
            Action::Wait => {}
        }
    }
}

fn move_board_elements(
    mut query: Query<(&BoardElement, &mut Transform), Without<Animator<Transform>>>,
) {
    for (BoardElement(coord, _), mut transform) in query.iter_mut() {
        transform.translation = screen_point(*coord);
    }
}

fn colorize_board_elements(
    mut query: Query<(
        &BoardElement,
        &mut Handle<ColorMaterial>,
        Option<&Ring>,
        Option<&Marker>,
        Option<&CursorElement>,
    )>,
    interaction_state: Res<InteractionState>,
    player_colors: Res<PlayerColors>,
    mouse_cursor_coord: Res<CursorCoord>,
    game_state: Res<GameState>,
) {
    for (BoardElement(coord, player), mut color_material, ring, marker, cursor_element) in
        query.iter_mut()
    {
        *color_material = if player == &PLAYER_HUMAN {
            if cursor_element.is_some() {
                player_colors.human_transparent.clone()
            } else {
                match *interaction_state {
                    InteractionState::RingMovement(start) => {
                        if *coord == start && ring.is_some() {
                            player_colors.human_highlighted.clone()
                        } else {
                            player_colors.human.clone()
                        }
                    }
                    InteractionState::RunRemoval { ref run_coords } => match mouse_cursor_coord.0 {
                        Some(cursor_coord) if run_coords.contains(&cursor_coord) => {
                            let run_from_cursor =
                                game_state.0.board.run_coords_from(cursor_coord).unwrap();
                            if run_from_cursor.contains(coord) {
                                player_colors.human_highlighted.clone()
                            } else {
                                player_colors.human.clone()
                            }
                        }
                        _ => {
                            if marker.is_some() && run_coords.contains(coord) {
                                player_colors.human_highlighted.clone()
                            } else {
                                player_colors.human.clone()
                            }
                        }
                    },
                    _ => player_colors.human.clone(),
                }
            }
        } else {
            player_colors.ai.clone()
        };
    }
}

fn mouse_cursor_system(
    game_state: Res<GameState>,
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
    interaction_state: Res<InteractionState>,
    mut mouse_cursor_coord: ResMut<CursorCoord>,
) {
    let Ok(mut window) = q_window.get_single_mut() else {
        return;
    };

    window.cursor.icon = match *interaction_state {
        InteractionState::WaitForAI => CursorIcon::Progress,
        InteractionState::Winner(_) => CursorIcon::Default,
        _ => CursorIcon::Pointer,
    };

    if let Some(cursor_position) = window.cursor_position() {
        let (camera, camera_transform) = q_camera.single();

        if let Some(cursor_position) = camera
            .viewport_to_world(camera_transform, cursor_position)
            .map(|ray| ray.origin.truncate())
        {
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
                InteractionState::RingPlacement => {
                    if game_state.0.board.is_free(cursor_coord) {
                        *cursor_ring_visibility = Visibility::Visible;
                        cursor_ring_coord.0 = cursor_coord;
                    }
                }
                InteractionState::MarkerPlacement => {
                    if game_state
                        .0
                        .board
                        .can_place_marker_at(cursor_coord, PLAYER_HUMAN)
                    {
                        *cursor_marker_visibility = Visibility::Visible;
                        cursor_marker_coord.0 = cursor_coord;
                    }
                }
                InteractionState::RingMovement(start) => {
                    if game_state.0.board.is_valid_ring_move(start, cursor_coord) {
                        *cursor_ring_visibility = Visibility::Visible;
                        cursor_ring_coord.0 = cursor_coord;
                    }
                }
                InteractionState::RunRemoval { .. } => {}
                InteractionState::RingRemoval => {}
                InteractionState::AutoMove => {}
                InteractionState::WaitForAI => {}
                InteractionState::Winner(_) => {}
            }

            mouse_cursor_coord.0 = Some(cursor_coord);
        }
    }
}

fn mouse_interaction_system(
    game_state: Res<GameState>,
    buttons: Res<ButtonInput<MouseButton>>,
    interaction_state: Res<InteractionState>,
    cursor_coord: Res<CursorCoord>,
    mut player_action_events: EventWriter<PlayerActionEvent>,
) {
    if matches!(*interaction_state, InteractionState::AutoMove) {
        // TODO
        player_action_events.send(PlayerActionEvent(PLAYER_HUMAN, Action::Wait));
    }

    if let Some(cursor_coord) = cursor_coord.0 {
        if buttons.just_pressed(MouseButton::Left) {
            match *interaction_state {
                InteractionState::RingPlacement => {
                    if game_state.0.board.is_free(cursor_coord) {
                        player_action_events.send(PlayerActionEvent(
                            PLAYER_HUMAN,
                            Action::PlaceRing(cursor_coord),
                        ));
                    }
                }
                InteractionState::MarkerPlacement => {
                    if game_state
                        .0
                        .board
                        .can_place_marker_at(cursor_coord, PLAYER_HUMAN)
                    {
                        player_action_events.send(PlayerActionEvent(
                            PLAYER_HUMAN,
                            Action::PlaceMarker(cursor_coord),
                        ));
                    }
                }
                InteractionState::RingMovement(start) => {
                    if game_state.0.board.is_valid_ring_move(start, cursor_coord) {
                        player_action_events.send(PlayerActionEvent(
                            PLAYER_HUMAN,
                            Action::MoveRing(start, cursor_coord),
                        ));
                    }
                }
                InteractionState::WaitForAI => {}
                InteractionState::RunRemoval { ref run_coords } => {
                    if run_coords.contains(&cursor_coord) {
                        player_action_events.send(PlayerActionEvent(
                            PLAYER_HUMAN,
                            Action::RemoveRun(cursor_coord),
                        ));
                    }
                }
                InteractionState::RingRemoval => {
                    if game_state.0.board.has_ring_at(cursor_coord, PLAYER_HUMAN) {
                        player_action_events.send(PlayerActionEvent(
                            PLAYER_HUMAN,
                            Action::RemoveRing(cursor_coord),
                        ));
                    }
                }
                InteractionState::AutoMove => {}
                InteractionState::Winner(_) => {}
            }
        }
    }
}
