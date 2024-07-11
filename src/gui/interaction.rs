use bevy::prelude::*;

use bevy::window::PrimaryWindow;

use bevy_tweening::{lens::TransformPositionLens, Animator, EaseFunction, Tween, TweeningPlugin};
use yinsh::{Action, Coord};

use super::board::{BoardElement, Marker, Ring};
use super::graphics::{
    marker_mesh, ring_mesh, spawn_marker, spawn_ring, MainCamera, PlayerColors, ANIMATION_DURATION,
    FOREGROUND_RENDER_LAYER,
};
use super::state::{GameState, PlayerActionEvent};
use super::PLAYER_HUMAN;
use super::{
    graphics::{screen_point, COLOR_RING_MOVEMENT_INDICATOR, SPACING},
    state::InteractionState,
};

#[derive(Component)]
pub struct CursorElement;

#[derive(Resource)]
pub struct CursorCoord(pub Option<Coord>);

fn setup_interaction_cursors(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    player_colors: Res<PlayerColors>,
) {
    commands.spawn((
        ring_mesh(
            &mut meshes,
            player_colors.human_transparent.clone(),
            Visibility::Hidden,
        ),
        BoardElement(Coord::new(0, 0), PLAYER_HUMAN),
        Ring,
        CursorElement,
        FOREGROUND_RENDER_LAYER,
    ));

    commands.spawn((
        marker_mesh(
            &mut meshes,
            player_colors.human_transparent.clone(),
            Visibility::Hidden,
        ),
        BoardElement(Coord::new(0, 0), PLAYER_HUMAN),
        Marker,
        CursorElement,
        FOREGROUND_RENDER_LAYER,
    ));
}

fn draw_ring_move_indicators(mut gizmos: Gizmos, interaction_state: Res<InteractionState>) {
    let indicator_color = COLOR_RING_MOVEMENT_INDICATOR;

    if let InteractionState::RingMovement(_, ref possible_moves) = *interaction_state {
        for coord in possible_moves {
            let screen_pos = screen_point(*coord);
            gizmos.circle(screen_pos, Dir3::Z, SPACING / 8., indicator_color);
        }
    }
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
                let run_coords = game_state.board.run_coords_from(seed).unwrap();

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
                    InteractionState::RingMovement(start, _) => {
                        if *coord == start && ring.is_some() {
                            player_colors.human_highlighted.clone()
                        } else {
                            player_colors.human.clone()
                        }
                    }
                    InteractionState::RunRemoval { ref run_coords } => match mouse_cursor_coord.0 {
                        Some(cursor_coord) if run_coords.contains(&cursor_coord) => {
                            let run_from_cursor =
                                game_state.board.run_coords_from(cursor_coord).unwrap();
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
                    if game_state.board.is_free(cursor_coord) {
                        *cursor_ring_visibility = Visibility::Visible;
                        cursor_ring_coord.0 = cursor_coord;
                    }
                }
                InteractionState::MarkerPlacement => {
                    if game_state
                        .board
                        .can_place_marker_at(cursor_coord, PLAYER_HUMAN)
                    {
                        *cursor_marker_visibility = Visibility::Visible;
                        cursor_marker_coord.0 = cursor_coord;
                    }
                }
                InteractionState::RingMovement(_, ref possible_ring_moves) => {
                    if possible_ring_moves.contains(&cursor_coord) {
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
                    if game_state.board.is_free(cursor_coord) {
                        player_action_events.send(PlayerActionEvent(
                            PLAYER_HUMAN,
                            Action::PlaceRing(cursor_coord),
                        ));
                    }
                }
                InteractionState::MarkerPlacement => {
                    if game_state
                        .board
                        .can_place_marker_at(cursor_coord, PLAYER_HUMAN)
                    {
                        player_action_events.send(PlayerActionEvent(
                            PLAYER_HUMAN,
                            Action::PlaceMarker(cursor_coord),
                        ));
                    }
                }
                InteractionState::RingMovement(start, ref possible_ring_moves) => {
                    if possible_ring_moves.contains(&cursor_coord) {
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
                    if game_state.board.has_ring_at(cursor_coord, PLAYER_HUMAN) {
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

pub fn interaction_plugin(app: &mut App) {
    app.add_plugins(TweeningPlugin)
        .add_systems(Startup, setup_interaction_cursors)
        .add_systems(
            Update,
            (
                draw_ring_move_indicators,
                update_board_elements,
                mouse_cursor_system,
                mouse_interaction_system,
                move_board_elements,
                colorize_board_elements,
            ),
        );
}
