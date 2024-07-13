use std::time::Duration;

use bevy::prelude::*;

use bevy::window::PrimaryWindow;

use bevy_tweening::lens::ColorMaterialColorLens;
use bevy_tweening::{lens::TransformPositionLens, Animator, EaseFunction, Tween, TweeningPlugin};
use bevy_tweening::{AnimationSystem, AssetAnimator, Delay, EaseMethod};
use yinsh::{all_coords, Action, Coord};

use super::ai::AiSet;
use super::board::{BoardElement, Marker, Ring};
use super::board_update_event::BoardUpdateEvent;
use super::graphics::{
    color_for_player, marker_mesh, ring_mesh, spawn_marker, spawn_ring, MainCamera, PlayerColors,
    ScaleFactor, ScaleFactorSet, ANIMATION_DURATION, FOREGROUND_RENDER_LAYER,
};
use super::state_update::{GameState, PlayerActionEvent, StateUpdateSet};
use super::PLAYER_HUMAN;
use super::{graphics::COLOR_RING_MOVEMENT_INDICATOR, state_update::InteractionState};

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

fn draw_ring_move_indicators(
    scale_factor: Res<ScaleFactor>,
    mut gizmos: Gizmos,
    interaction_state: Res<InteractionState>,
) {
    let indicator_color = COLOR_RING_MOVEMENT_INDICATOR;

    if let InteractionState::RingMovement(_, ref possible_moves) = *interaction_state {
        for coord in possible_moves {
            let screen_pos = scale_factor.screen_point(*coord);
            gizmos.circle(
                screen_pos,
                Dir3::Z,
                scale_factor.spacing / 8.,
                indicator_color,
            );
        }
    }
}

fn update_board_elements(
    mut board_update_events: EventReader<BoardUpdateEvent>,
    mut commands: Commands,
    scale_factor: Res<ScaleFactor>,
    mut meshes: ResMut<Assets<Mesh>>,
    player_colors: Res<PlayerColors>,
    mut q_rings: Query<
        (Entity, &mut BoardElement),
        (With<Ring>, (Without<Marker>, Without<CursorElement>)),
    >,
    mut q_markers: Query<
        (Entity, &mut BoardElement, &mut Handle<ColorMaterial>),
        (With<Marker>, Without<CursorElement>),
    >,
) {
    for event in board_update_events.read() {
        match *event {
            BoardUpdateEvent::AddRing(coord, player) => {
                spawn_ring(&mut commands, &mut meshes, &player_colors, coord, player);
            }
            BoardUpdateEvent::AddMarker(coord, player) => {
                spawn_marker(&mut commands, &mut meshes, &player_colors, coord, player);
            }
            BoardUpdateEvent::MoveRing(old_coord, new_coord) => {
                for (entity, mut ring) in q_rings.iter_mut() {
                    if ring.0 == old_coord {
                        let tween = Tween::new(
                            EaseFunction::QuadraticInOut,
                            ANIMATION_DURATION,
                            TransformPositionLens {
                                start: scale_factor.screen_point(old_coord),
                                end: scale_factor.screen_point(new_coord),
                            },
                        );

                        commands.entity(entity).insert(Animator::new(tween));

                        ring.0 = new_coord; // To make the change permanent

                        break;
                    }
                }
            }
            BoardUpdateEvent::RemoveRun(ref run_coords) => {
                for (entity, element, _) in q_markers.iter_mut() {
                    if run_coords.contains(&element.0) {
                        commands.entity(entity).despawn();
                    }
                }
            }
            BoardUpdateEvent::RemoveRing(coord) => {
                for (entity, element) in q_rings.iter_mut() {
                    if element.0 == coord {
                        commands.entity(entity).despawn();
                        break;
                    }
                }
            }
            BoardUpdateEvent::FlipMarkers(start, end, ref marker_coords) => {
                let mut i = 0;
                let total_distance = (end - start).norm();

                for (entity, mut element, mut color_material) in q_markers.iter_mut() {
                    if marker_coords.contains(&element.0) {
                        let distance_from_start = (element.0 - start).norm();

                        let delay =
                            ANIMATION_DURATION.mul_f32(distance_from_start / total_distance);

                        let tween = Tween::new(
                            EaseMethod::Linear,
                            Duration::from_secs_f32(1e-9),
                            ColorMaterialColorLens {
                                start: color_for_player(element.1),
                                end: color_for_player(element.1),
                            },
                        )
                        .then(Delay::new(delay).then(Tween::new(
                            EaseMethod::Linear,
                            ANIMATION_DURATION.div_f32(5.0),
                            ColorMaterialColorLens {
                                start: color_for_player(element.1),
                                end: color_for_player(element.1.next()),
                            },
                        )));

                        *color_material = player_colors.animated_markers[i].clone();
                        commands.entity(entity).insert(AssetAnimator::new(tween));

                        element.1.flip(); // To make the change permanent

                        i += 1;
                    }
                }
            }
        }
    }
}

fn clear_animators(mut q: Query<(Entity, &Animator<Transform>)>, mut commands: Commands) {
    for (entity, animator) in q.iter_mut() {
        if animator.tweenable().times_completed() == 1 {
            commands.entity(entity).remove::<Animator<Transform>>();
        }
    }
}

fn clear_asset_animators(
    mut q: Query<(Entity, &AssetAnimator<ColorMaterial>)>,
    mut commands: Commands,
) {
    for (entity, animator) in q.iter_mut() {
        if animator.tweenable().times_completed() == 1 {
            commands
                .entity(entity)
                .remove::<AssetAnimator<ColorMaterial>>();
        }
    }
}

fn move_board_elements(
    scale_factor: Res<ScaleFactor>,
    mut query: Query<(&BoardElement, &mut Transform), Without<Animator<Transform>>>,
) {
    for (BoardElement(coord, _), mut transform) in query.iter_mut() {
        transform.translation = scale_factor.screen_point(*coord);
    }
}

fn scale_board_elements(
    scale_factor: Res<ScaleFactor>,
    mut query: Query<&mut Transform, With<BoardElement>>,
) {
    for mut transform in query.iter_mut() {
        transform.scale = Vec3::splat(scale_factor.factor);
    }
}

fn colorize_board_elements(
    mut query: Query<(
        &BoardElement,
        &mut Handle<ColorMaterial>,
        Option<&Ring>,
        Option<&Marker>,
        Option<&CursorElement>,
        Option<&AssetAnimator<ColorMaterial>>,
    )>,
    interaction_state: Res<InteractionState>,
    player_colors: Res<PlayerColors>,
    mouse_cursor_coord: Res<CursorCoord>,
) {
    for (BoardElement(coord, player), mut color_material, ring, marker, cursor_element, animated) in
        query.iter_mut()
    {
        if animated.is_some() {
            continue;
        }

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
                    InteractionState::RunRemoval {
                        ref all_run_coords,
                        ref run_from_seed,
                    } => match mouse_cursor_coord.0 {
                        Some(cursor_coord) if all_run_coords.contains(&cursor_coord) => {
                            let run_from_cursor = run_from_seed.get(&cursor_coord).unwrap();
                            if run_from_cursor.contains(coord) {
                                player_colors.human_highlighted.clone()
                            } else {
                                player_colors.human.clone()
                            }
                        }
                        _ => {
                            if marker.is_some() && all_run_coords.contains(coord) {
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

fn grid_cursor_system(
    scale_factor: Res<ScaleFactor>,
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
                    let screen_pos = scale_factor.screen_point(*c);
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
                InteractionState::RingPlacement(ref free_coords) => {
                    if free_coords.contains(&cursor_coord) {
                        *cursor_ring_visibility = Visibility::Visible;
                        cursor_ring_coord.0 = cursor_coord;
                    }
                }
                InteractionState::MarkerPlacement(ref ring_coords) => {
                    if ring_coords.contains(&cursor_coord) {
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
                InteractionState::RingRemoval(_) => {}
                InteractionState::AutoMove => {}
                InteractionState::WaitForAI => {}
                InteractionState::Winner(_) => {}
            }

            mouse_cursor_coord.0 = Some(cursor_coord);
        }
    }
}

fn mouse_interaction_system(
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
                InteractionState::RingPlacement(ref free_coords) => {
                    if free_coords.contains(&cursor_coord) {
                        player_action_events.send(PlayerActionEvent(
                            PLAYER_HUMAN,
                            Action::PlaceRing(cursor_coord),
                        ));
                    }
                }
                InteractionState::MarkerPlacement(ref ring_coords) => {
                    if ring_coords.contains(&cursor_coord) {
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
                InteractionState::RunRemoval {
                    ref all_run_coords, ..
                } => {
                    if all_run_coords.contains(&cursor_coord) {
                        player_action_events.send(PlayerActionEvent(
                            PLAYER_HUMAN,
                            Action::RemoveRun(cursor_coord),
                        ));
                    }
                }
                InteractionState::RingRemoval(ref ring_coords) => {
                    if ring_coords.contains(&cursor_coord) {
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

pub fn plugin(app: &mut App) {
    app.add_plugins(TweeningPlugin)
        .insert_resource(InteractionState::RingPlacement(all_coords()))
        .insert_resource(CursorCoord(None))
        .insert_resource(GameState::initial())
        .add_systems(Startup, setup_interaction_cursors)
        .add_systems(
            Update,
            (
                draw_ring_move_indicators,
                (
                    clear_animators,
                    clear_asset_animators,
                    grid_cursor_system,
                    update_board_elements,
                    move_board_elements.ambiguous_with(AnimationSystem::AnimationUpdate),
                    scale_board_elements.ambiguous_with(AnimationSystem::AnimationUpdate),
                    colorize_board_elements.ambiguous_with(AnimationSystem::AnimationUpdate),
                    mouse_interaction_system.ambiguous_with(AiSet),
                )
                    .chain(),
            )
                .after(StateUpdateSet)
                .after(ScaleFactorSet),
        );
}
