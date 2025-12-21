use std::time::Duration;

use bevy::prelude::*;

use bevy::ecs::message::{MessageReader, MessageWriter};
use bevy::window::{CursorIcon, PrimaryWindow, SystemCursorIcon};

use bevy_tweening::lens::ColorMaterialColorLens;
use bevy_tweening::{
    AnimTarget, Tween, TweenAnim, TweenState, TweeningPlugin, lens::TransformPositionLens,
};
use bevy_tweening::{Delay, EaseMethod};

use bevy::prelude::MeshMaterial2d;
use yinsh::{Coord, Move, all_coords};

use crate::PLAYER_HUMAN;
use crate::ai::AiSet;
use crate::board::{BoardElement, Marker, Ring};
use crate::board_update_event::BoardUpdateEvent;
use crate::graphics::{
    ANIMATION_DURATION, FOREGROUND_RENDER_LAYER, MainCamera, PlayerColors, ScaleFactor,
    ScaleFactorSet, color_for_player, marker_mesh, ring_mesh, spawn_marker, spawn_ring,
};
use crate::state_update::{GameState, PlayerMoveEvent, StateUpdateSet};
use crate::{graphics::COLOR_RING_MOVEMENT_INDICATOR, state_update::InteractionState};

#[derive(Component)]
pub struct CursorElement;

#[derive(Resource)]
pub struct CursorCoord(pub Option<Coord>);

fn viewport_to_coord(
    viewport_pos: Vec2,
    camera: &Camera,
    camera_transform: &GlobalTransform,
    scale_factor: &ScaleFactor,
) -> Option<Coord> {
    let world_pos = camera
        .viewport_to_world(camera_transform, viewport_pos)
        .map(|ray| ray.origin.truncate())
        .ok()?;
    Some(
        yinsh::all_coords()
            .into_iter()
            .min_by_key(|c| {
                let screen_pos = scale_factor.screen_point(*c);
                let pos = Vec3::new(world_pos.x, world_pos.y, 0.0);
                (screen_pos - pos).length_squared() as i32
            })
            .unwrap(),
    )
}

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
                Isometry3d::from_translation(screen_pos),
                scale_factor.spacing / 8.,
                indicator_color,
            );
        }
    }
}

fn update_board_elements(
    mut board_update_events: MessageReader<BoardUpdateEvent>,
    mut commands: Commands,
    scale_factor: Res<ScaleFactor>,
    mut meshes: ResMut<Assets<Mesh>>,
    player_colors: Res<PlayerColors>,
    mut q_rings: Query<
        (Entity, &mut BoardElement),
        (With<Ring>, (Without<Marker>, Without<CursorElement>)),
    >,
    mut q_markers: Query<
        (
            Entity,
            &mut BoardElement,
            &mut MeshMaterial2d<ColorMaterial>,
        ),
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

                        commands.entity(entity).insert(TweenAnim::new(tween));

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
                            EaseMethod::default(),
                            Duration::from_secs_f32(1e-9),
                            ColorMaterialColorLens {
                                start: color_for_player(element.1),
                                end: color_for_player(element.1),
                            },
                        )
                        .then(Delay::new(delay).then(Tween::new(
                            EaseMethod::default(),
                            ANIMATION_DURATION.div_f32(5.0),
                            ColorMaterialColorLens {
                                start: color_for_player(element.1),
                                end: color_for_player(element.1.next()),
                            },
                        )));

                        let anim_material = player_colors.animated_markers[i].clone();
                        color_material.0 = anim_material.clone();
                        commands
                            .entity(entity)
                            .insert((TweenAnim::new(tween), AnimTarget::asset(&anim_material)));

                        element.1.flip(); // To make the change permanent

                        i += 1;
                    }
                }
            }
        }
    }
}

fn clear_animators(q: Query<(Entity, &TweenAnim)>, mut commands: Commands) {
    for (entity, animator) in q.iter() {
        if matches!(animator.tween_state(), TweenState::Completed) {
            commands.entity(entity).remove::<TweenAnim>();
            commands.entity(entity).remove::<AnimTarget>();
        }
    }
}

fn move_board_elements(
    scale_factor: Res<ScaleFactor>,
    mut query: Query<(&BoardElement, &mut Transform), Without<TweenAnim>>,
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
        &mut MeshMaterial2d<ColorMaterial>,
        Option<&Ring>,
        Option<&Marker>,
        Option<&CursorElement>,
        Option<&TweenAnim>,
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

        color_material.0 = if *player == PLAYER_HUMAN {
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
                            if run_from_cursor.contains(&coord) {
                                player_colors.human_highlighted.clone()
                            } else {
                                player_colors.human.clone()
                            }
                        }
                        _ => {
                            if marker.is_some() && all_run_coords.contains(&coord) {
                                player_colors.human_highlighted.clone()
                            } else {
                                player_colors.human.clone()
                            }
                        }
                    },
                    InteractionState::RingRemoval(ref removable_rings) => {
                        if ring.is_some()
                            && mouse_cursor_coord.0 == Some(*coord)
                            && removable_rings.contains(coord)
                        {
                            player_colors.human_transparent.clone()
                        } else {
                            player_colors.human.clone()
                        }
                    }
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
    mut q_window: Query<(Entity, &mut Window), With<PrimaryWindow>>,
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
    mut commands: Commands,
) {
    let Ok((window_entity, window)) = q_window.single_mut() else {
        return;
    };

    let cursor_icon = match *interaction_state {
        InteractionState::WaitForAI => SystemCursorIcon::Progress,
        InteractionState::Winner(_) => SystemCursorIcon::Default,
        _ => SystemCursorIcon::Pointer,
    };
    commands
        .entity(window_entity)
        .insert(CursorIcon::from(cursor_icon));

    if let Some(cursor_position) = window.cursor_position() {
        let Ok((camera, camera_transform)) = q_camera.single() else {
            return;
        };

        if let Some(cursor_coord) =
            viewport_to_coord(cursor_position, camera, camera_transform, &scale_factor)
        {
            let Ok((mut cursor_ring_coord, mut cursor_ring_visibility)) = cursor_ring.single_mut()
            else {
                return;
            };
            *cursor_ring_visibility = Visibility::Hidden;

            let Ok((mut cursor_marker_coord, mut cursor_marker_visibility)) =
                cursor_marker.single_mut()
            else {
                return;
            };
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
    touches: Res<Touches>,
    interaction_state: Res<InteractionState>,
    cursor_coord: Res<CursorCoord>,
    scale_factor: Res<ScaleFactor>,
    q_camera: Query<(&Camera, &GlobalTransform), With<MainCamera>>,
    mut player_move_events: MessageWriter<PlayerMoveEvent>,
) {
    if matches!(*interaction_state, InteractionState::AutoMove) {
        // TODO
        player_move_events.write(PlayerMoveEvent(PLAYER_HUMAN, Move::Wait));
    }

    // Get coordinate from touch if available
    let touch_coord = touches.iter_just_pressed().next().and_then(|touch| {
        let Ok((camera, camera_transform)) = q_camera.single() else {
            return None;
        };
        viewport_to_coord(touch.position(), camera, camera_transform, &scale_factor)
    });

    let clicked = buttons.just_pressed(MouseButton::Left);
    let cursor_coord = if touch_coord.is_some() {
        touch_coord
    } else if clicked {
        cursor_coord.0
    } else {
        None
    };

    if let Some(cursor_coord) = cursor_coord {
        match *interaction_state {
            InteractionState::RingPlacement(ref free_coords) => {
                if free_coords.contains(&cursor_coord) {
                    player_move_events
                        .write(PlayerMoveEvent(PLAYER_HUMAN, Move::PlaceRing(cursor_coord)));
                }
            }
            InteractionState::MarkerPlacement(ref ring_coords) => {
                if ring_coords.contains(&cursor_coord) {
                    player_move_events.write(PlayerMoveEvent(
                        PLAYER_HUMAN,
                        Move::PlaceMarker(cursor_coord),
                    ));
                }
            }
            InteractionState::RingMovement(start, ref possible_ring_moves) => {
                if possible_ring_moves.contains(&cursor_coord) {
                    player_move_events.write(PlayerMoveEvent(
                        PLAYER_HUMAN,
                        Move::MoveRing(start, cursor_coord),
                    ));
                }
            }
            InteractionState::WaitForAI => {}
            InteractionState::RunRemoval {
                ref all_run_coords, ..
            } => {
                if all_run_coords.contains(&cursor_coord) {
                    player_move_events
                        .write(PlayerMoveEvent(PLAYER_HUMAN, Move::RemoveRun(cursor_coord)));
                }
            }
            InteractionState::RingRemoval(ref ring_coords) => {
                if ring_coords.contains(&cursor_coord) {
                    player_move_events.write(PlayerMoveEvent(
                        PLAYER_HUMAN,
                        Move::RemoveRing(cursor_coord),
                    ));
                }
            }
            InteractionState::AutoMove => {}
            InteractionState::Winner(_) => {}
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
                    grid_cursor_system,
                    update_board_elements,
                    move_board_elements,
                    scale_board_elements,
                    colorize_board_elements,
                    mouse_interaction_system.ambiguous_with(AiSet),
                )
                    .chain(),
            )
                .after(StateUpdateSet)
                .after(ScaleFactorSet),
        );
}
