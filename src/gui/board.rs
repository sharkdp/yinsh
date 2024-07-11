use bevy::prelude::*;

use yinsh::{Coord, Player};

#[derive(Component)]
pub struct BoardElement(pub Coord, pub Player);

#[derive(Component)]
pub struct Ring;

#[derive(Component)]
pub struct Marker;
