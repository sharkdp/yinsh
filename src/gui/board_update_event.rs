use bevy::prelude::Event;

use yinsh::{Coord, Player};

#[derive(Event)]
pub enum BoardUpdateEvent {
    AddRing(Coord, Player),
    AddMarker(Coord, Player),
    MoveRing(Coord, Coord),
    RemoveRing(Coord),
    RemoveRun(Vec<Coord>),
    FlipMarkers(Vec<Coord>),
}
