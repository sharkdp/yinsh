use bevy::prelude::Message;

use yinsh::{Coord, Player};

#[derive(Message)]
pub enum BoardUpdateEvent {
    AddRing(Coord, Player),
    AddMarker(Coord, Player),
    MoveRing(Coord, Coord),
    RemoveRing(Coord),
    RemoveRun(Vec<Coord>),
    FlipMarkers(Coord, Coord, Vec<Coord>),
}
