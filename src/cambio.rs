use bevy::prelude::*;

#[derive(Component)]
#[require(Transform, InheritedVisibility)]
pub struct CambioPlayerState {
    pub cards: Vec<Entity>,
}

pub struct CambioState {
    pub table_card: Entity,
    pub players: Vec<Entity>,
}
