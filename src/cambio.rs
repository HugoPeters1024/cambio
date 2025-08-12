use crate::cards::*;
use bevy::prelude::*;

pub struct ServerCard {
    pub card: Card,
}

pub struct ClientCard {
    pub card: Card,
    pub entity: Entity,
}

pub struct CambioPlayerState<TCard> {
    pub cards: Vec<TCard>,
}

pub struct CambioState<TCard> {
    pub table_card: TCard,
    pub players: Vec<CambioPlayerState<TCard>>,
}
