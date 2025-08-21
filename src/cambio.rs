use bevy::{platform::collections::HashMap, prelude::*};
use bevy_renet::renet::ClientId;
use serde::{Deserialize, Serialize};

#[derive(Deref, Debug, Serialize, Deserialize, Clone, Copy)]
pub struct PlayerId(pub usize);

#[derive(Component)]
pub struct IsHeldBy(pub ClientId);

#[derive(Debug, Serialize, Deserialize, Component, Clone, Copy)]
pub enum CardId {
    PlayerCard { owned_by: PlayerId, card_idx: usize },
    StackCard,
}

#[derive(Component)]
#[require(Transform, InheritedVisibility)]
pub struct CambioPlayerState {
    pub last_mouse_pos_world: Vec2,
    pub player_id: PlayerId,
    pub cards: Vec<Entity>,
}

pub struct CambioState {
    pub table_card: Entity,
    pub players: HashMap<ClientId, Entity>,
}

impl CambioState {
    pub fn get_card(&self, card_id: CardId) -> Entity {
        match card_id {
            CardId::PlayerCard { owned_by, card_idx } => todo!(),
            CardId::StackCard => self.table_card,
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum CambioAction {
    PickUpCard { card: CardId },
}
