use bevy::{platform::collections::HashMap, prelude::*};
use bevy_renet::renet::ClientId;
use serde::{Deserialize, Serialize};

use crate::cards::CardRef;

#[derive(Debug, Serialize, Deserialize, Component)]
pub enum ServerMessage {
    PlayerConnected { id: ClientId, player_idx: usize },
    PlayerDisconnected { id: ClientId },
    CardPickedUp { id: ClientId, card: CardRef },
    CardDropped { id: ClientId, position: Vec2 },
}

#[derive(Debug, Serialize, Deserialize, Component)]
pub enum ServerMessageUnreliable {
    MousePositions(HashMap<ClientId, Vec2>),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum ClientClaim {
    ClickCard {
        card_ref: CardRef,
        mouse_pos: Vec2,
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum ClientClaimUnreliable {
    MousePosition(Vec2),
}
