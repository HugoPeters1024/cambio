use bevy::{platform::collections::HashMap, prelude::*};
use bevy_renet::renet::ClientId;
use serde::{Deserialize, Serialize};

use crate::cards::CardRef;


#[derive(Debug, Serialize, Deserialize, Component)]
pub enum ServerMessage {
    PlayerConnected { id: ClientId },
    PlayerDisconnected { id: ClientId },
    CardPickedUp { id: ClientId, card: CardRef },
}

#[derive(Debug, Serialize, Deserialize, Component)]
pub enum ServerMessageUnreliable {
    MousePositions(HashMap<ClientId, Vec2>),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum ClientClaim {
    PickupCard(CardRef),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum ClientClaimUnreliable {
    MousePosition(Vec2),
}
