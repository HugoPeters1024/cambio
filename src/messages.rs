use bevy::{platform::collections::HashMap, prelude::*};
use bevy_renet::renet::ClientId;
use serde::{Deserialize, Serialize};


#[derive(Debug, Serialize, Deserialize, Component)]
pub enum ServerMessages {
    PlayerConnected { id: ClientId },
    PlayerDisconnected { id: ClientId },
}

#[derive(Debug, Serialize, Deserialize, Component)]
pub enum ServerMessageUnreliable {
    MousePositions(HashMap<ClientId, Vec2>),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum ClientClaim {
}

#[derive(Debug, Serialize, Deserialize)]
pub enum ClientClaimUnreliable {
    MousePosition(Vec2),
}
