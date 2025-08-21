use bevy::prelude::*;
use bevy_renet::renet::ClientId;
use serde::{Deserialize, Serialize};

use crate::cambio::{CambioAction, PlayerId};

#[derive(Debug, Serialize, Deserialize, Component)]
pub enum ServerMessage {
    PlayerConnected {
        client_id: ClientId,
        player_idx: PlayerId,
    },
    PlayerDisconnected {
        client_id: ClientId,
    },
    StateUpdate {
        claimer_id: ClientId,
        action: CambioAction,
    },
}

#[derive(Debug, Serialize, Deserialize, Component)]
pub enum ServerMessageUnreliable {
    MousePositions(Vec<(ClientId, Vec2)>),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum ClientClaimUnreliable {
    MousePosition(Vec2),
}
