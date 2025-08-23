use bevy::prelude::*;
use bevy_renet::renet::ClientId;
use serde::{Deserialize, Serialize};

use crate::cambio::{CambioAction, PlayerId};

#[derive(Debug, Serialize, Deserialize, Component, Clone)]
pub enum ServerMessage {
    PlayerConnected {
        player_id: PlayerId,
    },
    PlayerDisconnected {
        player_id: PlayerId,
    },
    StateUpdate {
        claimer_id: PlayerId,
        action: CambioAction,
    },
}

#[derive(Debug, Serialize, Deserialize, Component)]
pub enum ServerMessageUnreliable {
    MousePositions(Vec<(PlayerId, Vec2)>),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum ClientClaimUnreliable {
    MousePosition(Vec2),
}
