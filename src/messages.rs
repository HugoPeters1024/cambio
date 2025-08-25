use bevy::prelude::*;
use serde::{Deserialize, Serialize};

use crate::cambio::{CardId, PlayerId, SlotId};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum ClientClaim {
    PickUpCard { card_id: CardId },
    DropCardOnSlot { card_id: CardId, slot_id: SlotId },
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum ServerMessage {
    PlayerConnected {
        player_id: PlayerId,
    },
    PlayerDisconnected {
        player_id: PlayerId,
    },
    ReceiveFreshSlot {
        actor: PlayerId,
        slot_id: SlotId,
    },
    ReceiveFreshCard {
        actor: PlayerId,
        slot_id: SlotId,
        card_id: CardId,
    },
    PickUpCard {
        actor: PlayerId,
        card_id: CardId,
    },
    DropCardOnSlot {
        actor: PlayerId,
        card_id: CardId,
        slot_id: SlotId,
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
