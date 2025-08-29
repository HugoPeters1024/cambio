use bevy::prelude::*;
use serde::{Deserialize, Serialize};

use crate::{
    cambio::{CardId, PlayerId, SlotId}, cards::KnownCard,
};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum ClientClaim {
    PickUpSlotCard {
        slot_id: SlotId,
        card_id: CardId,
    },
    DropCardOnSlot {
        card_id: CardId,
        slot_id: SlotId,
    },
    LookAtCardAtSlot {
        card_id: CardId,
        slot_id: SlotId,
    },
    TakeFreshCardFromDeck,
    DropCardOnDiscardPile {
        card_id: CardId,
    },
    TakeCardFromDiscardPile,
    SwapHeldCardWithSlotCard {
        slot_id: SlotId,
        held_card_id: CardId,
    },
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum ServerMessage {
    PlayerConnected {
        player_id: PlayerId,
    },
    FinishedReplayingHistory,
    PlayerDisconnected {
        player_id: PlayerId,
    },
    ReceiveFreshSlot {
        actor: PlayerId,
        slot_id: SlotId,
    },
    ReceiveFreshCardFromDeck {
        actor: PlayerId,
        slot_id: SlotId,
        card_id: CardId,
    },
    RevealCardAtSlot {
        actor: PlayerId,
        card_id: CardId,
        slot_id: SlotId,
    },
    /// Always means that some player picked up this
    /// card. This action is never taken as part of the
    /// normal turn flow, but can still happen during a turn.
    PickUpSlotCard {
        actor: PlayerId,
        slot_id: SlotId,
        card_id: CardId,
    },
    SwapHeldCardWithSlotCard {
        actor: PlayerId,
        slot_id: SlotId,
        held_card_id: CardId,
    },
    DropCardOnSlot {
        actor: PlayerId,
        card_id: CardId,
        slot_id: SlotId,
    },
    TakeFreshCardFromDeck {
        actor: PlayerId,
        card_id: CardId,
    },
    DropCardOnDiscardPile {
        actor: PlayerId,
        card_id: CardId,
        offset_x: f32,
        offset_y: f32,
        rotation: f32,
    },
    TakeCardFromDiscardPile {
        actor: PlayerId,
    },
    PlayerAtTurn {
        player_id: PlayerId,
    },
    PublishCardPublically {
        card_id: CardId,
        value: KnownCard,
    },
    PublishCardForPlayer {
        player_id: PlayerId,
        card_id: CardId,
        value: Option<KnownCard>,
    },
}

impl ServerMessage {
    pub fn redacted_for(&self, redacted_for: &PlayerId) -> ServerMessage {
        match self {
            ServerMessage::PublishCardForPlayer { player_id, card_id, .. } => {
                if redacted_for != player_id {
                    ServerMessage::PublishCardForPlayer { value: None, player_id: player_id.clone(), card_id: card_id.clone() }
                } else {
                    self.clone()
                }
            }
            _ => self.clone(),
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Component)]
pub enum ServerMessageUnreliable {
    MousePositions(Vec<(PlayerId, Vec2)>),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum ClientClaimUnreliable {
    MousePosition(Vec2),
}
