use bevy::prelude::*;
use serde::{Deserialize, Serialize};

use crate::{
    cambio::{CardId, PlayerId, SlotId},
    cards::KnownCard,
};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum ClientClaim {
    PickUpSlotCard { slot_id: SlotId },
    DropCardOnSlot { card_id: CardId, slot_id: SlotId },
    LookAtCard { card_id: CardId },
    TakeFreshCardFromDeck,
    DropHeldCardOnDiscardPile { card_id: CardId },
    TakeCardFromDiscardPile,
    SwapHeldCardWithSlotCard { slot_id: SlotId },
}

#[derive(Debug, Serialize, Deserialize, Clone)]
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
    ReceiveFreshCard {
        actor: PlayerId,
        slot_id: SlotId,
        card_id: CardId,
    },
    RevealCard {
        actor: PlayerId,
        card_id: CardId,
        value: Option<KnownCard>,
    },
    PickUpSlotCard {
        actor: PlayerId,
        slot_id: SlotId,
    },
    SwapHeldCardWithSlotCard {
        actor: PlayerId,
        slot_id: SlotId,
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
    DropHeldCardOnDiscardPile {
        actor: PlayerId,
        card_id: CardId,
        // not every player knows the value at
        // this point yet, but this action reveals
        // it for everyone.
        value: KnownCard,
        local_transform: Transform,
    },
    TakeCardFromDiscardPile {
        actor: PlayerId,
    },
    PlayerAtTurn {
        player_id: PlayerId,
    },
}

impl ServerMessage {
    pub fn redacted_for(&self, player_id: &PlayerId) -> ServerMessage {
        match self {
            ServerMessage::RevealCard { actor, card_id, .. } => {
                // Only the actor can see the card
                if actor == player_id {
                    self.clone()
                } else {
                    ServerMessage::RevealCard {
                        actor: *player_id,
                        card_id: *card_id,
                        value: None,
                    }
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
