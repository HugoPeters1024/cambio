use bevy::prelude::*;
use serde::{Deserialize, Serialize};

use crate::{
    cambio::{CardId, PlayerId, SlotId},
    cards::KnownCard,
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
    RevealCardAtSlot {
        actor: PlayerId,
        card_id: CardId,
        slot_id: SlotId,
        value: Option<KnownCard>,
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
        value: Option<KnownCard>,
    },
    DropCardOnDiscardPile {
        actor: PlayerId,
        card_id: CardId,
        // not every player knows the value at
        // this point yet, but this action reveals
        // it for everyone.
        value: KnownCard,
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
}

impl ServerMessage {
    pub fn redacted_for(&self, player_id: &PlayerId) -> ServerMessage {
        match self {
            ServerMessage::RevealCardAtSlot {
                actor,
                card_id,
                slot_id,
                ..
            } => {
                // Only the actor can see the card
                if actor == player_id {
                    self.clone()
                } else {
                    ServerMessage::RevealCardAtSlot {
                        actor: *actor,
                        card_id: *card_id,
                        slot_id: *slot_id,
                        value: None,
                    }
                }
            }
            ServerMessage::TakeFreshCardFromDeck { actor, card_id, .. } => {
                // Only the actor can see the card
                if actor == player_id {
                    self.clone()
                } else {
                    ServerMessage::TakeFreshCardFromDeck {
                        actor: *actor,
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
