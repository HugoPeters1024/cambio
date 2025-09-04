use bevy::{platform::collections::HashMap, prelude::*};
use serde::{Deserialize, Serialize};

use crate::{
    cambio::{CardId, PlayerId, SlotId},
    cards::KnownCard,
};

pub const RELIABLE_CHANNEL: usize = 0;
pub const UNRELIABLE_CHANNEL: usize = 1;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum ClientClaim {
    SetUserName {
        username: String,
    },
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
    TakeCardFromDiscardPile {
        card_id: CardId,
    },
    SwapHeldCardWithSlotCard {
        slot_id: SlotId,
        slot_card_id: CardId,
        held_card_id: CardId,
    },
    SlapTable,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum ServerMessage {
    PlayerConnected {
        player_id: PlayerId,
    },
    FinishedReplayingHistory {
        player_id: PlayerId,
    },
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
        // should the actor have the turn
        check_turn: bool,
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
        slot_card_id: CardId,
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
        card_id: CardId,
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
    ShuffleDiscardPile {
        card_ids: Vec<CardId>,
    },
    SlapTable {
        actor: PlayerId,
    },
    GameFinished {
        all_cards: HashMap<CardId, KnownCard>,
        final_scores: HashMap<PlayerId, i32>,
    },
    SetUsername {
        actor: PlayerId,
        username: String,
    },
}

impl ServerMessage {
    pub fn redacted_for(&self, redacted_for: &PlayerId) -> ServerMessage {
        match self {
            ServerMessage::PublishCardForPlayer {
                player_id, card_id, ..
            } => {
                if redacted_for != player_id {
                    ServerMessage::PublishCardForPlayer {
                        value: None,
                        player_id: player_id.clone(),
                        card_id: card_id.clone(),
                    }
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
