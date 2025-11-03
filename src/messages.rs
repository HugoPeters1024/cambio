use std::time::Duration;

use bevy::{platform::collections::HashMap, prelude::*};
use serde::{Deserialize, Serialize};

use crate::{
    cambio::{CardId, PlayerId, SlotId},
    cards::KnownCard,
};

pub const RELIABLE_CHANNEL: usize = 0;
pub const UNRELIABLE_CHANNEL: usize = 1;

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub enum ReceivedCardContext {
    Normal,
    Penalty,
    // The first 2 cards in the game
    MayLookAt,
}

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
    LookAtOwnCards,
    VoteNextRound,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum ServerMessage {
    PlayerConnected(PlayerId),
    FinishedReplayingHistory(PlayerId),
    PlayerDisconnected(PlayerId),
    ReceiveFreshSlot(PlayerId, SlotId),
    ReceiveFreshCardFromDeck(PlayerId, SlotId, CardId, ReceivedCardContext),
    RevealCardAtSlot {
        actor: PlayerId,
        card_id: CardId,
        slot_id: SlotId,
        // should the actor have the turn
        check_turn: bool,
        // should everybody see the card?
        for_everyone: bool,
    },
    /// Always means that some player picked up this
    /// card. This action is never taken as part of the
    /// normal turn flow, but can still happen during a turn.
    PickUpSlotCard(PlayerId, SlotId, CardId),
    SwapHeldCardWithSlotCard {
        actor: PlayerId,
        slot_id: SlotId,
        slot_card_id: CardId,
        held_card_id: CardId,
    },
    DropCardOnSlot(PlayerId, CardId, SlotId),
    TakeFreshCardFromDeck(PlayerId, CardId),
    DropCardOnDiscardPile {
        actor: PlayerId,
        card_id: CardId,
        offset_x: f32,
        offset_y: f32,
        rotation: f32,
    },
    TakeCardFromDiscardPile(PlayerId, CardId),
    PlayerAtTurn(PlayerId),
    PublishCardPublically(CardId, KnownCard),
    PublishCardForPlayer(PlayerId, CardId, Option<KnownCard>),
    ShuffleDiscardPile {
        card_ids: Vec<CardId>,
        // The shuffle seed must be determistic
        // for replaying purposes
        shuffle_seed: u64,
    },
    SlapTable(PlayerId),
    ReturnHeldCardToSlot(PlayerId, SlotId, CardId),
    RoundWillFinishIn(Duration),
    RoundFinished {
        all_cards: HashMap<CardId, KnownCard>,
        final_scores: HashMap<PlayerId, i32>,
        cumulative_scores: HashMap<PlayerId, i32>,
        is_game_over: bool,
    },
    SetUsername(PlayerId, String),
    VoteNextRound(PlayerId),
    ResetRound,
}

impl ServerMessage {
    pub fn redacted_for(&self, redacted_for: &PlayerId) -> ServerMessage {
        match self {
            ServerMessage::PublishCardForPlayer(player_id, card_id, _) => {
                if redacted_for != player_id {
                    ServerMessage::PublishCardForPlayer(player_id.clone(), card_id.clone(), None)
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
