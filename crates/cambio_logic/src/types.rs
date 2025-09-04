use std::collections::VecDeque;

use bevy::{
    platform::collections::{HashMap, HashSet},
    prelude::*,
};
use serde::{Deserialize, Serialize};
use strum_macros::EnumIter;

#[derive(
    Debug, Component, Serialize, Deserialize, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord,
)]
pub struct PlayerId(pub u8);

impl PlayerId {
    pub fn player_number(&self) -> u8 {
        self.0 + 1
    }
    pub fn up_or_down(&self) -> f32 {
        if self.0 <= 2 { 1.0 } else { -1.0 }
    }
}

#[derive(Component)]
pub struct IsHeldBy(pub PlayerId);

/// Marker component that we're laid on the discard pile because the top
/// card matched the rank.
#[derive(Component)]
pub struct WasMatchingDiscard;

#[derive(
    Debug, Serialize, Deserialize, Component, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord,
)]
pub struct CardId(pub u64);

#[derive(
    Debug, Serialize, Deserialize, Component, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord,
)]
pub struct SlotId(pub u64);

#[derive(Component)]
pub struct TakenFromSlot(pub SlotId);

#[derive(Component)]
pub struct UnrevealKnownCardTimer(pub Timer);

#[derive(Component)]
pub struct MyPlayer;

#[derive(Component)]
pub struct HasImmunity;

/// Special out of turn buff for a player
/// indicating that they may dump a card
/// to another player.
#[derive(Component)]
pub struct MayGiveCardTo(pub PlayerId);

#[derive(Debug, PartialEq, Eq)]
pub enum TurnBuff {
    MayLookAtOwnCard,
    MayLookAtOtherPlayersCard,
    MaySwapTwoCards {
        has_swapped_from_slot: Option<SlotId>,
    },
    MayLookAtCardAndThenSwap {
        has_looked_at: Option<SlotId>,
        has_swapped_from_slot: Option<SlotId>,
    },
}

#[derive(Component, Debug, PartialEq, Eq)]
pub enum PlayerAtTurn {
    Start,
    TookDeckCard,
    HasBuff(TurnBuff),
    TookDiscardedCard,
    SwappedCard,
    Finished,
}

#[derive(Component)]
pub struct CambioRoot;

#[derive(Component)]
pub struct DiscardPile;

#[derive(Component)]
pub struct PlayerState {
    pub username: String,
    pub last_mouse_pos_world: Vec2,
    pub slots: HashSet<Entity>,
}

#[derive(Component, Clone)]
pub struct CambioState {
    pub free_cards: VecDeque<CardId>,
    pub discard_pile: VecDeque<CardId>,
    pub card_index: HashMap<CardId, Entity>,
    pub slot_index: HashMap<SlotId, Entity>,
    pub player_index: HashMap<PlayerId, Entity>,
    pub message_drain: MessageDrain,
    pub card_lookup: CardValueLookup,
    pub discard_pile_entity: Entity,
    pub game_finished: bool,
}

#[derive(Default, Clone)]
pub struct MessageDrain {
    pub accepted: VecDeque<ServerMessage>,
    pub rejected: VecDeque<ServerMessage>,
}

#[derive(Clone, Default)]
pub struct CardValueLookup(pub HashMap<CardId, KnownCard>);

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum ClientClaim {
    WantsToPlay {
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, Serialize, Deserialize, EnumIter)]
pub enum Suit {
    #[default]
    Spades,
    Hearts,
    Clubs,
    Diamonds,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, Serialize, Deserialize, EnumIter)]
pub enum Rank {
    #[default]
    Ace,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
}

#[derive(Debug, Component, PartialEq, Eq, Clone, Copy, Default, Serialize, Deserialize)]
pub struct KnownCard {
    pub suit: Suit,
    pub rank: Rank,
}

impl KnownCard {
    pub fn penalty_score(&self) -> i32 {
        match self.rank {
            Rank::Ace => 1,
            Rank::Two => 2,
            Rank::Three => 3,
            Rank::Four => 4,
            Rank::Five => 5,
            Rank::Six => 6,
            Rank::Seven => 7,
            Rank::Eight => 8,
            Rank::Nine => 9,
            Rank::Ten => 10,
            Rank::Jack => 10,
            Rank::Queen => 10,
            Rank::King => {
                if self.suit == Suit::Hearts || self.suit == Suit::Diamonds {
                    -1
                } else {
                    10
                }
            }
        }
    }
}

#[derive(Debug, Component, PartialEq, Eq, Clone, Copy, Default)]
pub struct SomeCard;

#[derive(Debug, Component, Default)]
pub struct CardSlot;
