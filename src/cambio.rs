use std::{
    collections::{HashSet, VecDeque},
    time::Duration,
};

use bevy::{platform::collections::HashMap, prelude::*};
use bevy_matchbox::prelude::PeerId;
use bevy_tweening::{Tween, TweenAnim, lens::TransformPositionLens};
use rand::{SeedableRng, rngs::StdRng, seq::SliceRandom};
use serde::{Deserialize, Serialize};

use crate::{
    assets::{DESIRED_CARD_HEIGHT, DESIRED_CARD_WIDTH},
    cards::*,
    messages::{ReceivedCardContext, ServerMessage},
};

#[derive(
    Debug, Component, Serialize, Deserialize, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord,
)]
pub struct PlayerId {
    pub player_index: u8,
    pub peer_id: PeerId,
}

impl PlayerId {
    pub fn player_number(&self) -> u8 {
        self.player_index + 1
    }
    pub fn up_or_down(&self) -> f32 {
        if self.player_index <= 2 { 1.0 } else { -1.0 }
    }
}

#[derive(Component)]
#[relationship(relationship_target = IsHoldingCard)]
pub struct IsHeldBy(pub Entity);

#[derive(Component)]
#[relationship_target(relationship = IsHeldBy)]
pub struct IsHoldingCard(Entity);

impl IsHoldingCard {
    pub fn card_entity(&self) -> Entity {
        self.0
    }
}

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
#[relationship(relationship_target = SlotHasCard)]
pub struct BelongsToSlot(pub Entity);

#[derive(Component)]
#[relationship_target(relationship = BelongsToSlot)]
pub struct SlotHasCard(Entity);

impl SlotHasCard {
    pub fn card_entity(&self) -> Entity {
        self.0
    }
}

#[derive(Component)]
pub struct UnrevealKnownCardTimer(pub Timer);

#[derive(Component)]
pub struct MyPlayer;

#[derive(Component)]
pub struct HasImmunity;

#[derive(Component)]
pub struct MayLookAt(pub PlayerId);

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
        swap_origin: Option<SlotId>,
    },
    MayLookAtCardAndThenSwap {
        has_looked_at: Option<SlotId>,
        swap_origin: Option<SlotId>,
    },
}

#[derive(Component, Debug, PartialEq, Eq)]
pub enum TurnState {
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

/// Tracks cumulative scores across multiple rounds
#[derive(Resource, Debug, Clone)]
pub struct GameScores {
    pub cumulative_scores: HashMap<PlayerId, i32>,
    pub is_game_over: bool,
}

impl Default for GameScores {
    fn default() -> Self {
        Self {
            cumulative_scores: HashMap::new(),
            is_game_over: false,
        }
    }
}

#[derive(Component)]
#[require(Transform, InheritedVisibility)]
pub struct PlayerState {
    pub username: String,
    pub last_mouse_pos_world: Vec2,
    pub slots: HashSet<SlotId>,
    pub voted_next_round: bool,
}

impl PlayerState {
    fn reset_round(&mut self) {
        self.slots.clear();
        self.voted_next_round = false;
    }
}

#[derive(Component, Clone)]
pub struct CambioState {
    pub free_cards: VecDeque<CardId>,
    pub discard_pile: VecDeque<CardId>,
    pub card_index: HashMap<CardId, Entity>,
    pub slot_index: HashMap<SlotId, Entity>,
    pub player_index: HashMap<PlayerId, Entity>,
    pub card_lookup: CardValueLookup,
    pub discard_pile_entity: Entity,
    pub round_finished: bool,
    pub round_will_finish_in: Option<Timer>,
    pub next_round_will_start_in: Option<Timer>,
}

impl CambioState {
    fn reset_round(&mut self, commands: &mut Commands) {
        self.free_cards = (0..52).map(CardId).collect();
        self.discard_pile.clear();
        for (_, entity) in self.card_index.drain() {
            commands.entity(entity).despawn();
        }
        for (_, entity) in self.slot_index.drain() {
            commands.entity(entity).despawn();
        }
        self.round_finished = false;
        self.round_will_finish_in = None;
        self.next_round_will_start_in = None;
    }
}

#[derive(Message)]
pub struct AcceptedMessage(pub ServerMessage);

#[derive(Debug, Clone)]
pub enum RejectionReason {
    Other(String),
    DiscardWrongRank { actor: PlayerId, card_id: CardId },
}

#[derive(Clone, Default)]
pub struct CardValueLookup(pub HashMap<CardId, KnownCard>);

pub struct CambioPlugin;

impl Plugin for CambioPlugin {
    fn build(&self, app: &mut App) {
        app.add_message::<AcceptedMessage>();
        app.add_systems(Update, handle_unreveal_timer);
    }
}

fn handle_unreveal_timer(
    mut commands: Commands,
    time: Res<Time>,
    mut query: Query<(Entity, &mut UnrevealKnownCardTimer)>,
) {
    for (entity, mut timer) in query.iter_mut() {
        timer.0.tick(time.delta());
        if timer.0.is_finished() {
            commands
                .entity(entity)
                .remove::<UnrevealKnownCardTimer>()
                .remove::<KnownCard>();
        }
    }
}

pub fn spawn_cambio_root(mut commands: Commands) {
    let discard_pile = commands
        .spawn((Name::new("Discard Pile"), DiscardPile))
        .id();

    let root = commands
        .spawn((
            Name::new("Cambio"),
            InheritedVisibility::default(),
            Transform::default(),
            CambioRoot,
            CambioState {
                free_cards: (0..52).map(CardId).collect(),
                discard_pile: VecDeque::new(),
                card_index: HashMap::new(),
                slot_index: HashMap::new(),
                player_index: HashMap::new(),
                card_lookup: CardValueLookup::default(),
                discard_pile_entity: discard_pile,
                round_finished: false,
                round_will_finish_in: None,
                next_round_will_start_in: None,
            },
        ))
        .id();

    commands.entity(discard_pile).insert(ChildOf(root));
}

pub fn process_single_event(
    In((root, msg)): In<(Entity, ServerMessage)>,
    mut states: Query<&mut CambioState>,
    mut players: Query<(&PlayerId, &mut PlayerState)>,
    (mut transforms, global_transforms): (Query<&mut Transform>, Query<&GlobalTransform>),
    (held_by, is_holding): (Query<(Entity, &IsHeldBy)>, Query<&IsHoldingCard>),
    my_player: Query<Entity, With<MyPlayer>>,
    (belongs_to_slot, has_card): (Query<&BelongsToSlot>, Query<&SlotHasCard>),
    (card_ids, slot_ids): (Query<&CardId>, Query<&SlotId>),
    known_cards: Query<&KnownCard>,
    matching_discard: Query<&WasMatchingDiscard>,
    (may_give_card_to, may_look_at): (Query<&MayGiveCardTo>, Query<&MayLookAt>),
    immunity: Query<&HasImmunity>,
    mut accepted: MessageWriter<AcceptedMessage>,
    mut player_at_turn: Query<(Entity, &mut TurnState)>,
    mut commands: Commands,
) -> Result<(), RejectionReason> {
    info!("Processing message: {:?}", msg);

    macro_rules! reject {
        ($($arg:tt)*) => {
            return Err(RejectionReason::Other(format!($($arg)*)));
        };
    }

    let Ok(mut state) = states.get_mut(root) else {
        reject!("Provided entity is not a cambio root");
    };

    if state.round_finished
        && !matches!(msg, ServerMessage::VoteNextRound(..))
        && !matches!(msg, ServerMessage::NextRoundWillStartIn(..))
        && !matches!(msg, ServerMessage::ResetRound)
    {
        reject!("Round is finished");
    }

    macro_rules! card_id_must_be_top_of_deck {
        ($card_id: expr) => {
            let Some(deck_card) = state.free_cards.pop_front() else {
                reject!("Deck is empty.");
            };

            if deck_card != *$card_id {
                reject!(
                    "Card {} is not top of deck, {} is.",
                    $card_id.0,
                    deck_card.0
                );
            }
        };
    }

    macro_rules! player_must_exists {
        ($player_id: expr) => {{
            let Some(player_entity) = state.player_index.get($player_id).cloned() else {
                reject!("Player {} does not exist.", $player_id.player_number());
            };
            player_entity
        }};
    }

    macro_rules! card_must_exists {
        ($card_id: expr) => {{
            let Some(card_entity) = state.card_index.get($card_id).cloned() else {
                reject!("Card {} does not exist.", $card_id.0);
            };
            card_entity
        }};
    }

    macro_rules! slot_must_exists {
        ($slot_id: expr) => {{
            let Some(slot_entity) = state.slot_index.get($slot_id).cloned() else {
                reject!("Slot {} does not exist.", $slot_id.0);
            };
            slot_entity
        }};
    }

    macro_rules! slot_must_have_card {
        ($slot_id: expr, $card_id: expr) => {{
            let slot_entity = slot_must_exists!($slot_id);
            let card_entity = card_must_exists!($card_id);
            if let Ok(SlotHasCard(has_card)) = has_card.get(slot_entity) {
                if *has_card != card_entity {
                    reject!("Slot {} does not have card {}.", $slot_id.0, $card_id.0);
                }
            } else {
                reject!("Slot {} does not have any card.", $slot_id.0);
            }
        }};
    }

    macro_rules! slot_must_not_have_card {
        ($slot_id: expr) => {{
            let slot_entity = slot_must_exists!($slot_id);
            if has_card.contains(slot_entity) {
                reject!("Slot {} already has a card.", $slot_id.0);
            }
        }};
    }

    macro_rules! player_must_be_holding_card {
        ($player_id: expr, $card_id: expr) => {{
            let player_entity = player_must_exists!($player_id);
            let card_entity = card_must_exists!($card_id);
            if let Ok(IsHoldingCard(holding_card_entity)) = is_holding.get(player_entity) {
                if *holding_card_entity != card_entity {
                    reject!(
                        "Player {} is not holding card {}.",
                        $player_id.player_number(),
                        $card_id.0
                    );
                }
            } else {
                reject!(
                    "Player {} is not holding any card",
                    $player_id.player_number()
                );
            }
        }};
    }

    macro_rules! player_must_not_be_holding_a_card {
        ($player_id: expr) => {{
            let player_entity = player_must_exists!($player_id);
            if is_holding.get(player_entity).is_ok() {
                reject!(
                    "Player {} is already holding a card.",
                    $player_id.player_number()
                );
            }
        }};
    }

    macro_rules! card_must_be_known_internally {
        ($card_id: expr) => {{
            let Some(known_card) = state.card_lookup.0.get($card_id) else {
                reject!("Card {} should have known value.", $card_id.0);
            };
            known_card
        }};
    }

    macro_rules! card_must_not_be_held {
        ($card_id: expr) => {{
            let card_entity = card_must_exists!($card_id);
            if is_holding.get(card_entity).is_ok() {
                reject!("Card {} is already held.", $card_id.0);
            }
        }};
    }

    macro_rules! must_have_turn {
        ($player_id: expr) => {{
            let player_entity = player_must_exists!($player_id);
            let Ok((_, _)) = player_at_turn.get(player_entity) else {
                reject!(
                    "Player {} is acting out of turn.",
                    $player_id.player_number()
                );
            };
            player_at_turn.get_mut(player_entity).unwrap().1
        }};
    }

    macro_rules! slot_owner {
        ($slot_id: expr) => {{
            players
                .iter()
                .find(|(_, player_state)| player_state.slots.contains($slot_id))
                .unwrap()
                .0
        }};
    }

    macro_rules! slot_owner_must_not_be_immune {
        ($slot_id: expr) => {{
            let player_id = slot_owner!($slot_id);
            let player_entity = player_must_exists!(player_id);
            if immunity.contains(player_entity) {
                reject!("Player {} is immune.", player_id.player_number());
            }
        }};
    }

    macro_rules! actor_is_me {
        ($actor: expr) => {{
            if let Some(my_player_entity) = my_player.iter().next() {
                let my_player_id = players.get(my_player_entity).unwrap().0;
                my_player_id == $actor
            } else {
                false
            }
        }};
    }

    match &msg {
        ServerMessage::PlayerConnected(player_id) => {
            if state.player_index.contains_key(player_id) {
                reject!("Player {} is already connected.", player_id.player_number());
            }

            let pid = player_id.player_index as usize;
            let x = -250.0 + 250.0 * (pid % 3) as f32;
            let y = -150.0 + 300.0 * (pid / 3) as f32;

            let player_entity = commands
                .spawn((
                    *player_id,
                    PlayerState {
                        last_mouse_pos_world: Vec2::ZERO,
                        slots: HashSet::new(),
                        username: format!("Player {}", player_id.player_number()),
                        voted_next_round: false,
                    },
                    Name::new(format!("Player {}", player_id.player_number())),
                    Transform::from_xyz(x, y, 0.0),
                    ChildOf(root),
                ))
                .id();

            state.player_index.insert(*player_id, player_entity);
        }
        ServerMessage::NextRoundWillStartIn(duration) => {
            state.next_round_will_start_in = Some(Timer::new(duration.clone(), TimerMode::Once));
        },
        ServerMessage::VoteNextRound(actor) => {
            let player_entity = player_must_exists!(actor);
            let Ok((_, mut player)) = players.get_mut(player_entity) else {
                reject!("Player index corrupted");
            };
            player.voted_next_round = true;
        }
        ServerMessage::ResetRound => {
            for (player_id, mut player) in players.iter_mut() {
                let player_entity = player_must_exists!(player_id);
                player.reset_round();
                commands
                    .entity(player_entity)
                    .remove::<MayGiveCardTo>()
                    .remove::<HasImmunity>();
            }

            state.reset_round(&mut commands);
        }
        ServerMessage::PlayerDisconnected(player_id) => {
            let _ = player_must_exists!(player_id);
            state.player_index.remove(player_id);
        }
        ServerMessage::SetUsername(actor, username) => {
            let player_entity = player_must_exists!(actor);
            let Ok(mut player) = players.get_mut(player_entity) else {
                reject!("Player index corrupted");
            };
            player.1.username = username.clone();
        }
        ServerMessage::ReceiveFreshSlot(actor, slot_id) => {
            let player_entity = player_must_exists!(actor);
            if state.slot_index.contains_key(slot_id) {
                reject!("Slot {} already exists.", slot_id.0);
            }

            let Ok(mut player) = players.get_mut(player_entity) else {
                reject!("Player index corrupted");
            };

            let slot_idx = player.1.slots.len();
            if slot_idx >= 8 {
                reject!(
                    "Player {} already has the maximum number of slots",
                    actor.player_number()
                );
            }

            // Tiling the slots like:
            // 7 2 3 5
            // 6 0 1 4
            let (slot_x, slot_y): (i32, i32) = match slot_idx {
                0 => (0, 0),
                1 => (1, 0),
                2 => (0, 1),
                3 => (1, 1),
                4 => (2, 0),
                5 => (2, 1),
                6 => (-1, 0),
                7 => (-1, 1),
                _ => unreachable!(),
            };

            const SLOT_WIDTH: f32 = DESIRED_CARD_WIDTH + 8.0;
            const SLOT_HEIGHT: f32 = DESIRED_CARD_HEIGHT + 8.0;

            let x = SLOT_WIDTH * slot_x as f32 - SLOT_WIDTH / 2.0;
            let y = SLOT_HEIGHT * slot_y as f32 - SLOT_HEIGHT / 2.0;

            let slot_entity = commands
                .spawn((
                    Name::new(format!("Slot {}", slot_idx)),
                    Transform::from_xyz(x, y, 0.0),
                    CardSlot,
                    *slot_id,
                    ChildOf(player_entity),
                ))
                .id();

            player.1.slots.insert(*slot_id);
            state.slot_index.insert(*slot_id, slot_entity);
        }
        ServerMessage::ReceiveFreshCardFromDeck(actor, slot_id, card_id, context) => {
            let _ = player_must_exists!(actor);
            let slot_entity = slot_must_exists!(slot_id);
            slot_must_not_have_card!(slot_id);

            card_id_must_be_top_of_deck!(card_id);
            let card_entity = commands
                .spawn((
                    SomeCard,
                    Name::new(format!("Card {}", card_id.0)),
                    *card_id,
                    Transform::from_xyz(0.0, 0.0, 1.0),
                    ChildOf(slot_entity),
                    BelongsToSlot(slot_entity),
                    Pickable::default(),
                ))
                .id();

            if *context == ReceivedCardContext::MayLookAt {
                commands.entity(card_entity).insert(MayLookAt(*actor));
            }

            state.card_index.insert(*card_id, card_entity);
        }
        ServerMessage::ReturnHeldCardToSlot(actor, slot_id, card_id) => {
            let _player_entity = player_must_exists!(actor);
            let card_entity = card_must_exists!(card_id);
            let slot_entity = slot_must_exists!(slot_id);
            slot_must_have_card!(slot_id, card_id);

            let global_position_card = global_transforms.get(card_entity).unwrap().translation();
            let global_position_slot = global_transforms.get(slot_entity).unwrap().translation();

            let slot_to_card = global_position_card - global_position_slot;

            let tween = Tween::new(
                EaseFunction::CubicOut,
                Duration::from_millis(600),
                TransformPositionLens {
                    start: slot_to_card,
                    end: Vec3::new(0.0, 0.0, 1.0),
                },
            );

            commands
                .entity(card_entity)
                .remove::<IsHeldBy>()
                .insert(Transform::from_xyz(0.0, 0.0, 1.0))
                .insert(TweenAnim::new(tween))
                .insert(ChildOf(slot_entity))
                .insert(Pickable::default());
        }
        ServerMessage::PickUpSlotCard(actor, slot_id, card_id) => {
            let player_entity = player_must_exists!(actor);
            let card_entity = card_must_exists!(card_id);
            let _slot_entity = slot_must_exists!(slot_id);
            slot_must_have_card!(slot_id, card_id);
            player_must_not_be_holding_a_card!(actor);
            card_must_not_be_held!(card_id);
            slot_must_have_card!(slot_id, card_id);

            let slot_owner = slot_owner!(slot_id);
            if slot_owner != actor {
                slot_owner_must_not_be_immune!(slot_id);
            }

            if held_by.contains(card_entity) {
                reject!("Card is both attached to a slot and held?!");
            }

            commands
                .entity(card_entity)
                .insert(IsHeldBy(player_entity))
                .insert(Transform::from_xyz(0.0, 0.0, 10.0))
                .remove::<ChildOf>()
                .remove::<Pickable>();
        }
        ServerMessage::DropCardOnSlot(actor, card_id, slot_id) => {
            let player_entity = player_must_exists!(actor);
            let slot_entity = slot_must_exists!(slot_id);
            let card_entity = card_must_exists!(card_id);
            player_must_be_holding_card!(actor, card_id);

            if let Ok(BelongsToSlot(originating_slot_entity)) = belongs_to_slot.get(card_entity)
                && let Ok(MayGiveCardTo(player_to_screw)) = may_give_card_to.get(player_entity)
            {
                let originating_slot = slot_ids.get(*originating_slot_entity).unwrap();
                let slot_owner = slot_owner!(slot_id);
                let card_owner = slot_owner!(originating_slot);

                // Putting it back where it game from is always valid
                if originating_slot != slot_id {
                    // But if it is somewhere else then...

                    // 1. It must be empty
                    slot_must_not_have_card!(slot_id);

                    // 2. It must be on a slot of the player we are screwing
                    if slot_owner != player_to_screw {
                        reject!(
                            "You can only give a card to player {} right now.",
                            player_to_screw.player_number()
                        );
                    }

                    // 2. The card must be ours
                    if card_owner != actor {
                        reject!(
                            "You can only give one of your cards to player {} right now.",
                            player_to_screw.player_number()
                        );
                    }

                    // 3. The owner of the slot may not be immune
                    slot_owner_must_not_be_immune!(slot_id);

                    // 3. Then we can complete the action
                    commands.entity(player_entity).remove::<MayGiveCardTo>();
                }
            } else if let Ok(BelongsToSlot(originating_slot_entity)) =
                belongs_to_slot.get(card_entity)
                && !player_at_turn.contains(player_entity)
            {
                let originating_slot = slot_ids.get(*originating_slot_entity).unwrap();
                // We are specifically doing an out of turn drop with a card that originates from
                // another player's slot. The only way to drop this card on a slot is by returning
                // it to the origin.
                if originating_slot != slot_id {
                    reject!("Cannot put card back on a different slot.");
                }
            } else {
                let mut turn_state = must_have_turn!(actor);

                if let Ok(BelongsToSlot(taken_from_slot_entity)) = belongs_to_slot.get(card_entity)
                {
                    let taken_from_slot = slot_ids.get(*taken_from_slot_entity).unwrap();
                    match &*turn_state {
                        TurnState::HasBuff(TurnBuff::MaySwapTwoCards {
                            swap_origin: Some(swap_origin),
                        })
                        | TurnState::HasBuff(TurnBuff::MayLookAtCardAndThenSwap {
                            swap_origin: Some(swap_origin),
                            ..
                        }) => {
                            if swap_origin != slot_id {
                                reject!(
                                    "You may only drop put this card back on the slot where it was swapped from"
                                );
                            }
                            *turn_state = TurnState::Finished;
                        }
                        _ => {
                            if taken_from_slot != slot_id {
                                reject!(
                                    "You may only drop this card back on the slot it was taken from"
                                );
                            }
                        }
                    }
                } else {
                    reject!(
                        "You cannot lay down a card on a slot that you didn't pick up from another player"
                    );
                }
            }

            commands
                .entity(card_entity)
                .remove::<IsHeldBy>()
                .remove::<KnownCard>()
                .insert(ChildOf(slot_entity))
                .insert(Pickable::default())
                .insert(BelongsToSlot(slot_entity))
                .insert(Transform::from_xyz(0.0, 0.0, 1.0));
        }
        ServerMessage::RevealCardAtSlot {
            actor,
            card_id,
            slot_id,
            check_turn,
            for_everyone,
        } => {
            let _player_entity = player_must_exists!(actor);
            let card_entity = card_must_exists!(card_id);
            let _slot_entity = slot_must_exists!(slot_id);
            slot_must_have_card!(slot_id, card_id);
            slot_owner_must_not_be_immune!(slot_id);
            let slot_owner = slot_owner!(slot_id);

            if let Ok(may_look_at) = may_look_at.get(card_entity)
                && may_look_at.0 == *actor
            {
                // All good! These are the initial reveals at the start of the game
                commands.entity(card_entity).remove::<MayLookAt>();
            } else if *check_turn {
                let mut turn_state = must_have_turn!(actor);

                match &*turn_state {
                    TurnState::HasBuff(buff) => match buff {
                        TurnBuff::MayLookAtOwnCard => {
                            if actor != slot_owner {
                                reject!("Cannot reveal another player's card.");
                            }
                            *turn_state = TurnState::Finished;
                        }
                        TurnBuff::MayLookAtOtherPlayersCard => {
                            if actor == slot_owner {
                                reject!("Cannot reveal your own card.");
                            }
                            *turn_state = TurnState::Finished;
                        }
                        TurnBuff::MaySwapTwoCards { .. } => {
                            reject!("Cannot reveal a card at this point.");
                        }
                        TurnBuff::MayLookAtCardAndThenSwap { has_looked_at, .. } => {
                            if has_looked_at.is_some() {
                                reject!("You already looked at a card.");
                            }
                            *turn_state = TurnState::HasBuff(TurnBuff::MayLookAtCardAndThenSwap {
                                has_looked_at: Some(*slot_id),
                                swap_origin: None,
                            })
                        }
                    },
                    _ => {
                        reject!("Cannot reveal a card at this point.");
                    }
                }
            }

            // Only reveal the card if we're the one who picked it up
            if actor_is_me!(actor) || *for_everyone {
                if let Some(known_card) = state.card_lookup.0.get(card_id) {
                    commands.entity(card_entity).insert(*known_card).insert(
                        UnrevealKnownCardTimer(Timer::from_seconds(3.0, TimerMode::Once)),
                    );
                }
            }
        }
        ServerMessage::TakeFreshCardFromDeck(actor, card_id) => {
            let player_entity = player_must_exists!(actor);
            let mut turn_state = must_have_turn!(actor);

            if *turn_state != TurnState::Start {
                reject!(
                    "Can only take cards at the start of a turn, but turn state is {:?}.",
                    turn_state
                );
            }

            player_must_not_be_holding_a_card!(actor);
            card_id_must_be_top_of_deck!(card_id);
            *turn_state = TurnState::TookDeckCard;
            let card_entity = commands
                .spawn((
                    SomeCard,
                    *card_id,
                    Name::new(format!("Card {}", card_id.0)),
                    IsHeldBy(player_entity),
                    Transform::from_xyz(0.0, 0.0, 10.0),
                ))
                .id();

            // We assume that the server will JIT publish the card for the
            // right player before sending this message.
            if actor_is_me!(actor)
                && let Some(known_card) = state.card_lookup.0.get(card_id)
            {
                commands.entity(card_entity).insert(*known_card);
            }

            state.card_index.insert(*card_id, card_entity);
        }
        ServerMessage::DropCardOnDiscardPile {
            actor,
            card_id,
            offset_x,
            offset_y,
            rotation,
        } => {
            let player_entity = player_must_exists!(actor);
            let card_entity = card_must_exists!(card_id);
            player_must_be_holding_card!(actor, card_id);
            let known_value = card_must_be_known_internally!(card_id);

            if let Ok(BelongsToSlot(origin_slot_entity)) = belongs_to_slot.get(card_entity) {
                let origin_slot_id = slot_ids.get(*origin_slot_entity).unwrap();
                let Some(top_card) = state.discard_pile.front() else {
                    reject!("Discard pile is empty");
                };

                let top_card_entity = card_must_exists!(top_card);

                if matching_discard.contains(top_card_entity) {
                    reject!("The top card was already a matching discard by another player");
                }

                let Ok(top_card) = known_cards.get(top_card_entity) else {
                    reject!("How is the discard pile's top card not known?!");
                };

                if top_card.rank != known_value.rank {
                    return Err(RejectionReason::DiscardWrongRank {
                        actor: *actor,
                        card_id: *card_id,
                    });
                }

                let stolen_from = slot_owner!(origin_slot_id);
                if stolen_from != actor {
                    // This player claimed a match using another player's card!
                    // They now get to give one of their cards to this player
                    commands
                        .entity(player_entity)
                        .insert(MayGiveCardTo(*stolen_from));
                }

                // It might actually be the case that this player does currently have a
                // turn and they have just swapped to cards. In that case this is just a
                // shortcut way to end the turn.
                if let Ok((_, mut turn_state)) = player_at_turn.get_mut(player_entity) {
                    match *turn_state {
                        TurnState::HasBuff(TurnBuff::MaySwapTwoCards {
                            swap_origin: Some(_),
                        }) => {
                            *turn_state = TurnState::Finished;
                        }
                        TurnState::HasBuff(TurnBuff::MayLookAtCardAndThenSwap {
                            swap_origin: Some(_),
                            ..
                        }) => {
                            *turn_state = TurnState::Finished;
                        }
                        _ => {}
                    };
                }

                // Mark the card as used for a discard opportunity, preventing others
                // from doing it again on the same card.
                commands.entity(card_entity).insert(WasMatchingDiscard);

                // Player is has claimed a discard opportunity!
                // The other players holding such a card were too late and we put the card back
                for IsHoldingCard(held_card) in is_holding.iter() {
                    if let Ok(BelongsToSlot(originating_slot_entity)) =
                        belongs_to_slot.get(*held_card)
                    {
                        // This card is held by someone else trying to claim a dicard opportunity,
                        // but they were to late! Let's put the card back where it came from.
                        commands
                            .entity(*held_card)
                            .remove::<IsHeldBy>()
                            .remove::<KnownCard>()
                            .insert(ChildOf(*originating_slot_entity))
                            .insert(Pickable::default())
                            .insert(Transform::from_xyz(0.0, 0.0, 1.0));
                    }
                }
            } else {
                let mut turn_state = must_have_turn!(actor);
                match *turn_state {
                    TurnState::TookDeckCard => {
                        // Depending on the deck card that was discarded, players might
                        // receive a buff they can execute first.
                        if known_value.rank == Rank::Seven || known_value.rank == Rank::Eight {
                            *turn_state = TurnState::HasBuff(TurnBuff::MayLookAtOwnCard);
                        } else if known_value.rank == Rank::Nine || known_value.rank == Rank::Ten {
                            // Only if there is another elligble player to look at
                            if state
                                .player_index
                                .iter()
                                .filter(|(pid, pe)| *pid != actor && !immunity.contains(**pe))
                                .count()
                                == 0
                            {
                                *turn_state = TurnState::Finished;
                            } else {
                                *turn_state =
                                    TurnState::HasBuff(TurnBuff::MayLookAtOtherPlayersCard);
                            }
                        } else if known_value.rank == Rank::Jack || known_value.rank == Rank::Queen
                        {
                            *turn_state =
                                TurnState::HasBuff(TurnBuff::MaySwapTwoCards { swap_origin: None });
                        } else if known_value.rank == Rank::King {
                            *turn_state = TurnState::HasBuff(TurnBuff::MayLookAtCardAndThenSwap {
                                has_looked_at: None,
                                swap_origin: None,
                            });
                        } else {
                            *turn_state = TurnState::Finished;
                        }
                    }
                    TurnState::SwappedCard => {
                        *turn_state = TurnState::Finished;
                    }
                    TurnState::TookDiscardedCard => {
                        // Fine, the player is allowed to reconsider
                        *turn_state = TurnState::Start;
                    }
                    TurnState::Start | TurnState::HasBuff { .. } | TurnState::Finished => {
                        reject!("Couldn't be holding a card at this point.");
                    }
                }
            }

            let transform = Transform::from_xyz(
                *offset_x,
                *offset_y,
                0.1 + 0.1 * state.discard_pile.len() as f32,
            )
            .with_rotation(Quat::from_rotation_z(*rotation));

            commands
                .entity(card_entity)
                .remove::<IsHeldBy>()
                .remove::<BelongsToSlot>()
                .remove::<UnrevealKnownCardTimer>()
                .insert(*known_value)
                .insert(ChildOf(state.discard_pile_entity))
                .insert(transform);

            state.discard_pile.push_front(*card_id);
        }
        ServerMessage::PlayerAtTurn(player_id) => {
            let player_entity = player_must_exists!(player_id);

            for (entity, _) in player_at_turn.iter() {
                commands.entity(entity).remove::<TurnState>();
            }
            commands.entity(player_entity).insert(TurnState::Start);
        }
        ServerMessage::TakeCardFromDiscardPile(actor, card_id) => {
            let player_entity = player_must_exists!(actor);
            let card_entity = card_must_exists!(card_id);
            let mut turn_state = must_have_turn!(actor);
            player_must_not_be_holding_a_card!(actor);

            if state.discard_pile.len() == 0 {
                reject!("Discard pile is empty.");
            }

            if state.discard_pile[0] != *card_id {
                reject!("Provided card is not on top of the discard pile");
            }

            if *turn_state != TurnState::Start {
                reject!(
                    "Can only take cards at the start of a turn, but turn state is {:?}.",
                    turn_state
                );
            }
            *turn_state = TurnState::TookDiscardedCard;

            state.discard_pile.pop_front();
            commands
                .entity(card_entity)
                .insert(IsHeldBy(player_entity))
                .insert(Transform::from_xyz(0.0, 0.0, 10.0))
                .remove::<Pickable>()
                .remove::<ChildOf>();
        }
        ServerMessage::SwapHeldCardWithSlotCard {
            actor,
            slot_id,
            slot_card_id,
            held_card_id,
        } => {
            let player_entity = player_must_exists!(actor);
            let slot_entity = slot_must_exists!(slot_id);
            let held_card_entity = card_must_exists!(held_card_id);
            let slot_card_entity = card_must_exists!(slot_card_id);
            player_must_be_holding_card!(actor, held_card_id);
            slot_must_have_card!(slot_id, slot_card_id);
            card_must_not_be_held!(slot_card_id);
            slot_owner_must_not_be_immune!(slot_id);
            let slot_owner = slot_owner!(slot_id);

            if may_give_card_to.contains(player_entity) {
                reject!("Giving a card to another players is a buff that takes priority.");
            }

            let mut turn_state = must_have_turn!(actor);
            match *turn_state {
                TurnState::TookDeckCard | TurnState::TookDiscardedCard => {
                    if slot_owner != actor {
                        reject!("Cannot swap a card on another player's slot.");
                    }
                    *turn_state = TurnState::SwappedCard;
                }
                TurnState::Start => {
                    reject!("Cannot swap a card at the start of a turn.");
                }
                TurnState::Finished => {
                    reject!("Cannot swap a card at the end of a turn.");
                }
                TurnState::SwappedCard => {
                    reject!("Cannot swap a card twice.");
                }
                TurnState::HasBuff(TurnBuff::MayLookAtOwnCard) => {
                    reject!("Cannot swap a card at this point");
                }
                TurnState::HasBuff(TurnBuff::MayLookAtOtherPlayersCard) => {
                    reject!("Cannot swap a card at this point");
                }
                TurnState::HasBuff(TurnBuff::MaySwapTwoCards {
                    swap_origin: has_swapped_from_slot,
                }) => {
                    let Ok(BelongsToSlot(originating_slot_entity)) =
                        belongs_to_slot.get(held_card_entity)
                    else {
                        reject!("Can only swap cards taken from a player at this point");
                    };

                    let originating_slot = slot_ids.get(*originating_slot_entity).unwrap();
                    match has_swapped_from_slot {
                        Some(_) => {
                            reject!("Already swapped two cards");
                        }
                        None => {
                            *turn_state = TurnState::HasBuff(TurnBuff::MaySwapTwoCards {
                                swap_origin: Some(*originating_slot),
                            });
                        }
                    }
                }
                TurnState::HasBuff(TurnBuff::MayLookAtCardAndThenSwap {
                    has_looked_at,
                    swap_origin: has_swapped_from_slot,
                }) => {
                    let Some(has_looked_at_slot) = has_looked_at else {
                        reject!("You first need to look at a card");
                    };

                    if has_swapped_from_slot.is_some() {
                        reject!("Already swapped two cards");
                    }

                    let Ok(BelongsToSlot(originating_slot_entity)) =
                        belongs_to_slot.get(held_card_entity)
                    else {
                        reject!("Can only swap cards taken from a player at this point");
                    };

                    let originating_slot = slot_ids.get(*originating_slot_entity).unwrap();
                    if has_looked_at_slot != *originating_slot && has_looked_at_slot != *slot_id {
                        reject!("The swap has to include the card you looked at");
                    };

                    *turn_state = TurnState::HasBuff(TurnBuff::MayLookAtCardAndThenSwap {
                        has_looked_at: Some(has_looked_at_slot),
                        swap_origin: Some(*originating_slot),
                    });
                }
            }

            // make sure we inform the player that the card it is holding
            // is no longer the correct entity. If we do not do this we will get
            // a panic. Note that will not be necessary anymore in bevy 0.17:
            // https://github.com/bevyengine/bevy/pull/20232
            commands.entity(player_entity).remove::<IsHoldingCard>();
            commands.entity(slot_entity).remove::<SlotHasCard>();

            // pick up the slot card
            commands
                .entity(slot_card_entity)
                .remove::<ChildOf>()
                .remove::<BelongsToSlot>()
                .remove::<Pickable>()
                .insert(IsHeldBy(player_entity))
                .insert(Transform::from_xyz(0.0, 0.0, 10.0));

            // drop the held card
            commands
                .entity(held_card_entity)
                .remove::<IsHeldBy>()
                .remove::<KnownCard>()
                .remove::<BelongsToSlot>()
                .insert(ChildOf(slot_entity))
                .insert(BelongsToSlot(slot_entity))
                .insert(Transform::from_xyz(0.0, 0.0, 1.0))
                .insert(Pickable::default());

            // if the source card has a TakenFromSlot marker, we give the card
            // that will now give it to card that is now held. This will ensure
            // proper swapping logic when swapping two slot cards as part of a buff.
            if let Ok(BelongsToSlot(origin_slot)) = belongs_to_slot.get(held_card_entity) {
                commands
                    .entity(slot_card_entity)
                    .insert(BelongsToSlot(*origin_slot));
            }
        }
        ServerMessage::FinishedReplayingHistory(player_id) => {
            player_must_exists!(player_id);
        }
        ServerMessage::PublishCardPublically(card_id, value) => {
            state.card_lookup.0.insert(*card_id, *value);
        }
        ServerMessage::PublishCardForPlayer(_, card_id, value) => {
            if let Some(value) = value {
                state.card_lookup.0.insert(*card_id, *value);
            }
        }
        ServerMessage::ShuffleDiscardPile {
            card_ids,
            shuffle_seed,
        } => {
            if card_ids.is_empty() {
                reject!("Cannot shuffle an empty discard pile");
            }

            let mut cards_in_discard_pile: HashSet<CardId> =
                state.discard_pile.iter().copied().collect();

            for card_id in card_ids.iter() {
                let _ = card_must_exists!(card_id);
                if !cards_in_discard_pile.contains(card_id) {
                    reject!("Card {} is not in the discard pile", card_id.0);
                }
            }

            for card_id in card_ids.iter() {
                let card_entity = card_must_exists!(card_id);
                state.card_index.remove(card_id);
                state.free_cards.push_front(*card_id);
                cards_in_discard_pile.remove(card_id);
                commands.entity(card_entity).despawn();
            }

            state
                .discard_pile
                .retain(|card_entity| cards_in_discard_pile.contains(card_entity));

            // Reset the z position of the cards left in the discard pile
            for (i, card_id) in state.discard_pile.iter().enumerate() {
                let card_entity = card_must_exists!(card_id);
                if let Ok(mut transform) = transforms.get_mut(card_entity) {
                    transform.translation.z = 0.0 + 0.1 * i as f32;
                }
            }

            // These cards were returned to the deck, now we are free to
            // shuffle the card_id -> known_value assignment here on the host!
            // This is not strictly necessary but it prevents smart clients to
            // keep track of which card_ids correspond to which values.
            let mut ids_to_shuffle: Vec<CardId> = Vec::new();
            let mut values_to_shuffle: Vec<KnownCard> = Vec::new();

            for card_id in card_ids {
                let known_card = state.card_lookup.0.remove(card_id).unwrap().clone();

                ids_to_shuffle.push(*card_id);
                values_to_shuffle.push(known_card);
            }

            let mut entropy = StdRng::seed_from_u64(*shuffle_seed);
            ids_to_shuffle.shuffle(&mut entropy);

            for (card_id, known_card) in ids_to_shuffle.iter().zip(values_to_shuffle.iter()) {
                state.card_lookup.0.insert(*card_id, known_card.clone());
            }
        }
        ServerMessage::SlapTable(actor) => {
            if !immunity.is_empty() {
                reject!("Only one player can slap the table");
            }

            let player_entity = player_must_exists!(actor);
            let mut turn_state = must_have_turn!(actor);
            player_must_not_be_holding_a_card!(actor);

            if *turn_state != TurnState::Start {
                reject!("Can only slap the table at the start of a turn");
            }

            *turn_state = TurnState::Finished;
            commands.entity(player_entity).insert(HasImmunity);
        }
        ServerMessage::RoundWillFinishIn(duration) => {
            state.round_will_finish_in = Some(Timer::new(duration.clone(), TimerMode::Once));
        }
        ServerMessage::RoundFinished {
            all_cards,
            cumulative_scores: _,
            is_game_over: _,
            ..
        } => {
            for (card_id, known_card) in all_cards.iter() {
                state.card_lookup.0.insert(*card_id, *known_card);
            }

            // The game is finished! Anyone holding a card puts it back
            for (held_card, _) in held_by.iter() {
                if let Ok(BelongsToSlot(originating_slot_entity)) = belongs_to_slot.get(held_card) {
                    // This card is held by someone else trying to claim a dicard opportunity,
                    // but they were to late! Let's put the card back where it came from.
                    let held_card_id = card_ids.get(held_card).expect("Card index corrupted");

                    commands
                        .entity(held_card)
                        .remove::<IsHeldBy>()
                        .remove::<KnownCard>()
                        .remove::<UnrevealKnownCardTimer>()
                        .remove::<BelongsToSlot>()
                        .insert(ChildOf(*originating_slot_entity))
                        .insert(Pickable::default())
                        .insert(Transform::from_xyz(0.0, 0.0, 1.0));

                    if let Some(known_value) = state.card_lookup.0.get(held_card_id) {
                        commands.entity(held_card).insert(*known_value);
                    }
                }
            }

            for (_, player_state) in players.iter() {
                for slot_id in player_state.slots.iter() {
                    let slot_entity = slot_must_exists!(slot_id);
                    if let Ok(SlotHasCard(card_entity)) = has_card.get(slot_entity) {
                        if let Ok(card_id) = card_ids.get(*card_entity) {
                            if let Some(known_value) = state.card_lookup.0.get(card_id) {
                                commands
                                    .entity(*card_entity)
                                    .remove::<UnrevealKnownCardTimer>()
                                    .insert(*known_value);
                            }
                        }
                    }
                }
            }

            for (entity, _) in player_at_turn.iter() {
                commands.entity(entity).remove::<TurnState>();
            }

            for (card_id, known_card) in all_cards.iter() {
                if let Some(card_entity) = state.card_index.get(card_id) {
                    commands.entity(*card_entity).insert(*known_card);
                }
            }

            state.round_finished = true;
            state.round_will_finish_in = None;
        }
    }

    // If we're still here then we accepted the message.
    accepted.write(AcceptedMessage(msg.clone()));
    Ok(())
}

#[cfg(test)]
mod tests {
    use bevy::{asset::uuid::Uuid, ecs::message::MessageRegistry};
    use strum::IntoEnumIterator;

    use crate::messages::ReceivedCardContext;

    use super::*;

    const fn player0() -> PlayerId {
        PlayerId {
            player_index: 0,
            peer_id: PeerId(Uuid::nil()),
        }
    }

    const fn player1() -> PlayerId {
        PlayerId {
            player_index: 1,
            peer_id: PeerId(Uuid::nil()),
        }
    }

    #[derive(Debug)]
    struct TestSetup {
        root: Entity,
        world: World,
    }

    impl TestSetup {
        fn new() -> TestSetup {
            let mut world = World::new();
            MessageRegistry::register_message::<AcceptedMessage>(&mut world);

            world.run_system_cached(spawn_cambio_root).unwrap();
            let root = world
                .query::<(Entity, &CambioState)>()
                .single(&world)
                .unwrap()
                .0;

            TestSetup { root, world }
        }

        fn with_card_on_discard_pile(mut self, card_id: CardId, value: KnownCard) -> TestSetup {
            let discard_pile_entity = self
                .world
                .query::<(Entity, &mut DiscardPile)>()
                .single(&self.world)
                .unwrap()
                .0;

            let card_entity = self
                .world
                .spawn((SomeCard, card_id, value, ChildOf(discard_pile_entity)))
                .id();

            let mut state = self
                .world
                .query::<&mut CambioState>()
                .get_mut(&mut self.world, self.root)
                .unwrap();
            state.discard_pile.push_front(card_id);
            state.card_index.insert(card_id, card_entity);

            self.accepts(&ServerMessage::PublishCardPublically(card_id, value));

            self
        }

        fn with_events(mut self, events: &[ServerMessage]) -> TestSetup {
            for event in events {
                self.accepts(event);
            }
            self
        }

        fn with_event(self, event: ServerMessage) -> TestSetup {
            self.with_events(&[event])
        }

        fn accepts(&mut self, event: &ServerMessage) {
            match self.run_event_inplace(event) {
                Ok(_) => {}
                Err(reason) => assert!(
                    false,
                    "Unexpectedly rejected event: {:?} for reason ({:?})",
                    event, reason
                ),
            }
        }

        fn rejects(&mut self, event: &ServerMessage) {
            assert!(
                !self.run_event_inplace(event).is_ok(),
                "Unexpectedly accepted event: {:?}",
                event
            );
        }

        fn run_event_inplace(&mut self, event: &ServerMessage) -> Result<(), RejectionReason> {
            self.world
                .run_system_cached_with(process_single_event, (self.root, event.clone()))
                .unwrap()
        }

        fn with_all_cards_known(mut self) -> TestSetup {
            for (card_id, card) in Rank::iter()
                .flat_map(|rank| Suit::iter().map(move |suit| KnownCard { rank, suit }))
                .enumerate()
                .map(|(index, card)| (CardId(index as u64), card))
            {
                self.accepts(&ServerMessage::PublishCardPublically(card_id, card));
            }
            self
        }

        fn one_player_one_card_start_of_turn() -> TestSetup {
            TestSetup::new().with_events(&[
                ServerMessage::PlayerConnected(player0()),
                ServerMessage::ReceiveFreshSlot(player0(), SlotId(0)),
                ServerMessage::ReceiveFreshCardFromDeck(
                    player0(),
                    SlotId(0),
                    CardId(0),
                    ReceivedCardContext::Normal,
                ),
                ServerMessage::PlayerAtTurn(player0()),
            ])
        }

        fn one_player_with_slot_cards(cards: Vec<(CardId, KnownCard)>) -> Self {
            let mut env =
                TestSetup::new().with_events(&[ServerMessage::PlayerConnected(player0())]);

            for (i, (card_id, card)) in cards.iter().enumerate() {
                env.accepts(&ServerMessage::PublishCardPublically(
                    *card_id,
                    card.clone(),
                ));
                env.accepts(&ServerMessage::ReceiveFreshSlot(
                    player0(),
                    SlotId(i as u64),
                ));
                env.accepts(&ServerMessage::ReceiveFreshCardFromDeck(
                    player0(),
                    SlotId(i as u64),
                    *card_id,
                    ReceivedCardContext::Normal,
                ));
            }

            env.accepts(&ServerMessage::PlayerAtTurn(player0()));

            env
        }

        fn one_player_two_cards_start_of_turn() -> TestSetup {
            TestSetup::new().with_events(&[
                ServerMessage::PlayerConnected(player0()),
                ServerMessage::ReceiveFreshSlot(player0(), SlotId(0)),
                ServerMessage::ReceiveFreshCardFromDeck(
                    player0(),
                    SlotId(0),
                    CardId(0),
                    ReceivedCardContext::Normal,
                ),
                ServerMessage::ReceiveFreshSlot(player0(), SlotId(1)),
                ServerMessage::ReceiveFreshCardFromDeck(
                    player0(),
                    SlotId(1),
                    CardId(1),
                    ReceivedCardContext::Normal,
                ),
                ServerMessage::PlayerAtTurn(player0()),
            ])
        }

        fn two_players_one_cards() -> TestSetup {
            Self::one_player_one_card_start_of_turn().with_events(&[
                ServerMessage::PlayerConnected(player1()),
                ServerMessage::ReceiveFreshSlot(player1(), SlotId(1)),
                ServerMessage::ReceiveFreshCardFromDeck(
                    player1(),
                    SlotId(1),
                    CardId(1),
                    ReceivedCardContext::Normal,
                ),
            ])
        }

        fn get_world(self) -> World {
            self.world
        }
    }

    #[test]
    fn test_player_cannot_connect_twice() {
        let mut env = TestSetup::new().with_event(ServerMessage::PlayerConnected(player0()));

        env.rejects(&ServerMessage::PlayerConnected(player0()));
    }

    #[rstest::rstest]
    fn putting_down_an_seven_or_eight_gives_buff(
        #[values(KnownCard { rank: Rank::Seven, suit: Suit::Hearts }, KnownCard { rank: Rank::Eight, suit: Suit::Hearts })]
        known_card: KnownCard,
    ) {
        let mut world = TestSetup::one_player_one_card_start_of_turn()
            .with_all_cards_known()
            .with_events(&[
                ServerMessage::PublishCardForPlayer(player0(), CardId(1), Some(known_card)),
                ServerMessage::TakeFreshCardFromDeck(player0(), CardId(1)),
                ServerMessage::DropCardOnDiscardPile {
                    actor: player0(),
                    card_id: CardId(1),
                    offset_x: 0.0,
                    offset_y: 0.0,
                    rotation: 0.0,
                },
            ])
            .get_world();

        let state = world
            .query::<&mut CambioState>()
            .single_mut(&mut world)
            .unwrap()
            .clone();
        let turn_state = world
            .get::<TurnState>(state.player_index[&player0()])
            .unwrap();
        assert_eq!(*turn_state, TurnState::HasBuff(TurnBuff::MayLookAtOwnCard));
    }

    #[test]
    fn take_from_deck_and_swap_flow() {
        let mut world = TestSetup::one_player_one_card_start_of_turn()
            .with_all_cards_known()
            .with_events(&[
                ServerMessage::TakeFreshCardFromDeck(player0(), CardId(1)),
                ServerMessage::SwapHeldCardWithSlotCard {
                    actor: player0(),
                    slot_card_id: CardId(0),
                    held_card_id: CardId(1),
                    slot_id: SlotId(0),
                },
                ServerMessage::DropCardOnDiscardPile {
                    actor: player0(),
                    card_id: CardId(0),
                    offset_x: 0.0,
                    offset_y: 0.0,
                    rotation: 0.0,
                },
            ])
            .get_world();

        let state = world
            .query::<&mut CambioState>()
            .single_mut(&mut world)
            .unwrap()
            .clone();
        let turn_state = world
            .get::<TurnState>(state.player_index[&player0()])
            .unwrap();

        assert_eq!(*turn_state, TurnState::Finished);
    }

    #[test]
    fn take_from_discard_pile_and_swap_flow() {
        TestSetup::one_player_one_card_start_of_turn()
            .with_card_on_discard_pile(
                CardId(5),
                KnownCard {
                    rank: Rank::Ace,
                    suit: Suit::Hearts,
                },
            )
            .with_events(&[
                ServerMessage::TakeCardFromDiscardPile(player0(), CardId(5)),
                ServerMessage::SwapHeldCardWithSlotCard {
                    actor: player0(),
                    held_card_id: CardId(5),
                    slot_card_id: CardId(0),
                    slot_id: SlotId(0),
                },
                ServerMessage::PublishCardPublically(
                    CardId(0),
                    KnownCard {
                        rank: Rank::King,
                        suit: Suit::Hearts,
                    },
                ),
                ServerMessage::DropCardOnDiscardPile {
                    actor: player0(),
                    card_id: CardId(0),
                    offset_x: 0.0,
                    offset_y: 0.0,
                    rotation: 0.0,
                },
            ]);
    }

    #[test]
    fn taking_card_from_deck_must_match_state() {
        TestSetup::one_player_one_card_start_of_turn()
            .with_all_cards_known()
            .rejects(&ServerMessage::TakeFreshCardFromDeck(player0(), CardId(30)));
    }

    #[test]
    fn taking_cards_from_deck_will_exhaust_deck() {
        let mut env = TestSetup::one_player_one_card_start_of_turn().with_all_cards_known();

        for i in 1..51 {
            // We don't want to run into any buffs
            env.accepts(&ServerMessage::PublishCardPublically(
                CardId(i),
                KnownCard {
                    rank: Rank::Ace,
                    suit: Suit::Hearts,
                },
            ));

            env.accepts(&ServerMessage::TakeFreshCardFromDeck(player0(), CardId(i)));

            env.accepts(&ServerMessage::DropCardOnDiscardPile {
                actor: player0(),
                card_id: CardId(i),
                offset_x: 0.0,
                offset_y: 0.0,
                rotation: 0.0,
            });
            env.accepts(&ServerMessage::PlayerAtTurn(player0()));
        }

        env.rejects(&ServerMessage::TakeFreshCardFromDeck(player0(), CardId(52)));
    }

    #[test]
    fn cannot_use_two_discard_opportunities() {
        let mut env = TestSetup::one_player_two_cards_start_of_turn()
            .with_card_on_discard_pile(
                CardId(30),
                KnownCard {
                    rank: Rank::Ace,
                    suit: Suit::Hearts,
                },
            )
            .with_events(&[
                ServerMessage::PublishCardPublically(
                    CardId(0),
                    KnownCard {
                        rank: Rank::Ace,
                        suit: Suit::Spades,
                    },
                ),
                ServerMessage::PublishCardPublically(
                    CardId(1),
                    KnownCard {
                        rank: Rank::Ace,
                        suit: Suit::Diamonds,
                    },
                ),
            ]);

        env.accepts(&ServerMessage::PickUpSlotCard(
            player0(),
            SlotId(0),
            CardId(0),
        ));

        env.accepts(&ServerMessage::DropCardOnDiscardPile {
            actor: player0(),
            card_id: CardId(0),
            offset_x: 0.0,
            offset_y: 0.0,
            rotation: 0.0,
        });

        env.accepts(&ServerMessage::PickUpSlotCard(
            player0(),
            SlotId(1),
            CardId(1),
        ));

        env.rejects(&ServerMessage::DropCardOnDiscardPile {
            actor: player0(),
            card_id: CardId(1),
            offset_x: 0.0,
            offset_y: 0.0,
            rotation: 0.0,
        });
    }

    #[test]
    fn flow_use_swap_buff() {
        TestSetup::one_player_two_cards_start_of_turn()
            .with_all_cards_known()
            .with_events(&[
                ServerMessage::PublishCardForPlayer(
                    player0(),
                    CardId(2),
                    Some(KnownCard {
                        rank: Rank::Jack,
                        suit: Suit::Hearts,
                    }),
                ),
                ServerMessage::TakeFreshCardFromDeck(player0(), CardId(2)),
                ServerMessage::DropCardOnDiscardPile {
                    actor: player0(),
                    card_id: CardId(2),
                    offset_x: 0.0,
                    offset_y: 0.0,
                    rotation: 0.0,
                },
                ServerMessage::PickUpSlotCard(player0(), SlotId(0), CardId(0)),
                ServerMessage::SwapHeldCardWithSlotCard {
                    actor: player0(),
                    held_card_id: CardId(0),
                    slot_card_id: CardId(1),
                    slot_id: SlotId(1),
                },
                ServerMessage::DropCardOnSlot(player0(), CardId(1), SlotId(0)),
            ]);
    }

    #[test]
    fn pickup_card_after_discard() {
        let mut env = TestSetup::one_player_with_slot_cards(vec![
            (
                CardId(0),
                KnownCard {
                    rank: Rank::Three,
                    suit: Suit::Hearts,
                },
            ),
            (
                CardId(1),
                KnownCard {
                    rank: Rank::Four,
                    suit: Suit::Spades,
                },
            ),
        ])
        .with_card_on_discard_pile(
            CardId(50),
            KnownCard {
                suit: Suit::Diamonds,
                rank: Rank::Three,
            },
        );

        // We discard our 3
        env.accepts(&ServerMessage::PickUpSlotCard(
            player0(),
            SlotId(0),
            CardId(0),
        ));
        env.accepts(&ServerMessage::DropCardOnDiscardPile {
            actor: player0(),
            card_id: CardId(0),
            offset_x: 0.0,
            offset_y: 0.0,
            rotation: 0.0,
        });

        // We then pick up the 3 as part of our turn
        env.accepts(&ServerMessage::TakeCardFromDiscardPile(
            player0(),
            CardId(0),
        ));

        // and try to undo this action
        env.accepts(&ServerMessage::DropCardOnDiscardPile {
            actor: player0(),
            card_id: CardId(0),
            offset_x: 0.0,
            offset_y: 0.0,
            rotation: 0.0,
        });

        // pick it up again anyway
        env.accepts(&ServerMessage::TakeCardFromDiscardPile(
            player0(),
            CardId(0),
        ));

        // swap
        env.accepts(&ServerMessage::SwapHeldCardWithSlotCard {
            actor: player0(),
            held_card_id: CardId(0),
            slot_card_id: CardId(1),
            slot_id: SlotId(1),
        });

        // discard
        env.accepts(&ServerMessage::DropCardOnDiscardPile {
            actor: player0(),
            card_id: CardId(1),
            offset_x: 0.0,
            offset_y: 0.0,
            rotation: 0.0,
        });

        let mut world = env.get_world();

        let state = world
            .query::<&mut CambioState>()
            .single_mut(&mut world)
            .unwrap()
            .clone();
        let turn_state = world
            .get::<TurnState>(state.player_index[&player0()])
            .unwrap();

        assert_eq!(*turn_state, TurnState::Finished);
    }

    /// Tests that dropping a card on the discard pile during a swap is allowed
    /// and propely finishes the swap action. (It should be equivalent to first dropping
    /// the card on the slot and then picking it up again.)
    #[test]
    fn drop_card_on_discard_pile_during_swap() {
        let env = TestSetup::two_players_one_cards()
            .with_all_cards_known()
            .with_events(&[
                // First get the card swap buff
                ServerMessage::PublishCardPublically(
                    CardId(2),
                    KnownCard {
                        rank: Rank::Jack,
                        suit: Suit::Hearts,
                    },
                ),
                ServerMessage::TakeFreshCardFromDeck(player0(), CardId(2)),
                ServerMessage::DropCardOnDiscardPile {
                    actor: player0(),
                    card_id: CardId(2),
                    offset_x: 0.0,
                    offset_y: 0.0,
                    rotation: 0.0,
                },
                ServerMessage::PickUpSlotCard(player0(), SlotId(0), CardId(0)),
                ServerMessage::SwapHeldCardWithSlotCard {
                    actor: player0(),
                    held_card_id: CardId(0),
                    slot_card_id: CardId(1),
                    slot_id: SlotId(1),
                },
                // make sure that the swapped card is also a Jack.
                ServerMessage::PublishCardPublically(
                    CardId(1),
                    KnownCard {
                        rank: Rank::Jack,
                        suit: Suit::Diamonds,
                    },
                ),
                ServerMessage::DropCardOnDiscardPile {
                    actor: player0(),
                    card_id: CardId(1),
                    offset_x: 0.0,
                    offset_y: 0.0,
                    rotation: 0.0,
                },
            ]);

        let mut world = env.get_world();

        let state = world
            .query::<&mut CambioState>()
            .single_mut(&mut world)
            .unwrap()
            .clone();
        let turn_state = world
            .get::<TurnState>(state.player_index[&player0()])
            .unwrap();

        assert_eq!(*turn_state, TurnState::Finished);
    }
}
