use std::{
    collections::{HashSet, VecDeque},
    time::Duration,
};

use bevy::{platform::collections::HashMap, prelude::*};
use bevy_matchbox::prelude::PeerId;
use bevy_tweening::{Animator, Tween, lens::TransformPositionLens};
use serde::{Deserialize, Serialize};

use crate::{
    assets::{DESIRED_CARD_HEIGHT, DESIRED_CARD_WIDTH},
    cards::*,
    messages::ServerMessage,
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

#[derive(Component)]
#[require(Transform, InheritedVisibility)]
pub struct PlayerState {
    pub username: String,
    pub last_mouse_pos_world: Vec2,
    pub slots: HashSet<SlotId>,
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
    pub game_finished: bool,
    pub game_will_finish_in: Option<Timer>,
}

#[derive(Event)]
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
        app.add_event::<AcceptedMessage>();
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
        if timer.0.finished() {
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
                game_finished: false,
                game_will_finish_in: None,
            },
        ))
        .id();

    commands.entity(discard_pile).insert(ChildOf(root));
}

pub fn process_single_event(
    In((root, msg)): In<(Entity, ServerMessage)>,
    mut states: Query<&mut CambioState>,
    mut players: Query<(&PlayerId, &mut PlayerState)>,
    global_transforms: Query<&GlobalTransform>,
    (held_by, is_holding): (Query<(Entity, &IsHeldBy)>, Query<&IsHoldingCard>),
    my_player: Query<Entity, With<MyPlayer>>,
    (belongs_to_slot, has_card): (Query<&BelongsToSlot>, Query<&SlotHasCard>),
    (card_ids, slot_ids): (Query<&CardId>, Query<&SlotId>),
    known_cards: Query<&KnownCard>,
    children: Query<&Children>,
    matching_discard: Query<&WasMatchingDiscard>,
    may_give_card_to: Query<&MayGiveCardTo>,
    immunity: Query<&HasImmunity>,
    mut accepted: EventWriter<AcceptedMessage>,
    mut player_at_turn: Query<(Entity, &mut TurnState)>,
    mut commands: Commands,
) -> Result<ServerMessage, RejectionReason> {
    info!("Processing message: {:?}", msg);

    macro_rules! reject {
        ($($arg:tt)*) => {
            warn!($($arg)*);
            println!($($arg)*);
            return Err(RejectionReason::Other(format!($($arg)*)));
        };
    }

    let Ok(mut state) = states.get_mut(root) else {
        reject!("Provided entity is not a cambio root");
    };

    if state.game_finished {
        reject!("Game is finished");
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
        ServerMessage::PlayerConnected { player_id } => {
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
                    },
                    Name::new(format!("Player {}", player_id.player_number())),
                    Transform::from_xyz(x, y, 0.0),
                    ChildOf(root),
                ))
                .id();

            state.player_index.insert(*player_id, player_entity);
        }
        ServerMessage::SetUsername { actor, username } => {
            let player_entity = player_must_exists!(actor);
            let Ok(mut player) = players.get_mut(player_entity) else {
                reject!("Player index corrupted");
            };
            player.1.username = username.clone();
        }
        ServerMessage::PlayerDisconnected { player_id } => {
            let _ = player_must_exists!(player_id);
            state.player_index.remove(player_id);
        }
        ServerMessage::ReceiveFreshSlot { actor, slot_id } => {
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
        ServerMessage::ReceiveFreshCardFromDeck {
            actor,
            card_id,
            slot_id,
            ..
        } => {
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

            state.card_index.insert(*card_id, card_entity);
        }
        ServerMessage::ReturnHeldCardToSlot {
            actor,
            card_id,
            slot_id,
        } => {
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
                .insert(Animator::new(tween))
                .insert(ChildOf(slot_entity))
                .insert(Pickable::default());
        }
        ServerMessage::PickUpSlotCard {
            actor,
            slot_id,
            card_id,
        } => {
            let player_entity = player_must_exists!(actor);
            let card_entity = card_must_exists!(card_id);
            let slot_entity = slot_must_exists!(slot_id);
            slot_must_have_card!(slot_id, card_id);
            player_must_not_be_holding_a_card!(actor);
            card_must_not_be_held!(card_id);

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
                .insert(BelongsToSlot(slot_entity))
                .insert(Transform::from_xyz(0.0, 0.0, 10.0))
                .remove::<ChildOf>()
                .remove::<Pickable>();
        }
        ServerMessage::DropCardOnSlot {
            actor,
            card_id,
            slot_id,
        } => {
            let player_entity = player_must_exists!(actor);
            let slot_entity = slot_must_exists!(slot_id);
            let card_entity = card_must_exists!(card_id);
            player_must_be_holding_card!(actor, card_id);

            if children
                .iter_descendants(slot_entity)
                .any(|c| card_ids.contains(c))
            {
                reject!("Slot already has a card.");
            }

            if let Ok(BelongsToSlot(originating_slot_entity)) = belongs_to_slot.get(card_entity)
                && let Ok(MayGiveCardTo(player_to_screw)) = may_give_card_to.get(player_entity)
            {
                let originating_slot = slot_ids.get(*originating_slot_entity).unwrap();
                let slot_owner = slot_owner!(slot_id);
                let card_owner = slot_owner!(originating_slot);

                // Putting it back where it game from is always valid
                if originating_slot != slot_id {
                    // But if it is somewhere else then...

                    // 1. It must be on a slot of the player we are screwing
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
                            has_swapped_from_slot: Some(swap_origin),
                        })
                        | TurnState::HasBuff(TurnBuff::MayLookAtCardAndThenSwap {
                            has_swapped_from_slot: Some(swap_origin),
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
                .insert(Transform::from_xyz(0.0, 0.0, 1.0));
        }
        ServerMessage::RevealCardAtSlot {
            actor,
            card_id,
            slot_id,
            check_turn,
        } => {
            let _player_entity = player_must_exists!(actor);
            let card_entity = card_must_exists!(card_id);
            let _slot_entity = slot_must_exists!(slot_id);
            slot_must_have_card!(slot_id, card_id);
            slot_owner_must_not_be_immune!(slot_id);
            let slot_owner = slot_owner!(slot_id);

            if *check_turn {
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
                                has_swapped_from_slot: None,
                            })
                        }
                    },
                    _ => {
                        reject!("Cannot reveal a card at this point.");
                    }
                }
            }

            // Only reveal the card if we're the one who picked it up
            if actor_is_me!(actor) {
                if let Some(known_card) = state.card_lookup.0.get(card_id) {
                    commands.entity(card_entity).insert(*known_card).insert(
                        UnrevealKnownCardTimer(Timer::from_seconds(3.0, TimerMode::Once)),
                    );
                }
            }
        }
        ServerMessage::TakeFreshCardFromDeck { actor, card_id } => {
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
                            *turn_state = TurnState::HasBuff(TurnBuff::MaySwapTwoCards {
                                has_swapped_from_slot: None,
                            });
                        } else if known_value.rank == Rank::King {
                            *turn_state = TurnState::HasBuff(TurnBuff::MayLookAtCardAndThenSwap {
                                has_looked_at: None,
                                has_swapped_from_slot: None,
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
        ServerMessage::PlayerAtTurn { player_id } => {
            let player_entity = player_must_exists!(player_id);

            for (entity, _) in player_at_turn.iter() {
                commands.entity(entity).remove::<TurnState>();
            }
            commands.entity(player_entity).insert(TurnState::Start);
        }
        ServerMessage::TakeCardFromDiscardPile { actor, card_id } => {
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
                    has_swapped_from_slot,
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
                                has_swapped_from_slot: Some(*originating_slot),
                            });
                        }
                    }
                }
                TurnState::HasBuff(TurnBuff::MayLookAtCardAndThenSwap {
                    has_looked_at,
                    has_swapped_from_slot,
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
                        has_swapped_from_slot: Some(*originating_slot),
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
        ServerMessage::FinishedReplayingHistory { .. } => {}
        ServerMessage::PublishCardPublically { card_id, value } => {
            state.card_lookup.0.insert(*card_id, *value);
        }
        ServerMessage::PublishCardForPlayer { card_id, value, .. } => {
            if let Some(value) = value {
                state.card_lookup.0.insert(*card_id, *value);
            }
        }
        ServerMessage::ShuffleDiscardPile { card_ids } => {
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
        }
        ServerMessage::SlapTable { actor } => {
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
        ServerMessage::GameWillFinishIn(duration) => {
            state.game_will_finish_in = Some(Timer::new(duration.clone(), TimerMode::Once));
        }
        ServerMessage::GameFinished { all_cards, .. } => {
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
                    children.iter_descendants(slot_entity).for_each(|entity| {
                        if let Ok(card_id) = card_ids.get(entity) {
                            if let Some(known_value) = state.card_lookup.0.get(card_id) {
                                commands
                                    .entity(entity)
                                    .remove::<UnrevealKnownCardTimer>()
                                    .insert(*known_value);
                            }
                        }
                    });
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

            state.game_finished = true;
            state.game_will_finish_in = None;
        }
    }

    // If we're still here then we accepted the message.
    accepted.write(AcceptedMessage(msg.clone()));
    Ok(msg)
}

#[cfg(test)]
mod tests {
    use bevy::{asset::uuid::Uuid, ecs::event::EventRegistry};
    use strum::IntoEnumIterator;

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
            EventRegistry::register_event::<AcceptedMessage>(&mut world);

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

            self.accepts(&ServerMessage::PublishCardPublically { card_id, value });

            self
        }

        fn with_events(mut self, events: &[ServerMessage]) -> TestSetup {
            for event in events {
                assert!(
                    self.run_event_inplace(event),
                    "Unexpectedly rejected event in setup phase: {:?}",
                    event
                );
            }
            self
        }

        fn with_event(self, event: ServerMessage) -> TestSetup {
            self.with_events(&[event])
        }

        fn accepts(&mut self, event: &ServerMessage) {
            assert!(
                self.run_event_inplace(event),
                "Unexpectedly rejected event: {:?}",
                event
            );
        }

        fn rejects(&mut self, event: &ServerMessage) {
            assert!(
                !self.run_event_inplace(event),
                "Unexpectedly accepted event: {:?}",
                event
            );
        }

        fn run_event_inplace(&mut self, event: &ServerMessage) -> bool {
            self.world
                .run_system_cached_with(process_single_event, (self.root, event.clone()))
                .unwrap()
                .is_ok()
        }

        fn with_all_cards_known(mut self) -> TestSetup {
            for (card_id, card) in Rank::iter()
                .flat_map(|rank| Suit::iter().map(move |suit| KnownCard { rank, suit }))
                .enumerate()
                .map(|(index, card)| (CardId(index as u64), card))
            {
                self.run_event_inplace(&ServerMessage::PublishCardPublically {
                    card_id,
                    value: card,
                });
            }
            self
        }

        fn one_player_one_card_start_of_turn() -> TestSetup {
            TestSetup::new().with_events(&[
                ServerMessage::PlayerConnected {
                    player_id: player0(),
                },
                ServerMessage::ReceiveFreshSlot {
                    actor: player0(),
                    slot_id: SlotId(0),
                },
                ServerMessage::ReceiveFreshCardFromDeck {
                    actor: player0(),
                    slot_id: SlotId(0),
                    card_id: CardId(0),
                    is_penalty: false,
                },
                ServerMessage::PlayerAtTurn {
                    player_id: player0(),
                },
            ])
        }

        fn one_player_two_cards_start_of_turn() -> TestSetup {
            TestSetup::new().with_events(&[
                ServerMessage::PlayerConnected {
                    player_id: player0(),
                },
                ServerMessage::ReceiveFreshSlot {
                    actor: player0(),
                    slot_id: SlotId(0),
                },
                ServerMessage::ReceiveFreshCardFromDeck {
                    actor: player0(),
                    slot_id: SlotId(0),
                    card_id: CardId(0),
                    is_penalty: false,
                },
                ServerMessage::ReceiveFreshSlot {
                    actor: player0(),
                    slot_id: SlotId(1),
                },
                ServerMessage::ReceiveFreshCardFromDeck {
                    actor: player0(),
                    slot_id: SlotId(1),
                    card_id: CardId(1),
                    is_penalty: false,
                },
                ServerMessage::PlayerAtTurn {
                    player_id: player0(),
                },
            ])
        }

        fn two_players_one_cards() -> TestSetup {
            Self::one_player_one_card_start_of_turn().with_events(&[
                ServerMessage::PlayerConnected {
                    player_id: player1(),
                },
                ServerMessage::ReceiveFreshSlot {
                    actor: player1(),
                    slot_id: SlotId(1),
                },
                ServerMessage::ReceiveFreshCardFromDeck {
                    actor: player1(),
                    slot_id: SlotId(1),
                    card_id: CardId(1),
                    is_penalty: false,
                },
            ])
        }

        fn get_world(self) -> World {
            self.world
        }
    }

    #[test]
    fn test_player_cannot_connect_twice() {
        let mut env = TestSetup::new().with_event(ServerMessage::PlayerConnected {
            player_id: player0(),
        });

        env.rejects(&ServerMessage::PlayerConnected {
            player_id: player0(),
        });
    }

    #[rstest::rstest]
    fn putting_down_an_seven_or_eight_gives_buff(
        #[values(KnownCard { rank: Rank::Seven, suit: Suit::Hearts }, KnownCard { rank: Rank::Eight, suit: Suit::Hearts })]
        known_card: KnownCard,
    ) {
        let mut world = TestSetup::one_player_one_card_start_of_turn()
            .with_all_cards_known()
            .with_events(&[
                ServerMessage::PublishCardForPlayer {
                    player_id: player0(),
                    card_id: CardId(1),
                    value: Some(known_card),
                },
                ServerMessage::TakeFreshCardFromDeck {
                    actor: player0(),
                    card_id: CardId(1),
                },
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
                ServerMessage::TakeFreshCardFromDeck {
                    actor: player0(),
                    card_id: CardId(1),
                },
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
                ServerMessage::TakeCardFromDiscardPile {
                    actor: player0(),
                    card_id: CardId(5),
                },
                ServerMessage::SwapHeldCardWithSlotCard {
                    actor: player0(),
                    held_card_id: CardId(5),
                    slot_card_id: CardId(0),
                    slot_id: SlotId(0),
                },
                ServerMessage::PublishCardPublically {
                    card_id: CardId(0),
                    value: KnownCard {
                        rank: Rank::King,
                        suit: Suit::Hearts,
                    },
                },
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
            .rejects(&ServerMessage::TakeFreshCardFromDeck {
                actor: player0(),
                card_id: CardId(30),
            });
    }

    #[test]
    fn taking_cards_from_deck_will_exhaust_deck() {
        let mut env = TestSetup::one_player_one_card_start_of_turn().with_all_cards_known();

        for i in 1..51 {
            // We don't want to run into any buffs
            env.accepts(&ServerMessage::PublishCardPublically {
                card_id: CardId(i),
                value: KnownCard {
                    rank: Rank::Ace,
                    suit: Suit::Hearts,
                },
            });

            env.accepts(&ServerMessage::TakeFreshCardFromDeck {
                actor: player0(),
                card_id: CardId(i),
            });

            env.accepts(&ServerMessage::DropCardOnDiscardPile {
                actor: player0(),
                card_id: CardId(i),
                offset_x: 0.0,
                offset_y: 0.0,
                rotation: 0.0,
            });
            env.accepts(&ServerMessage::PlayerAtTurn {
                player_id: player0(),
            });
        }

        env.rejects(&ServerMessage::TakeFreshCardFromDeck {
            actor: player0(),
            card_id: CardId(52),
        });
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
                ServerMessage::PublishCardPublically {
                    card_id: CardId(0),
                    value: KnownCard {
                        rank: Rank::Ace,
                        suit: Suit::Spades,
                    },
                },
                ServerMessage::PublishCardPublically {
                    card_id: CardId(1),
                    value: KnownCard {
                        rank: Rank::Ace,
                        suit: Suit::Diamonds,
                    },
                },
            ]);

        env.accepts(&ServerMessage::PickUpSlotCard {
            actor: player0(),
            slot_id: SlotId(0),
            card_id: CardId(0),
        });

        env.accepts(&ServerMessage::DropCardOnDiscardPile {
            actor: player0(),
            card_id: CardId(0),
            offset_x: 0.0,
            offset_y: 0.0,
            rotation: 0.0,
        });

        env.accepts(&ServerMessage::PickUpSlotCard {
            actor: player0(),
            slot_id: SlotId(1),
            card_id: CardId(1),
        });

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
                ServerMessage::PublishCardForPlayer {
                    player_id: player0(),
                    card_id: CardId(2),
                    value: Some(KnownCard {
                        rank: Rank::Jack,
                        suit: Suit::Hearts,
                    }),
                },
                ServerMessage::TakeFreshCardFromDeck {
                    actor: player0(),
                    card_id: CardId(2),
                },
                ServerMessage::DropCardOnDiscardPile {
                    actor: player0(),
                    card_id: CardId(2),
                    offset_x: 0.0,
                    offset_y: 0.0,
                    rotation: 0.0,
                },
                ServerMessage::PickUpSlotCard {
                    actor: player0(),
                    slot_id: SlotId(0),
                    card_id: CardId(0),
                },
                ServerMessage::SwapHeldCardWithSlotCard {
                    actor: player0(),
                    held_card_id: CardId(0),
                    slot_card_id: CardId(1),
                    slot_id: SlotId(1),
                },
                ServerMessage::DropCardOnSlot {
                    actor: player0(),
                    card_id: CardId(1),
                    slot_id: SlotId(0),
                },
            ]);
    }
}
