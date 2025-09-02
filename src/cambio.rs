use std::collections::{HashSet, VecDeque};

use bevy::{platform::collections::HashMap, prelude::*};
use bevy_matchbox::prelude::PeerId;
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
pub struct UnrevealKnownCardTimer(Timer);

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
#[require(Transform, InheritedVisibility)]
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

pub struct CambioPlugin;

impl Plugin for CambioPlugin {
    fn build(&self, app: &mut App) {
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
                message_drain: MessageDrain::default(),
                card_lookup: CardValueLookup::default(),
                discard_pile_entity: discard_pile,
                game_finished: false,
            },
        ))
        .id();

    commands.entity(discard_pile).insert(ChildOf(root));
}

pub fn process_single_event(
    In((root, msg)): In<(Entity, ServerMessage)>,
    mut states: Query<&mut CambioState>,
    mut players: Query<(&PlayerId, &mut PlayerState)>,
    held: Query<(Entity, &IsHeldBy)>,
    taken_from_slot: Query<&TakenFromSlot>,
    card_ids: Query<&CardId>,
    known_cards: Query<&KnownCard>,
    children: Query<&Children>,
    matching_discard: Query<&WasMatchingDiscard>,
    may_give_card_to: Query<&MayGiveCardTo>,
    immunity: Query<&HasImmunity>,
    mut player_at_turn: Query<(Entity, &mut PlayerAtTurn)>,
    mut commands: Commands,
) -> bool {
    println!("Processing message: {:?}", msg);

    macro_rules! reject {
        ($($arg:tt)*) => {
            warn!($($arg)*);
            return false;
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
            if !children
                .iter_descendants(slot_entity)
                .any(|c| card_ids.get(c) == Ok(&$card_id))
            {
                reject!("Slot {} does not have card {}.", $slot_id.0, $card_id.0);
            }
        }};
    }

    macro_rules! player_must_be_holding_card {
        ($player_id: expr, $card_id: expr) => {{
            let card_entity = card_must_exists!($card_id);
            if let Ok((_, held_by)) = held.get(card_entity) {
                if held_by.0 != *$player_id {
                    reject!(
                        "Player {} is not holding card {}, Player {} is.",
                        $player_id.player_number(),
                        $card_id.0,
                        held_by.0.player_number()
                    );
                }
            } else {
                reject!("Card is not being held at all",);
            }
        }};
    }

    macro_rules! player_must_not_be_holding_a_card {
        ($player_id: expr) => {{
            if held.iter().any(|(_, held_by)| held_by.0 == *$player_id) {
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
            let slot_entity = slot_must_exists!($slot_id);
            players
                .iter()
                .find(|(_, player_state)| player_state.slots.contains(&slot_entity))
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

    match &msg {
        ServerMessage::PlayerConnected { player_id } => {
            if state.player_index.contains_key(player_id) {
                reject!("Player {} is already connected.", player_id.player_number());
            }

            let pid = player_id.player_index as usize;
            let x = -200.0 + 200.0 * (pid % 3) as f32;
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
            if slot_idx >= 4 {
                reject!(
                    "Player {} already has the maximum number of slots",
                    actor.player_number()
                );
            }

            let x = (DESIRED_CARD_WIDTH + 10.0) * (slot_idx % 2) as f32
                - (DESIRED_CARD_WIDTH + 10.0) / 2.0;
            let y = (DESIRED_CARD_HEIGHT + 10.0) * (slot_idx / 2) as f32
                - (DESIRED_CARD_HEIGHT + 10.0) / 2.0;

            let slot_entity = commands
                .spawn((
                    Name::new(format!("Slot {}", slot_idx)),
                    Transform::from_xyz(x, y, 0.0),
                    CardSlot,
                    *slot_id,
                    ChildOf(player_entity),
                ))
                .id();

            player.1.slots.insert(slot_entity);
            state.slot_index.insert(*slot_id, slot_entity);
        }
        ServerMessage::ReceiveFreshCardFromDeck {
            actor,
            card_id,
            slot_id,
        } => {
            let _ = player_must_exists!(actor);
            let slot_entity = slot_must_exists!(slot_id);

            if children
                .iter_descendants(slot_entity)
                .any(|c| card_ids.contains(c))
            {
                reject!("Slot already has a card.");
            }

            card_id_must_be_top_of_deck!(card_id);
            let card_entity = commands
                .spawn((
                    SomeCard,
                    Name::new(format!("Card {}", card_id.0)),
                    *card_id,
                    Transform::from_xyz(0.0, 0.0, 1.0),
                    ChildOf(slot_entity),
                    Pickable::default(),
                ))
                .id();

            state.card_index.insert(*card_id, card_entity);
        }
        ServerMessage::PickUpSlotCard {
            actor,
            slot_id,
            card_id,
        } => {
            let _player_entity = player_must_exists!(actor);
            let card_entity = card_must_exists!(card_id);
            slot_must_have_card!(slot_id, card_id);
            player_must_not_be_holding_a_card!(actor);

            let slot_owner = slot_owner!(slot_id);
            if slot_owner != actor {
                slot_owner_must_not_be_immune!(slot_id);
            }

            if held.contains(card_entity) {
                reject!("Card is both attached to a slot and held?!");
            }

            commands
                .entity(card_entity)
                .insert(IsHeldBy(*actor))
                .insert(TakenFromSlot(*slot_id))
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

            if let Ok(TakenFromSlot(originating_slot)) = taken_from_slot.get(card_entity)
                && let Ok(MayGiveCardTo(player_to_screw)) = may_give_card_to.get(player_entity)
            {
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
            } else if let Ok(TakenFromSlot(originating_slot)) = taken_from_slot.get(card_entity)
                && !player_at_turn.contains(player_entity)
            {
                // We are specifically doing an out of turn drop with a card that originates from
                // another player's slot. The only way to drop this card on a slot is by returning
                // it to the origin.
                if originating_slot != slot_id {
                    reject!("Cannot put card back on a different slot.");
                }
            } else {
                let mut turn_state = must_have_turn!(actor);

                if let Ok(TakenFromSlot(taken_from_slot)) = taken_from_slot.get(card_entity) {
                    match &*turn_state {
                        PlayerAtTurn::HasBuff(TurnBuff::MaySwapTwoCards {
                            has_swapped_from_slot: Some(swap_origin),
                        })
                        | PlayerAtTurn::HasBuff(TurnBuff::MayLookAtCardAndThenSwap {
                            has_swapped_from_slot: Some(swap_origin),
                            ..
                        }) => {
                            if swap_origin != slot_id {
                                reject!(
                                    "You may only drop put this card back on the slot where it was swapped from"
                                );
                            }
                            *turn_state = PlayerAtTurn::Finished;
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
                .remove::<TakenFromSlot>()
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
            let player_entity = player_must_exists!(actor);
            let card_entity = card_must_exists!(card_id);
            slot_must_have_card!(slot_id, card_id);
            slot_owner_must_not_be_immune!(slot_id);

            if *check_turn {
                let mut turn_state = must_have_turn!(actor);

                match &*turn_state {
                    PlayerAtTurn::HasBuff(buff) => {
                        let slot_entity = slot_must_exists!(slot_id);
                        let Ok(player_state) = players.get_mut(player_entity) else {
                            reject!("Player index corrupted");
                        };

                        match buff {
                            TurnBuff::MayLookAtOwnCard => {
                                if !player_state.1.slots.contains(&slot_entity) {
                                    reject!("Cannot reveal another player's card.");
                                }
                                *turn_state = PlayerAtTurn::Finished;
                            }
                            TurnBuff::MayLookAtOtherPlayersCard => {
                                if player_state.1.slots.contains(&slot_entity) {
                                    reject!("Cannot reveal your own card.");
                                }
                                *turn_state = PlayerAtTurn::Finished;
                            }
                            TurnBuff::MaySwapTwoCards { .. } => {
                                reject!("Cannot reveal a card at this point.");
                            }
                            TurnBuff::MayLookAtCardAndThenSwap { has_looked_at, .. } => {
                                if has_looked_at.is_some() {
                                    reject!("You already looked at a card.");
                                }
                                *turn_state =
                                    PlayerAtTurn::HasBuff(TurnBuff::MayLookAtCardAndThenSwap {
                                        has_looked_at: Some(*slot_id),
                                        has_swapped_from_slot: None,
                                    })
                            }
                        }
                    }
                    _ => {
                        reject!("Cannot reveal a card at this point.");
                    }
                }
            }

            if let Some(known_card) = state.card_lookup.0.get(card_id) {
                commands
                    .entity(card_entity)
                    .insert(*known_card)
                    .insert(UnrevealKnownCardTimer(Timer::from_seconds(
                        3.0,
                        TimerMode::Once,
                    )));
            }
        }
        ServerMessage::TakeFreshCardFromDeck { actor, card_id } => {
            let _player_entity = player_must_exists!(actor);
            let mut turn_state = must_have_turn!(actor);

            if *turn_state != PlayerAtTurn::Start {
                reject!(
                    "Can only take cards at the start of a turn, but turn state is {:?}.",
                    turn_state
                );
            }

            if held.iter().any(|held| held.1.0 == *actor) {
                reject!(
                    "Player {} is already holding a card.",
                    actor.player_number()
                );
            }

            *turn_state = PlayerAtTurn::TookDeckCard;

            card_id_must_be_top_of_deck!(card_id);
            let card_entity = commands
                .spawn((
                    SomeCard,
                    *card_id,
                    Name::new(format!("Card {}", card_id.0)),
                    IsHeldBy(*actor),
                    Transform::from_xyz(0.0, 0.0, 10.0),
                ))
                .id();

            // We assume that the server will JIT publish the card for the
            // right player before sending this message.
            if let Some(known_card) = state.card_lookup.0.get(card_id) {
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

            if let Ok(TakenFromSlot(origin_slot_id)) = taken_from_slot.get(card_entity) {
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
                    reject!("Cannot discard a card of a different rank!");
                }

                let stolen_from = slot_owner!(origin_slot_id);
                if stolen_from != actor {
                    // This player claimed a match using another player's card!
                    // They now get to give one of their cards to this player
                    commands
                        .entity(player_entity)
                        .insert(MayGiveCardTo(*stolen_from));
                }

                // Player is has claimed a discard opportunity!
                // The other players holding such a card were too late and we put the card back
                for (held_card, _) in held.iter().filter(|(_, held_by)| held_by.0 != *actor) {
                    if let Ok(TakenFromSlot(originating_slot)) = taken_from_slot.get(held_card) {
                        // This card is held by someone else trying to claim a dicard opportunity,
                        // but they were to late! Let's put the card back where it came from.
                        let slot_entity = slot_must_exists!(originating_slot);
                        commands
                            .entity(held_card)
                            .remove::<IsHeldBy>()
                            .remove::<KnownCard>()
                            .remove::<TakenFromSlot>()
                            .insert(ChildOf(slot_entity))
                            .insert(Pickable::default())
                            .insert(Transform::from_xyz(0.0, 0.0, 1.0));
                    }
                }
            } else {
                let mut turn_state = must_have_turn!(actor);
                match *turn_state {
                    PlayerAtTurn::TookDeckCard => {
                        // Depending on the deck card that was discarded, players might
                        // receive a buff they can execute first.
                        if known_value.rank == Rank::Seven || known_value.rank == Rank::Eight {
                            *turn_state = PlayerAtTurn::HasBuff(TurnBuff::MayLookAtOwnCard);
                        } else if known_value.rank == Rank::Nine || known_value.rank == Rank::Ten {
                            // Only if there is another elligble player to look at
                            if state
                                .player_index
                                .iter()
                                .filter(|(pid, pe)| *pid != actor && !immunity.contains(**pe))
                                .count()
                                == 0
                            {
                                *turn_state = PlayerAtTurn::Finished;
                            } else {
                                *turn_state =
                                    PlayerAtTurn::HasBuff(TurnBuff::MayLookAtOtherPlayersCard);
                            }
                        } else if known_value.rank == Rank::Jack || known_value.rank == Rank::Queen
                        {
                            *turn_state = PlayerAtTurn::HasBuff(TurnBuff::MaySwapTwoCards {
                                has_swapped_from_slot: None,
                            });
                        } else if known_value.rank == Rank::King {
                            *turn_state =
                                PlayerAtTurn::HasBuff(TurnBuff::MayLookAtCardAndThenSwap {
                                    has_looked_at: None,
                                    has_swapped_from_slot: None,
                                });
                        } else {
                            *turn_state = PlayerAtTurn::Finished;
                        }
                    }
                    PlayerAtTurn::SwappedCard => {
                        *turn_state = PlayerAtTurn::Finished;
                    }
                    PlayerAtTurn::TookDiscardedCard => {
                        // Fine, the player is allowed to reconsider
                        *turn_state = PlayerAtTurn::Start;
                    }
                    PlayerAtTurn::Start | PlayerAtTurn::HasBuff { .. } | PlayerAtTurn::Finished => {
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
                .remove::<TakenFromSlot>()
                .remove::<UnrevealKnownCardTimer>()
                .insert(*known_value)
                .insert(ChildOf(state.discard_pile_entity))
                .insert(transform);

            state.discard_pile.push_front(*card_id);
        }
        ServerMessage::PlayerAtTurn { player_id } => {
            let player_entity = player_must_exists!(player_id);

            for (entity, _) in player_at_turn.iter() {
                commands.entity(entity).remove::<PlayerAtTurn>();
            }
            commands.entity(player_entity).insert(PlayerAtTurn::Start);
        }
        ServerMessage::TakeCardFromDiscardPile { actor, card_id } => {
            let _player_entity = player_must_exists!(actor);
            let card_entity = card_must_exists!(card_id);
            let mut turn_state = must_have_turn!(actor);

            if held.iter().any(|held| held.1.0 == *actor) {
                reject!(
                    "Player {} is already holding a card.",
                    actor.player_number()
                );
            }

            if state.discard_pile.len() == 0 {
                reject!("Discard pile is empty.");
            }

            if state.discard_pile[0] != *card_id {
                reject!("Provided card is not on top of the discard pile");
            }

            if *turn_state != PlayerAtTurn::Start {
                reject!(
                    "Can only take cards at the start of a turn, but turn state is {:?}.",
                    turn_state
                );
            }
            *turn_state = PlayerAtTurn::TookDiscardedCard;

            state.discard_pile.pop_front();
            commands
                .entity(card_entity)
                .insert(IsHeldBy(*actor))
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
            player_must_be_holding_card!(actor, held_card_id);
            slot_must_have_card!(slot_id, slot_card_id);
            slot_owner_must_not_be_immune!(slot_id);

            let slot_card_entity = card_must_exists!(slot_card_id);

            let Ok(player_state) = players.get_mut(player_entity) else {
                reject!("Player index corrupted");
            };

            if may_give_card_to.contains(player_entity) {
                reject!("Giving a card to another players is a buff that takes priority.");
            }

            let mut turn_state = must_have_turn!(actor);
            match *turn_state {
                PlayerAtTurn::TookDeckCard | PlayerAtTurn::TookDiscardedCard => {
                    if !player_state.1.slots.contains(&slot_entity) {
                        reject!("Cannot swap a card on another player's slot.");
                    }
                    *turn_state = PlayerAtTurn::SwappedCard;
                }
                PlayerAtTurn::Start => {
                    reject!("Cannot swap a card at the start of a turn.");
                }
                PlayerAtTurn::Finished => {
                    reject!("Cannot swap a card at the end of a turn.");
                }
                PlayerAtTurn::SwappedCard => {
                    reject!("Cannot swap a card twice.");
                }
                PlayerAtTurn::HasBuff(TurnBuff::MayLookAtOwnCard) => {
                    reject!("Cannot swap a card at this point");
                }
                PlayerAtTurn::HasBuff(TurnBuff::MayLookAtOtherPlayersCard) => {
                    reject!("Cannot swap a card at this point");
                }
                PlayerAtTurn::HasBuff(TurnBuff::MaySwapTwoCards {
                    has_swapped_from_slot,
                }) => {
                    let Ok(TakenFromSlot(originating_slot)) = taken_from_slot.get(held_card_entity)
                    else {
                        reject!("Can only swap cards taken from a player at this point");
                    };

                    match has_swapped_from_slot {
                        Some(_) => {
                            reject!("Already swapped two cards");
                        }
                        None => {
                            *turn_state = PlayerAtTurn::HasBuff(TurnBuff::MaySwapTwoCards {
                                has_swapped_from_slot: Some(*originating_slot),
                            });
                        }
                    }
                }
                PlayerAtTurn::HasBuff(TurnBuff::MayLookAtCardAndThenSwap {
                    has_looked_at,
                    has_swapped_from_slot,
                }) => {
                    let Some(has_looked_at_slot) = has_looked_at else {
                        reject!("You first need to look at a card");
                    };

                    if has_swapped_from_slot.is_some() {
                        reject!("Already swapped two cards");
                    }

                    let Ok(TakenFromSlot(originating_slot)) = taken_from_slot.get(held_card_entity)
                    else {
                        reject!("Can only swap cards taken from a player at this point");
                    };

                    if has_looked_at_slot != *originating_slot && has_looked_at_slot != *slot_id {
                        reject!("The swap has to include the card you looked at");
                    };

                    *turn_state = PlayerAtTurn::HasBuff(TurnBuff::MayLookAtCardAndThenSwap {
                        has_looked_at: Some(has_looked_at_slot),
                        has_swapped_from_slot: Some(*originating_slot),
                    });
                }
            }

            commands
                .entity(held_card_entity)
                .remove::<IsHeldBy>()
                .remove::<KnownCard>()
                .remove::<TakenFromSlot>()
                .insert(ChildOf(slot_entity))
                .insert(Transform::from_xyz(0.0, 0.0, 1.0))
                .insert(Pickable::default());

            commands
                .entity(slot_card_entity)
                .remove::<ChildOf>()
                .insert(IsHeldBy(*actor))
                .insert(Transform::from_xyz(0.0, 0.0, 10.0))
                .remove::<Pickable>();

            // if the source card has a TakenFromSlot marker, we give the card
            // that will now be held also one, otherwise we do not as this interfers
            // with the turn flow.
            if taken_from_slot.contains(held_card_entity) {
                commands
                    .entity(slot_card_entity)
                    .insert(TakenFromSlot(*slot_id));
            }
        }
        ServerMessage::FinishedReplayingHistory => {}
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

            if *turn_state != PlayerAtTurn::Start {
                reject!("Can only slap the table at the start of a turn");
            }

            *turn_state = PlayerAtTurn::Finished;
            commands.entity(player_entity).insert(HasImmunity);
        }
        ServerMessage::GameFinished { all_cards, .. } => {
            for (card_id, known_card) in all_cards.iter() {
                state.card_lookup.0.insert(*card_id, *known_card);
            }

            // The game is finished! Anyone holding a card puts it back
            for (held_card, _) in held.iter() {
                if let Ok(TakenFromSlot(originating_slot)) = taken_from_slot.get(held_card) {
                    // This card is held by someone else trying to claim a dicard opportunity,
                    // but they were to late! Let's put the card back where it came from.
                    let slot_entity = slot_must_exists!(originating_slot);
                    let held_card_id = card_ids.get(held_card).expect("Card index corrupted");

                    commands
                        .entity(held_card)
                        .remove::<IsHeldBy>()
                        .remove::<KnownCard>()
                        .remove::<UnrevealKnownCardTimer>()
                        .remove::<TakenFromSlot>()
                        .insert(ChildOf(slot_entity))
                        .insert(Pickable::default())
                        .insert(Transform::from_xyz(0.0, 0.0, 1.0));

                    if let Some(known_value) = state.card_lookup.0.get(held_card_id) {
                        commands.entity(held_card).insert(*known_value);
                    }
                }
            }

            for (_, player_state) in players.iter() {
                for slot_entity in player_state.slots.iter() {
                    children.iter_descendants(*slot_entity).for_each(|entity| {
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
                commands.entity(entity).remove::<PlayerAtTurn>();
            }

            for (card_id, known_card) in all_cards.iter() {
                if let Some(card_entity) = state.card_index.get(card_id) {
                    commands.entity(*card_entity).insert(*known_card);
                }
            }

            state.game_finished = true;
        }
    }

    // If we're still here then we accepted the message.
    state.message_drain.accepted.push_back(msg);
    true
}

#[cfg(test)]
mod tests {
    use bevy::asset::uuid::Uuid;
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
                .spawn((SomeCard, card_id, ChildOf(discard_pile_entity)))
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
                assert!(self.run_event_inplace(event));
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
            .get::<PlayerAtTurn>(state.player_index[&player0()])
            .unwrap();
        assert_eq!(
            *turn_state,
            PlayerAtTurn::HasBuff(TurnBuff::MayLookAtOwnCard)
        );
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
            .get::<PlayerAtTurn>(state.player_index[&player0()])
            .unwrap();

        assert_eq!(*turn_state, PlayerAtTurn::Finished);
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
}
