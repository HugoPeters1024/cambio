use std::{
    collections::{HashSet, VecDeque},
    time::Duration,
};

use bevy::{platform::collections::HashMap, prelude::*};
use bevy_renet::renet::ClientId;
use bevy_tweening::{Animator, Tween, lens::TransformScaleLens};
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
    pub client_id: ClientId,
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

/// Marker component that were laid on the discard pile because the top
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
pub struct DiscardPile {
    pub cards: VecDeque<Entity>,
}

#[derive(Component)]
#[require(Transform, InheritedVisibility)]
pub struct PlayerState {
    pub last_mouse_pos_world: Vec2,
    pub slots: HashSet<Entity>,
}

#[derive(Resource)]
pub struct CambioState {
    pub root: Entity,
    pub free_cards: VecDeque<CardId>,
    pub card_index: HashMap<CardId, Entity>,
    pub slot_index: HashMap<SlotId, Entity>,
    pub player_index: HashMap<PlayerId, Entity>,
}

#[derive(Resource, Default)]
pub struct MessageDrain {
    accepted: VecDeque<ServerMessage>,
    rejected: VecDeque<ServerMessage>,
}

#[derive(Resource, Default)]
pub struct CardValueLookup(pub HashMap<CardId, KnownCard>);

impl MessageDrain {
    pub fn drain_accepted(&mut self) -> std::collections::vec_deque::Drain<'_, ServerMessage> {
        self.accepted.drain(..)
    }

    pub fn drain_rejected(&mut self) -> std::collections::vec_deque::Drain<'_, ServerMessage> {
        self.rejected.drain(..)
    }
}

pub struct CambioPlugin;

impl Plugin for CambioPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<MessageDrain>();
        app.init_resource::<CardValueLookup>();
        app.world_mut()
            .run_system_cached(setup_game_resource)
            .unwrap();

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

fn setup_game_resource(mut commands: Commands) {
    let root = commands
        .spawn((
            Name::new("Cambio"),
            InheritedVisibility::default(),
            Transform::default(),
            CambioRoot,
        ))
        .id();

    commands.spawn((
        Name::new("Discard Pile"),
        DiscardPile {
            cards: VecDeque::new(),
        },
        Transform::from_xyz(0.0, 0.0, 0.0),
        ChildOf(root),
        Sprite::from_color(
            Color::srgb(0.0, 0.0, 0.2),
            Vec2::new(DESIRED_CARD_WIDTH, DESIRED_CARD_HEIGHT),
        ),
        Pickable::default(),
    ));

    let state = CambioState {
        root,
        free_cards: (0..52).map(CardId).collect(),
        card_index: HashMap::new(),
        slot_index: HashMap::new(),
        player_index: HashMap::new(),
    };

    commands.insert_resource(state);
}

pub fn process_single_event(
    In(msg): In<ServerMessage>,
    mut state: ResMut<CambioState>,
    mut players: Query<(&PlayerId, &mut PlayerState)>,
    held: Query<(Entity, &IsHeldBy)>,
    taken_from_slot: Query<&TakenFromSlot>,
    card_ids: Query<&CardId>,
    known_cards: Query<&KnownCard>,
    children: Query<&Children>,
    matching_discard: Query<&WasMatchingDiscard>,
    may_give_card_to: Query<&MayGiveCardTo>,
    mut card_lookup: ResMut<CardValueLookup>,
    mut discard_pile: Single<(Entity, &mut DiscardPile)>,
    mut player_at_turn: Query<(Entity, &mut PlayerAtTurn)>,
    mut commands: Commands,
    mut message_drain: ResMut<MessageDrain>,
) -> bool {
    println!("Processing message: {:?}", msg);

    macro_rules! reject {
        ($($arg:tt)*) => {
            println!($($arg)*);
            warn!($($arg)*);
            return false;
        };
    }

    macro_rules! card_id_must_be_top_of_deck {
        ($card_id: expr) => {
            let Some(deck_card) = state.free_cards.pop_front() else {
                reject!("Deck is empty.");
            };

            if deck_card != $card_id {
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
            let Some(player_entity) = state.player_index.get(&$player_id) else {
                reject!("Player {} does not exist.", $player_id.player_number());
            };
            player_entity
        }};
    }

    macro_rules! card_must_exists {
        ($card_id: expr) => {{
            let Some(card_entity) = state.card_index.get(&$card_id) else {
                reject!("Card {} does not exist.", $card_id.0);
            };
            card_entity
        }};
    }

    macro_rules! slot_must_exists {
        ($slot_id: expr) => {{
            let Some(slot_entity) = state.slot_index.get(&$slot_id) else {
                reject!("Slot {} does not exist.", $slot_id.0);
            };
            slot_entity
        }};
    }

    macro_rules! slot_must_have_card {
        ($slot_id: expr, $card_id: expr) => {{
            let &slot_entity = slot_must_exists!($slot_id);
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
            let &card_entity = card_must_exists!($card_id);
            if let Ok((_, held_by)) = held.get(card_entity) {
                if held_by.0 != $player_id {
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
            if held.iter().any(|(_, held_by)| held_by.0 == $player_id) {
                reject!(
                    "Player {} is already holding a card.",
                    $player_id.player_number()
                );
            }
        }};
    }

    macro_rules! card_must_be_known_internally {
        ($card_id: expr) => {{
            let Some(known_card) = card_lookup.0.get(&$card_id) else {
                reject!("Card {} should have known value.", $card_id.0);
            };
            known_card
        }};
    }

    macro_rules! try_get_slot_card {
        ($slot_id: expr) => {{
            let &slot_entity = slot_must_exists!($slot_id);
            let mut found = None;
            for child in children.iter_descendants(slot_entity) {
                if let Ok(card_id) = card_ids.get(child) {
                    found = Some(*card_id);
                }
            }
            found
        }};
    }

    macro_rules! must_have_turn {
        ($player_id: expr) => {{
            let &player_entity = player_must_exists!($player_id);
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
                .find(|(_, player_state)| player_state.slots.contains(slot_entity))
                .unwrap()
                .0
        }};
    }

    match msg {
        ServerMessage::PlayerConnected { player_id } => {
            if state.player_index.contains_key(&player_id) {
                reject!("Player {} is already connected.", player_id.player_number());
            }

            println!("Player {} connected.", player_id.player_number());
            let pid = player_id.player_index as usize;
            let x = -200.0 + 200.0 * (pid % 3) as f32;
            let y = -150.0 + 300.0 * (pid / 3) as f32;

            let player_entity = commands
                .spawn((
                    player_id,
                    PlayerState {
                        last_mouse_pos_world: Vec2::ZERO,
                        slots: HashSet::new(),
                    },
                    Name::new(format!("Player {}", player_id.player_number())),
                    Transform::from_xyz(x, y, 0.0),
                    ChildOf(state.root),
                ))
                .id();

            state.player_index.insert(player_id, player_entity);
        }
        ServerMessage::PlayerDisconnected { player_id } => {
            println!("Player {} disconnected.", player_id.player_number());

            let player_entity = *state.player_index.get(&player_id).unwrap();
            commands.entity(player_entity).despawn();
            state.player_index.remove(&player_id);
        }
        ServerMessage::ReceiveFreshSlot { actor, slot_id } => {
            let player_entity = player_must_exists!(actor);
            if state.slot_index.contains_key(&slot_id) {
                reject!("Slot {} already exists.", slot_id.0);
            }

            let Ok(mut player) = players.get_mut(*player_entity) else {
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
                    slot_id,
                    ChildOf(*player_entity),
                ))
                .id();

            player.1.slots.insert(slot_entity);
            state.slot_index.insert(slot_id, slot_entity);
        }
        ServerMessage::ReceiveFreshCardFromDeck {
            actor,
            card_id,
            slot_id,
        } => {
            let _ = player_must_exists!(actor);
            let &slot_entity = slot_must_exists!(slot_id);

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
                    card_id,
                    Transform::from_xyz(0.0, 0.0, 1.0),
                    ChildOf(slot_entity),
                    Pickable::default(),
                    Animator::new(Tween::new(
                        EaseFunction::CubicOut,
                        Duration::from_millis(500),
                        TransformScaleLens {
                            start: Vec3::ZERO,
                            end: Vec3::ONE,
                        },
                    )),
                ))
                .id();

            state.card_index.insert(card_id, card_entity);
        }
        ServerMessage::PickUpSlotCard {
            actor,
            slot_id,
            card_id,
        } => {
            let &_player_entity = player_must_exists!(actor);
            let &card_entity = card_must_exists!(card_id);
            slot_must_have_card!(slot_id, card_id);
            player_must_not_be_holding_a_card!(actor);

            if held.contains(card_entity) {
                reject!("Card is both attached to a slot and held?!");
            }

            commands
                .entity(card_entity)
                .insert(IsHeldBy(actor))
                .insert(TakenFromSlot(slot_id))
                .insert(Transform::from_xyz(0.0, 0.0, 10.0))
                .remove::<ChildOf>()
                .remove::<Pickable>();
        }
        ServerMessage::DropCardOnSlot {
            actor,
            card_id,
            slot_id,
        } => {
            let &player_entity = player_must_exists!(actor);
            let &slot_entity = slot_must_exists!(slot_id);
            let &card_entity = card_must_exists!(card_id);
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
                let card_owner = slot_owner!(*originating_slot);

                // Putting it back where it game from is always valid
                if *originating_slot != slot_id {
                    // But if it is somewhere else then...

                    // 1. It must be on a slot of the player we are screwing
                    if slot_owner != player_to_screw {
                        reject!(
                            "You can only give a card to player {} right now.",
                            player_to_screw.player_number()
                        );
                    }

                    // 2. The card must be ours
                    if *card_owner != actor {
                        reject!(
                            "You can only give one of your cards to player {} right now.",
                            player_to_screw.player_number()
                        );
                    }

                    // 3. Then we can complete the action
                    commands.entity(player_entity).remove::<MayGiveCardTo>();
                }
            } else if let Ok(TakenFromSlot(originating_slot)) = taken_from_slot.get(card_entity)
                && !player_at_turn.contains(player_entity)
            {
                // We are specifically doing an out of turn drop with a card that originates from
                // another player's slot. The only way to drop this card on a slot is by returning
                // it to the origin.
                if *originating_slot != slot_id {
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
                            if *swap_origin != slot_id {
                                reject!(
                                    "You may only drop put this card back on the slot where it was swapped from"
                                );
                            }
                            *turn_state = PlayerAtTurn::Finished;
                        }
                        _ => {
                            if *taken_from_slot != slot_id {
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
        } => {
            let &player_entity = player_must_exists!(actor);
            let &card_entity = card_must_exists!(card_id);
            slot_must_have_card!(slot_id, card_id);
            let mut turn_state = must_have_turn!(actor);

            match &*turn_state {
                PlayerAtTurn::HasBuff(buff) => {
                    let slot_entity = slot_must_exists!(slot_id);
                    let Ok(player_state) = players.get_mut(player_entity) else {
                        reject!("Player index corrupted");
                    };

                    match buff {
                        TurnBuff::MayLookAtOwnCard => {
                            if !player_state.1.slots.contains(slot_entity) {
                                reject!("Cannot reveal another player's card.");
                            }
                            *turn_state = PlayerAtTurn::Finished;
                        }
                        TurnBuff::MayLookAtOtherPlayersCard => {
                            if player_state.1.slots.contains(slot_entity) {
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
                                    has_looked_at: Some(slot_id),
                                    has_swapped_from_slot: None,
                                })
                        }
                    }
                }
                _ => {
                    reject!("Cannot reveal a card at this point.");
                }
            }

            if let Some(known_card) = card_lookup.0.get(&card_id) {
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

            if held.iter().any(|held| held.1.0 == actor) {
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
                    card_id,
                    Name::new(format!("Card {}", card_id.0)),
                    IsHeldBy(actor),
                    Transform::from_xyz(0.0, 0.0, 10.0),
                ))
                .id();

            // We assume that the server will JIT publish the card for the
            // right player before sending this message.
            if let Some(known_card) = card_lookup.0.get(&card_id) {
                commands.entity(card_entity).insert(*known_card);
            }

            state.card_index.insert(card_id, card_entity);
        }
        ServerMessage::DropCardOnDiscardPile {
            actor,
            card_id,
            offset_x,
            offset_y,
            rotation,
        } => {
            let &player_entity = player_must_exists!(actor);
            let &card_entity = card_must_exists!(card_id);
            player_must_be_holding_card!(actor, card_id);
            let known_value = card_must_be_known_internally!(card_id);

            if let Ok(TakenFromSlot(origin_slot_id)) = taken_from_slot.get(card_entity) {
                let Some(top_card_entity) = discard_pile.1.cards.back() else {
                    reject!("Discard pile is empty");
                };

                if matching_discard.contains(*top_card_entity) {
                    reject!("The top card was already a matching discard by another player");
                }

                let Ok(top_card) = known_cards.get(*top_card_entity) else {
                    reject!("How is the discard pile's top card not known?!");
                };

                if top_card.rank != known_value.rank {
                    reject!("Cannot discard a card of a different rank!");
                }

                let &stolen_from = slot_owner!(*origin_slot_id);
                if stolen_from != actor {
                    // This player claimed a match using another player's card!
                    // They now get to give one of their cards to this player
                    commands
                        .entity(player_entity)
                        .insert(MayGiveCardTo(stolen_from));
                }

                // Player is has claimed a discard opportunity!
                // The other players holding such a card were too late and put the card back
                for (held_card, _) in held.iter().filter(|(_, held_by)| held_by.0 != actor) {
                    if let Ok(TakenFromSlot(originating_slot)) = taken_from_slot.get(held_card) {
                        // This card is held by someone else trying to claim a dicard opportunity,
                        // but they were to late! Let's put the card back where it came from.
                        let &slot_entity = slot_must_exists!(*originating_slot);
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
                            *turn_state =
                                PlayerAtTurn::HasBuff(TurnBuff::MayLookAtOtherPlayersCard);
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
                offset_x,
                offset_y,
                0.1 + 0.1 * discard_pile.1.cards.len() as f32,
            )
            .with_rotation(Quat::from_rotation_z(rotation));

            commands
                .entity(card_entity)
                .remove::<IsHeldBy>()
                .remove::<TakenFromSlot>()
                .insert(*known_value)
                .insert(ChildOf(discard_pile.0))
                .insert(transform);

            discard_pile.1.cards.push_back(card_entity);
        }
        ServerMessage::PlayerAtTurn { player_id } => {
            let &player_entity = player_must_exists!(player_id);

            for (entity, _) in player_at_turn.iter() {
                commands.entity(entity).remove::<PlayerAtTurn>();
            }
            commands.entity(player_entity).insert(PlayerAtTurn::Start);
        }
        ServerMessage::TakeCardFromDiscardPile { actor } => {
            let &_player_entity = player_must_exists!(actor);
            let mut turn_state = must_have_turn!(actor);

            if held.iter().any(|held| held.1.0 == actor) {
                reject!(
                    "Player {} is already holding a card.",
                    actor.player_number()
                );
            }

            let Some(card_entity) = discard_pile.1.cards.pop_back() else {
                reject!("Discard pile is empty.");
            };

            if *turn_state != PlayerAtTurn::Start {
                reject!(
                    "Can only take cards at the start of a turn, but turn state is {:?}.",
                    turn_state
                );
            }
            *turn_state = PlayerAtTurn::TookDiscardedCard;

            commands
                .entity(card_entity)
                .insert(IsHeldBy(actor))
                .insert(Transform::from_xyz(0.0, 0.0, 10.0))
                .remove::<ChildOf>();
        }
        ServerMessage::SwapHeldCardWithSlotCard {
            actor,
            slot_id,
            held_card_id,
        } => {
            let &player_entity = player_must_exists!(actor);
            let &slot_entity = slot_must_exists!(slot_id);
            let &held_card_entity = card_must_exists!(held_card_id);
            player_must_be_holding_card!(actor, held_card_id);

            let Some(slot_card_id) = try_get_slot_card!(slot_id) else {
                reject!("Slot is empty.");
            };

            let &slot_card_entity = card_must_exists!(slot_card_id);

            let Ok(player_state) = players.get_mut(player_entity) else {
                reject!("Player index corrupted");
            };

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

                    if has_looked_at_slot != *originating_slot && has_looked_at_slot != slot_id {
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
                .insert(IsHeldBy(actor))
                .insert(Transform::from_xyz(0.0, 0.0, 10.0))
                .remove::<Pickable>();

            // if the source card has a TakenFromSlot marker, we give the card
            // that will now be held also one, otherwise we do not as this interfers
            // with the turn flow.
            if taken_from_slot.contains(held_card_entity) {
                commands
                    .entity(slot_card_entity)
                    .insert(TakenFromSlot(slot_id));
            }
        }
        ServerMessage::FinishedReplayingHistory => {}
        ServerMessage::PublishCardPublically { card_id, value } => {
            card_lookup.0.insert(card_id, value);
        }
        ServerMessage::PublishCardForPlayer { card_id, value, .. } => {
            if let Some(value) = value {
                card_lookup.0.insert(card_id, value);
            }
        }
    }

    // If we're still here then we accepted the message.

    // Post check: If we're at the end of the turn, move to the next player
    for (current_player_at_turn, turn_state) in player_at_turn.iter() {
        if *turn_state == PlayerAtTurn::Finished {
            println!("Moving to next player");
            let mut all_players = state.player_index.iter().collect::<Vec<_>>();
            all_players.sort_by_key(|(p, _)| p.player_number());

            let current_player_idx = all_players
                .iter()
                .position(|(_, pe)| **pe == current_player_at_turn)
                .unwrap();

            let next_player_idx = (current_player_idx + 1) % all_players.len();

            let &next_player = all_players[next_player_idx].1;

            commands
                .entity(current_player_at_turn)
                .remove::<PlayerAtTurn>();
            commands.entity(next_player).insert(PlayerAtTurn::Start);
        }
    }

    // If we're still here then we accepted the message.
    message_drain.accepted.push_back(msg);
    true
}

#[cfg(test)]
mod tests {
    use strum::IntoEnumIterator;

    use super::*;

    const fn player0() -> PlayerId {
        PlayerId {
            player_index: 0,
            client_id: 0,
        }
    }

    const fn player1() -> PlayerId {
        PlayerId {
            player_index: 1,
            client_id: 1,
        }
    }

    #[derive(Debug)]
    struct TestSetup {
        world: World,
    }

    impl TestSetup {
        fn new() -> TestSetup {
            let mut world = World::new();

            world.init_resource::<CardValueLookup>();
            world.init_resource::<MessageDrain>();
            world.run_system_cached(setup_game_resource).unwrap();

            TestSetup { world }
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

            self.world
                .query::<(Entity, &mut DiscardPile)>()
                .single_mut(&mut self.world)
                .unwrap()
                .1
                .cards
                .push_back(card_entity);
            self.world
                .get_resource_mut::<CambioState>()
                .unwrap()
                .card_index
                .insert(card_id, card_entity);

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
                .run_system_cached_with(process_single_event, event.clone())
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
        let world = TestSetup::one_player_one_card_start_of_turn()
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

        let state = world.get_resource::<CambioState>().unwrap();
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
        let world = TestSetup::one_player_one_card_start_of_turn()
            .with_all_cards_known()
            .with_events(&[
                ServerMessage::TakeFreshCardFromDeck {
                    actor: player0(),
                    card_id: CardId(1),
                },
                ServerMessage::SwapHeldCardWithSlotCard {
                    actor: player0(),
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

        let state = world.get_resource::<CambioState>().unwrap();
        let turn_state = world
            .get::<PlayerAtTurn>(state.player_index[&player0()])
            .unwrap();

        assert_eq!(*turn_state, PlayerAtTurn::Start);
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
                ServerMessage::TakeCardFromDiscardPile { actor: player0() },
                ServerMessage::SwapHeldCardWithSlotCard {
                    actor: player0(),
                    held_card_id: CardId(5),
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
        }

        env.rejects(&ServerMessage::TakeFreshCardFromDeck {
            actor: player0(),
            card_id: CardId(52),
        });
    }
}
