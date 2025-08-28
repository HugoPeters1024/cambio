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
pub struct MyPlayer;

#[derive(Debug, PartialEq, Eq)]
pub enum TurnBuff {
    MayLookAtOwnCard,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TurnState {
    Start,
    TookDeckCard,
    HasBuff(TurnBuff),
    TookDiscardedCard,
    SwappedCard,
    Finished,
}

#[derive(Component)]
pub struct PlayerAtTurn(pub TurnState);

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
    pub card_index: HashMap<CardId, Entity>,
    pub slot_index: HashMap<SlotId, Entity>,
    pub player_index: HashMap<PlayerId, Entity>,
}

#[derive(Debug)]
pub struct IncomingMessage(pub ServerMessage);

#[derive(Debug)]
pub struct ProcessedMessage(pub ServerMessage);

#[derive(Resource, Default)]
pub struct MessageBus {
    incoming: VecDeque<IncomingMessage>,
    processed: VecDeque<ProcessedMessage>,
}

impl MessageBus {
    pub fn speculate(&mut self, msg: ServerMessage) {
        self.incoming.push_back(IncomingMessage(msg));
    }

    pub fn drain_processed(&mut self) -> std::collections::vec_deque::Drain<'_, ProcessedMessage> {
        self.processed.drain(..)
    }
}

pub struct CambioPlugin;

impl Plugin for CambioPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<MessageBus>();
        app.world_mut()
            .run_system_cached(setup_game_resource)
            .unwrap();

        app.add_systems(PreUpdate, process_messages);
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
        card_index: HashMap::new(),
        slot_index: HashMap::new(),
        player_index: HashMap::new(),
    };

    commands.insert_resource(state);
}

pub fn process_messages(world: &mut World) {
    let mut bus = world.get_resource_mut::<MessageBus>().unwrap();
    let to_process = std::mem::take(&mut bus.incoming);

    for msg in to_process {
        if let Some(processed) = world
            .run_system_cached_with(process_single_event, msg)
            .unwrap()
        {
            let mut bus = world.get_resource_mut::<MessageBus>().unwrap();
            bus.processed.push_back(processed);
        }
    }
}

pub fn process_single_event(
    In(IncomingMessage(msg)): In<IncomingMessage>,
    mut state: ResMut<CambioState>,
    mut players: Query<&mut PlayerState>,
    held: Query<(Entity, &IsHeldBy)>,
    taken_from_slot: Query<&TakenFromSlot>,
    card_ids: Query<&CardId>,
    children: Query<&Children>,
    mut discard_pile: Single<(Entity, &mut DiscardPile)>,
    mut player_at_turn: Query<(Entity, &mut PlayerAtTurn)>,
    mut commands: Commands,
) -> Option<ProcessedMessage> {
    println!("Processing message: {:?}", msg);

    macro_rules! reject {
    ($($arg:tt)*) => {
        warn!($($arg)*);
        return None;
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
            &mut player_at_turn.get_mut(player_entity).unwrap().1.0
        }};
    }

    match msg {
        ServerMessage::PlayerConnected { player_id } => {
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
            commands.entity(player_entity).despawn_related::<Children>();
            state.player_index.remove(&player_id);
        }
        ServerMessage::ReceiveFreshSlot { actor, slot_id } => {
            let player_entity = player_must_exists!(actor);

            let Ok(mut player) = players.get_mut(*player_entity) else {
                error!("Player index corrupted");
                return None;
            };

            let slot_idx = player.slots.len();
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

            player.slots.insert(slot_entity);
            state.slot_index.insert(slot_id, slot_entity);
        }
        ServerMessage::ReceiveFreshCard {
            actor: _,
            slot_id,
            card_id,
        } => {
            let &slot_entity = slot_must_exists!(slot_id);

            if children
                .iter_descendants(slot_entity)
                .any(|c| card_ids.contains(c))
            {
                reject!("Slot already has a card.");
            }

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

            if let Ok(TakenFromSlot(originating_slot)) = taken_from_slot.get(card_entity) {
                // This card was picked up for an out of turn claim, it can
                // only be put back on the slot it was taken from
                // (i.e. the player reconsidered their action).
                if *originating_slot != slot_id {
                    reject!("Cannot put card back on a different slot.");
                }
            } else {
                let turn_state = must_have_turn!(actor);

                // This card was picked up by the actor, it can only be
                // dropped on their slot if it is part of their turn flow.
                let Ok(player_state) = players.get_mut(player_entity) else {
                    reject!("Player index corrupted");
                };
                if !player_state.slots.contains(&slot_entity) {
                    reject!("Cannot put a card to another player's slot.");
                }

                match turn_state {
                    TurnState::TookDiscardedCard => {
                        reject!("Cards taken from the discard can only be swapped");
                    }
                    TurnState::TookDeckCard => {
                        reject!("Cards taken from the deck can only be swapped");
                    }
                    TurnState::Start => {
                        reject!(
                            "Player is at the start of the turn but already holding a card (but not to claim a discard)"
                        );
                    }
                    TurnState::Finished => {
                        reject!(
                            "Player is at the end of the turn but still holding a card (but not to claim a discard)"
                        );
                    }
                    TurnState::SwappedCard => {
                        reject!("You may not put a swapped card on a slot");
                    }
                    TurnState::HasBuff { .. } => {
                        reject!("Cannot be holding a card at this point");
                    }
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
            value: known_card,
        } => {
            let &player_entity = player_must_exists!(actor);
            let &card_entity = card_must_exists!(card_id);
            slot_must_have_card!(slot_id, card_id);
            let turn_state = must_have_turn!(actor);

            match turn_state {
                TurnState::HasBuff(TurnBuff::MayLookAtOwnCard) => {
                    let slot_entity = slot_must_exists!(slot_id);
                    let Ok(player_state) = players.get_mut(player_entity) else {
                        reject!("Player index corrupted");
                    };
                    if !player_state.slots.contains(slot_entity) {
                        reject!("Cannot reveal a card to another player's slot.");
                    }
                }
                _ => {
                    reject!("Cannot reveal a card at this point.");
                }
            }

            if let Some(known_card) = known_card {
                commands.entity(card_entity).insert(known_card);
            }
        }
        ServerMessage::TakeFreshCardFromDeck {
            actor,
            card_id,
            value,
        } => {
            let _player_entity = player_must_exists!(actor);
            let turn_state = must_have_turn!(actor);

            if *turn_state != TurnState::Start {
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

            *turn_state = TurnState::TookDeckCard;
            let card_entity = commands
                .spawn((
                    SomeCard,
                    card_id,
                    Name::new(format!("Card {}", card_id.0)),
                    IsHeldBy(actor),
                    Transform::from_xyz(0.0, 0.0, 10.0),
                ))
                .id();

            if let Some(value) = value {
                commands.entity(card_entity).insert(value);
            }

            state.card_index.insert(card_id, card_entity);
        }
        ServerMessage::DropCardOnDiscardPile {
            actor,
            card_id,
            value,
            offset_x,
            offset_y,
            rotation,
        } => {
            let &_player_entity = player_must_exists!(actor);
            let &card_entity = card_must_exists!(card_id);
            player_must_be_holding_card!(actor, card_id);

            if taken_from_slot.contains(card_entity) {
                // Player is attempting to claim a discard opportunity!
                //
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
                let turn_state = must_have_turn!(actor);
                match turn_state {
                    TurnState::TookDeckCard => {
                        // Depending on the deck card that was discarded, players might
                        // receive a buff they can execute first.
                        if value.rank == Rank::Seven || value.rank == Rank::Eight {
                            *turn_state = TurnState::HasBuff(TurnBuff::MayLookAtOwnCard);
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
                offset_x,
                offset_y,
                0.1 + 0.1 * discard_pile.1.cards.len() as f32,
            )
            .with_rotation(Quat::from_rotation_z(rotation));

            commands
                .entity(card_entity)
                .remove::<IsHeldBy>()
                .remove::<TakenFromSlot>()
                .insert(value)
                .insert(ChildOf(discard_pile.0))
                .insert(transform);

            discard_pile.1.cards.push_back(card_entity);
        }
        ServerMessage::PlayerAtTurn { player_id } => {
            let &player_entity = player_must_exists!(player_id);

            for (entity, _) in player_at_turn.iter() {
                commands.entity(entity).remove::<PlayerAtTurn>();
            }
            commands
                .entity(player_entity)
                .insert(PlayerAtTurn(TurnState::Start));
        }
        ServerMessage::TakeCardFromDiscardPile { actor } => {
            let &_player_entity = player_must_exists!(actor);
            let turn_state = must_have_turn!(actor);

            if held.iter().any(|held| held.1.0 == actor) {
                reject!(
                    "Player {} is already holding a card.",
                    actor.player_number()
                );
            }

            let Some(card_entity) = discard_pile.1.cards.pop_back() else {
                reject!("Discard pile is empty.");
            };

            if *turn_state != TurnState::Start {
                reject!(
                    "Can only take cards at the start of a turn, but turn state is {:?}.",
                    turn_state
                );
            }
            *turn_state = TurnState::TookDiscardedCard;

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

            let turn_state = must_have_turn!(actor);
            match turn_state {
                TurnState::TookDeckCard => {}
                TurnState::TookDiscardedCard => {}
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
            }

            if !player_state.slots.contains(&slot_entity) {
                reject!("Cannot swap a card on another player's slot.");
            }

            *turn_state = TurnState::SwappedCard;

            commands
                .entity(held_card_entity)
                .remove::<IsHeldBy>()
                .remove::<KnownCard>()
                .insert(ChildOf(slot_entity))
                .insert(Transform::from_xyz(0.0, 0.0, 1.0))
                .insert(Pickable::default());

            commands
                .entity(slot_card_entity)
                .remove::<ChildOf>()
                .insert(IsHeldBy(actor))
                .insert(Transform::from_xyz(0.0, 0.0, 10.0))
                .remove::<Pickable>();
        }
        ServerMessage::FinishedReplayingHistory => {}
    }

    // If we're still here then we accepted the message.

    // Post check: If we're at the end of the turn, move to the next player
    for (current_player_at_turn, turn_state) in player_at_turn.iter() {
        if turn_state.0 == TurnState::Finished {
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
            commands
                .entity(next_player)
                .insert(PlayerAtTurn(TurnState::Start));
        }
    }

    // If we're still here then we accepted the message.
    return Some(ProcessedMessage(msg));
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {}
}
