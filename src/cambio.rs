use std::collections::{HashSet, VecDeque};

use bevy::{platform::collections::HashMap, prelude::*};
use bevy_rand::{global::GlobalEntropy, prelude::WyRand};
use bevy_renet::renet::ClientId;
use rand::Rng;
use serde::{Deserialize, Serialize};
use std::ops::DerefMut;

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
pub struct MyPlayer;

#[derive(Debug, PartialEq, Eq)]
enum TurnState {
    Start,
    TookDeckCard,
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
    pub incoming: VecDeque<IncomingMessage>,
    pub processed: VecDeque<ProcessedMessage>,
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
    global_transforms: Query<&GlobalTransform>,
    held: Query<(Entity, &IsHeldBy)>,
    card_ids: Query<&CardId>,
    children: Query<&Children>,
    mut discard_pile: Single<(Entity, &mut DiscardPile)>,
    mut player_at_turn: Query<(Entity, &mut PlayerAtTurn)>,
    mut commands: Commands,
) -> Option<ProcessedMessage> {
    println!("Processing message: {:?}", msg);

    macro_rules! player_exists {
        ($player_id: expr) => {{
            let Some(player_entity) = state.player_index.get(&$player_id) else {
                warn!("Player {} does not exist.", $player_id.player_number());
                return None;
            };
            player_entity
        }};
    }

    macro_rules! card_exists {
        ($card_id: expr) => {{
            let Some(card_entity) = state.card_index.get(&$card_id) else {
                warn!("Card {} does not exist.", $card_id.0);
                return None;
            };
            card_entity
        }};
    }

    macro_rules! slot_exists {
        ($slot_id: expr) => {{
            let Some(slot_entity) = state.slot_index.get(&$slot_id) else {
                warn!("Slot {} does not exist.", $slot_id.0);
                return None;
            };
            slot_entity
        }};
    }

    macro_rules! try_get_slot_card {
        ($slot_id: expr) => {{
            let &slot_entity = slot_exists!($slot_id);
            let mut found = None;
            for child in children.iter_descendants(slot_entity) {
                if let Ok(card_id) = card_ids.get(child) {
                    found = Some(*card_id);
                }
            }
            found
        }};
    }

    macro_rules! try_get_held_card {
        ($player_id: expr) => {
            if let Some((held_card, _)) = held.iter().find(|(_, held_by)| held_by.0 == $player_id) {
                Some(held_card)
            } else {
                None
            }
        };
    }

    macro_rules! has_turn {
        ($player_id: expr) => {{
            let &player_entity = player_exists!($player_id);
            let Ok((_, _)) = player_at_turn.get(player_entity) else {
                warn!(
                    "Player {} is acting out of turn.",
                    $player_id.player_number()
                );
                return None;
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
            let player_entity = player_exists!(actor);

            let Ok(mut player) = players.get_mut(*player_entity) else {
                error!("Player index corrupted");
                return None;
            };

            let slot_idx = player.slots.len();
            if slot_idx >= 4 {
                warn!(
                    "Player {} already has the maximum number of slots",
                    actor.player_number()
                );
                return None;
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
            let &slot_entity = slot_exists!(slot_id);

            if children
                .iter_descendants(slot_entity)
                .any(|c| card_ids.contains(c))
            {
                warn!("Slot already has a card.");
                return None;
            }

            let card_entity = commands
                .spawn((
                    SomeCard,
                    Name::new(format!("Card {}", card_id.0)),
                    card_id,
                    Transform::from_xyz(0.0, 0.0, 1.0),
                    ChildOf(slot_entity),
                    Pickable::default(),
                ))
                .id();

            state.card_index.insert(card_id, card_entity);
        }
        ServerMessage::PickUpSlotCard { actor, slot_id } => {
            if held.iter().any(|held| held.1.0 == actor) {
                warn!(
                    "Player {} is already holding a card.",
                    actor.player_number()
                );
                return None;
            }

            let Some(card_id) = try_get_slot_card!(slot_id) else {
                warn!("Slot is empty.");
                return None;
            };

            let card_entity = card_exists!(card_id);

            if held.contains(*card_entity) {
                warn!("Card already held.");
                return None;
            }

            let gt = global_transforms.get(*card_entity).unwrap();

            let card_entity = *state.card_index.get(&card_id).unwrap();
            commands
                .entity(card_entity)
                .insert(IsHeldBy(actor))
                .insert(Transform::from_xyz(0.0, 0.0, 10.0).with_rotation(gt.rotation()))
                .remove::<ChildOf>()
                .remove::<Pickable>();
        }
        ServerMessage::DropCardOnSlot {
            actor,
            card_id,
            slot_id,
        } => {
            let player_entity = player_exists!(actor);
            let &slot_entity = slot_exists!(slot_id);
            let &card_entity = card_exists!(card_id);

            let Ok((_, held_by)) = held.get(card_entity) else {
                warn!("Card not held by anyone.");
                return None;
            };

            if held_by.0 != actor {
                warn!(
                    "Player {} is not holding the card, Player {} is.",
                    actor.player_number(),
                    held_by.0.player_number()
                );
                return None;
            }

            let Ok(player_state) = players.get_mut(*player_entity) else {
                error!("Player index corrupted");
                return None;
            };

            if !player_state.slots.contains(&slot_entity) {
                warn!("Cannot drop a card on another player's slot.");
                return None;
            }

            if children
                .iter_descendants(slot_entity)
                .any(|c| card_ids.contains(c))
            {
                warn!("Slot already has a card.");
                return None;
            }

            commands
                .entity(card_entity)
                .remove::<IsHeldBy>()
                .remove::<KnownCard>()
                .insert(ChildOf(slot_entity))
                .insert(Pickable::default())
                .insert(Transform::from_xyz(0.0, 0.0, 1.0));
        }
        ServerMessage::RevealCard {
            actor,
            card_id,
            value: known_card,
        } => {
            player_exists!(actor);
            let &card_entity = card_exists!(card_id);

            let Ok((_, held_by)) = held.get(card_entity) else {
                warn!("Card not held by anyone.");
                return None;
            };

            if held_by.0 != actor {
                warn!(
                    "Player {} is not holding the card, Player {} is.",
                    actor.player_number(),
                    held_by.0.player_number()
                );
                return None;
            }

            if let Some(known_card) = known_card {
                commands.entity(card_entity).insert(known_card);
            }
        }
        ServerMessage::TakeFreshCardFromDeck { actor, card_id } => {
            let player_entity = player_exists!(actor);
            let turn_state = has_turn!(actor);

            if *turn_state != TurnState::Start {
                warn!(
                    "Can only take cards at the start of a turn, but turn state is {:?}.",
                    turn_state
                );
                return None;
            }

            if held.iter().any(|held| held.1.0 == actor) {
                warn!(
                    "Player {} is already holding a card.",
                    actor.player_number()
                );
                return None;
            }

            *turn_state = TurnState::TookDeckCard;
            let card_entity = commands.spawn((
                SomeCard,
                card_id,
                Name::new(format!("Card {}", card_id.0)),
                IsHeldBy(actor),
                Transform::from_xyz(0.0, 0.0, 10.0),
            ));

            state.card_index.insert(card_id, card_entity.id());
        }
        ServerMessage::DropHeldCardOnDiscardPile {
            actor,
            card_id,
            value,
            local_transform,
        } => {
            let &player_entity = player_exists!(actor);
            let &card_entity = card_exists!(card_id);

            let Ok((_, held_by)) = held.get(card_entity) else {
                warn!("Card not held by anyone.");
                return None;
            };

            if held_by.0 != actor {
                warn!(
                    "Player {} is not holding the card, Player {} is.",
                    actor.player_number(),
                    held_by.0.player_number()
                );
                return None;
            }

            if let Ok((_, mut turn_state)) = player_at_turn.get_mut(player_entity) {
                match turn_state.0 {
                    TurnState::TookDeckCard => {
                        turn_state.0 = TurnState::Finished;
                    }
                    TurnState::TookDiscardedCard => {
                        warn!("Cannot discard a card that was just taken from the discard pile.");
                        return None;
                    }
                    TurnState::Start => {}
                    TurnState::Finished => {}
                    TurnState::SwappedCard => {
                        turn_state.0 = TurnState::Finished;
                    }
                }
            }

            let mut transform = local_transform;
            transform.translation.z = 0.1 + 0.1 * discard_pile.1.cards.len() as f32;

            commands
                .entity(card_entity)
                .remove::<IsHeldBy>()
                .remove::<KnownCard>()
                .insert(value)
                .insert(ChildOf(discard_pile.0))
                .insert(transform);

            discard_pile.1.cards.push_back(card_entity);
        }
        ServerMessage::PlayerAtTurn { player_id } => {
            let &player_entity = player_exists!(player_id);

            for (entity, _) in player_at_turn.iter() {
                commands.entity(entity).remove::<PlayerAtTurn>();
            }
            commands
                .entity(player_entity)
                .insert(PlayerAtTurn(TurnState::Start));
        }
        ServerMessage::TakeCardFromDiscardPile { actor } => {
            let &_player_entity = player_exists!(actor);
            let turn_state = has_turn!(actor);

            if *turn_state != TurnState::Start {
                warn!(
                    "Can only take cards at the start of a turn, but turn state is {:?}.",
                    turn_state
                );
                return None;
            }
            *turn_state = TurnState::TookDiscardedCard;

            if held.iter().any(|held| held.1.0 == actor) {
                warn!(
                    "Player {} is already holding a card.",
                    actor.player_number()
                );
                return None;
            }

            let Some(card_entity) = discard_pile.1.cards.pop_back() else {
                warn!("Discard pile is empty.");
                return None;
            };

            commands
                .entity(card_entity)
                .insert(IsHeldBy(actor))
                .insert(Transform::from_xyz(0.0, 0.0, 10.0))
                .remove::<ChildOf>();
        }
        ServerMessage::SwapHeldCardWithSlotCard { actor, slot_id } => {
            let &player_entity = player_exists!(actor);
            let &slot_entity = slot_exists!(slot_id);

            let turn_state = has_turn!(actor);
            match turn_state {
                TurnState::TookDeckCard => {
                    *turn_state = TurnState::SwappedCard;
                }
                TurnState::TookDiscardedCard => {
                    *turn_state = TurnState::SwappedCard;
                }
                TurnState::Start => {
                    warn!("Cannot swap a card at the start of a turn.");
                    return None;
                }
                TurnState::Finished => {
                    warn!("Cannot swap a card at the end of a turn.");
                    return None;
                }
                TurnState::SwappedCard => {
                    warn!("Cannot swap a card twice.");
                    return None;
                }
            }

            let Some(slot_card_id) = try_get_slot_card!(slot_id) else {
                warn!("Slot is empty.");
                return None;
            };

            let &slot_card_entity = card_exists!(slot_card_id);

            let Some(held_card_entity) = try_get_held_card!(actor) else {
                warn!("Player {} is not holding a card.", actor.player_number());
                return None;
            };

            let Ok(player_state) = players.get_mut(player_entity) else {
                error!("Player index corrupted");
                return None;
            };

            if !player_state.slots.contains(&slot_entity) {
                warn!("Cannot swap a card on another player's slot.");
                return None;
            }

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
    }

    // If we're still here then we accepted the message.
    return Some(ProcessedMessage(msg));
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {}
}
