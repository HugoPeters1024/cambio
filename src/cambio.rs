use std::collections::VecDeque;

use bevy::{platform::collections::HashMap, prelude::*};
use bevy_renet::renet::ClientId;
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

#[derive(Component)]
pub struct CambioRoot;

#[derive(Component)]
pub struct DiscardPile;

#[derive(Component)]
#[require(Transform, InheritedVisibility)]
pub struct PlayerState {
    pub last_mouse_pos_world: Vec2,
    pub slots: Vec<Entity>,
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
        DiscardPile,
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
    discard_pile: Single<Entity, With<DiscardPile>>,
    mut commands: Commands,
) -> Option<ProcessedMessage> {
    println!("Processing message: {:?}", msg);
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
                        slots: Vec::new(),
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
            let Some(player_entity) = state.player_index.get(&actor) else {
                warn!("Player not found.");
                return None;
            };

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

            player.slots.push(slot_entity);
            state.slot_index.insert(slot_id, slot_entity);
        }
        ServerMessage::ReceiveFreshCard {
            actor: _,
            slot_id,
            card_id,
        } => {
            let Some(&slot_entity) = state.slot_index.get(&slot_id) else {
                warn!("Slot not found.");
                return None;
            };

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
        ServerMessage::PickUpCard { actor, card_id } => {
            if held.iter().any(|held| held.1.0 == actor) {
                warn!(
                    "Player {} is already holding a card.",
                    actor.player_number()
                );
                return None;
            }

            let Some(card_entity) = state.card_index.get(&card_id) else {
                warn!("Card not found.");
                return None;
            };

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
            let Some(&card_entity) = state.card_index.get(&card_id) else {
                warn!("Card not found.");
                return None;
            };

            let Some(&slot_entity) = state.slot_index.get(&slot_id) else {
                error!("Slot index corrupted.");
                return None;
            };

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
            let Some(&card_entity) = state.card_index.get(&card_id) else {
                warn!("Card not found.");
                return None;
            };

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
            if held.iter().any(|held| held.1.0 == actor) {
                warn!(
                    "Player {} is already holding a card.",
                    actor.player_number()
                );
                return None;
            }

            let card_entity = commands.spawn((
                SomeCard,
                card_id,
                Name::new(format!("Card {}", card_id.0)),
                IsHeldBy(actor),
                Transform::from_xyz(0.0, 0.0, 10.0),
            ));

            state.card_index.insert(card_id, card_entity.id());
        }
        ServerMessage::DropCardOnDiscardPile { actor, card_id } => {
            let Some(&card_entity) = state.card_index.get(&card_id) else {
                warn!("Card not found.");
                return None;
            };

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

            commands
                .entity(card_entity)
                .remove::<IsHeldBy>()
                .remove::<KnownCard>()
                .insert(ChildOf(*discard_pile))
                .insert(Transform::from_xyz(0.0, 0.0, 1.0));
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
