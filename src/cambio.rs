use bevy::{platform::collections::HashMap, prelude::*};
use bevy_renet::renet::ClientId;
use serde::{Deserialize, Serialize};

use crate::{cards::*, messages::ServerMessage};

#[derive(
    Debug, Component, Serialize, Deserialize, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord,
)]
pub struct PlayerId {
    pub player_number: u8,
    pub client_id: ClientId,
}

#[derive(Component)]
pub struct IsHeldBy(pub PlayerId);

#[derive(
    Debug, Serialize, Deserialize, Component, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord,
)]
pub enum CardId {
    PlayerCard { owned_by: PlayerId, slot_idx: usize },
    StackCard,
}

#[derive(
    Debug, Serialize, Deserialize, Component, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord,
)]
pub struct SlotId {
    pub player_id: PlayerId,
    pub slot_idx: usize,
}

#[derive(Component)]
pub struct MyPlayer;

#[derive(Component)]
#[require(Transform, InheritedVisibility)]
pub struct PlayerState {
    pub last_mouse_pos_world: Vec2,
    pub slots: Vec<Entity>,
}

#[derive(Resource, Default)]
pub struct CambioState {
    pub card_index: HashMap<CardId, Entity>,
    pub slot_index: HashMap<SlotId, Entity>,
    pub players: HashMap<PlayerId, Entity>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum CambioAction {
    PickUpCard { card_id: CardId },
    DropCardOnSlot { card_id: CardId, slot_id: SlotId },
}

pub struct CambioPlugin;

#[derive(Debug, Event)]
pub struct IncomingMessage(pub ServerMessage);

#[derive(Debug, Event)]
pub struct ProcessedMessage(pub ServerMessage);

impl Plugin for CambioPlugin {
    fn build(&self, app: &mut App) {
        app.add_event::<IncomingMessage>();
        app.add_event::<ProcessedMessage>();
        app.add_observer(setup_player);
        app.add_observer(setup_slot);
        app.add_observer(on_spawn_card);

        app.add_systems(PreUpdate, process_message);
    }
}

fn setup_player(trigger: Trigger<OnAdd, PlayerId>, mut commands: Commands) {
    println!("Setting up player");

    commands
        .entity(trigger.target())
        .insert(PlayerState {
            last_mouse_pos_world: Vec2::ZERO,
            slots: Vec::new(),
        })
        .with_children(|p| {
            for _ in 0..4 {
                p.spawn(CardSlot);
            }
        });
}

fn setup_slot(
    trigger: Trigger<OnAdd, CardSlot>,
    mut commands: Commands,
    mut state: ResMut<CambioState>,
    parents: Query<&ChildOf>,
    mut players: Query<(&PlayerId, &mut PlayerState)>,
) {
    println!("Setting up a slot");
    if let Ok(parent) = parents.get(trigger.target()) {
        if let Ok((player_id, mut player)) = players.get_mut(parent.0) {
            let slot_idx = player.slots.len();

            let x = 100.0 * (slot_idx % 2) as f32;
            let y = 100.0 * (slot_idx / 2) as f32;

            player.slots.push(trigger.target());
            let slot_id = SlotId {
                player_id: *player_id,
                slot_idx,
            };

            let slot = commands
                .entity(trigger.target())
                .insert((
                    Name::new(format!("Slot {}", slot_idx)),
                    Transform::from_xyz(x, y, 0.0),
                    slot_id,
                ))
                .with_child((
                    SomeCard,
                    CardId::PlayerCard {
                        owned_by: *player_id,
                        slot_idx,
                    },
                    Transform::from_xyz(2.0, 2.0, 1.0),
                ))
                .id();

            state.slot_index.insert(slot_id, slot);
        }
    }
}

fn on_spawn_card(
    trigger: Trigger<OnAdd, CardId>,
    card_ids: Query<&CardId>,
    mut state: ResMut<CambioState>,
) {
    let card_id = *card_ids.get(trigger.target()).unwrap();
    state.card_index.insert(card_id, trigger.target());
}

pub fn process_message(
    mut claims: EventReader<IncomingMessage>,
    mut validated: EventWriter<ProcessedMessage>,
    mut state: ResMut<CambioState>,
    held: Query<&IsHeldBy>,
    mut transforms: Query<&mut Transform>,
    mut commands: Commands,
) {
    for IncomingMessage(msg) in claims.read() {
        println!("Processing message: {:?}", msg);
        match msg {
            ServerMessage::PlayerConnected { player_id } => {
                println!("Player {} connected.", player_id.player_number);
                let x = 150.0 - 300.0 * (player_id.player_number / 2) as f32;
                let y = 150.0 - 300.0 * (player_id.player_number % 2) as f32;

                let player_entity = commands
                    .spawn((*player_id, Transform::from_xyz(x, y, 0.0)))
                    .id();

                state.players.insert(*player_id, player_entity);
                validated.write(ProcessedMessage(msg.clone()));
            }
            ServerMessage::PlayerDisconnected { player_id } => {
                println!("Player {} disconnected.", player_id.player_number);

                let player_entity = *state.players.get(player_id).unwrap();
                commands.entity(player_entity).despawn_related::<Children>();
                state.players.remove(player_id);

                validated.write(ProcessedMessage(msg.clone()));
            }
            ServerMessage::StateUpdate { claimer_id, action } => match action {
                CambioAction::PickUpCard { card_id } => {
                    if held.iter().any(|held| &held.0 == claimer_id) {
                        warn!(
                            "Player {} is already holding a card.",
                            claimer_id.player_number
                        );
                        continue;
                    }

                    let Some(card_entity) = state.card_index.get(card_id) else {
                        warn!("Card not found.");
                        continue;
                    };

                    if held.contains(*card_entity) {
                        warn!("Card already held.");
                        continue;
                    }

                    let card_entity = *state.card_index.get(card_id).unwrap();
                    commands
                        .entity(card_entity)
                        .insert(IsHeldBy(*claimer_id))
                        .remove::<ChildOf>()
                        .remove::<Pickable>();
                    validated.write(ProcessedMessage(msg.clone()));
                }
                CambioAction::DropCardOnSlot { card_id, slot_id } => {
                    let Some(&card_entity) = state.card_index.get(card_id) else {
                        warn!("Card not found.");
                        continue;
                    };

                    let Some(&slot_entity) = state.slot_index.get(slot_id) else {
                        warn!("Slot not found.");
                        continue;
                    };

                    let Ok(held_by) = held.get(card_entity) else {
                        warn!("Card not held.");
                        continue;
                    };

                    if &held_by.0 != claimer_id {
                        warn!(
                            "Player {} is not holding the card.",
                            claimer_id.player_number
                        );
                        continue;
                    }

                    commands
                        .entity(card_entity)
                        .remove::<IsHeldBy>()
                        .insert(ChildOf(slot_entity))
                        .insert(Pickable::default());

                    if let Ok(mut transform) = transforms.get_mut(card_entity) {
                        transform.translation.x = 2.0;
                        transform.translation.y = 2.0;
                    }

                    validated.write(ProcessedMessage(msg.clone()));
                }
            },
        }
    }
}
