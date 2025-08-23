use bevy::{platform::collections::HashMap, prelude::*, sprite::Anchor};
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

#[derive(Resource)]
pub struct CambioState {
    pub root: Entity,
    pub card_index: HashMap<CardId, Entity>,
    pub slot_index: HashMap<SlotId, Entity>,
    pub player_index: HashMap<PlayerId, Entity>,
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

        app.world_mut()
            .run_system_cached(setup_game_resource)
            .unwrap();

        app.add_observer(setup_player);
        app.add_observer(setup_slot);
        app.add_observer(on_spawn_card);

        app.add_systems(Startup, setup_game);
        app.add_systems(PreUpdate, process_message);
    }
}

fn setup_game_resource(mut commands: Commands) {
    let root = commands
        .spawn((
            Name::new("Cambio"),
            InheritedVisibility::default(),
            Transform::default(),
        ))
        .id();

    let state = CambioState {
        root,
        card_index: HashMap::new(),
        slot_index: HashMap::new(),
        player_index: HashMap::new(),
    };

    commands.insert_resource(state);
}

fn setup_game(mut commands: Commands, state: Res<CambioState>) {
    commands.entity(state.root).with_children(|p| {
        p.spawn((
            SomeCard,
            CardId::StackCard,
            KnownCard {
                suit: Suit::Spades,
                rank: Rank::Ace,
            },
        ));
    });
}

fn setup_player(trigger: Trigger<OnAdd, PlayerId>, mut commands: Commands) {
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
    if let Ok(parent) = parents.get(trigger.target()) {
        if let Ok((player_id, mut player)) = players.get_mut(parent.0) {
            let slot_idx = player.slots.len();

            let x = (DESIRED_CARD_WIDTH + 10.0) * (slot_idx % 2) as f32 - DESIRED_CARD_WIDTH;
            let y = (DESIRED_CARD_HEIGHT + 10.0) * (slot_idx / 2) as f32 - DESIRED_CARD_HEIGHT;

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
    mut commands: Commands,
    card_ids: Query<&CardId>,
    mut state: ResMut<CambioState>,
) {
    commands.entity(trigger.target()).insert(Anchor::Center);
    let card_id = *card_ids.get(trigger.target()).unwrap();
    state.card_index.insert(card_id, trigger.target());
}

pub fn process_message(
    mut claims: EventReader<IncomingMessage>,
    mut validated: EventWriter<ProcessedMessage>,
    mut state: ResMut<CambioState>,
    held: Query<&IsHeldBy>,
    mut commands: Commands,
) {
    for IncomingMessage(msg) in claims.read() {
        println!("Processing message: {:?}", msg);
        match msg {
            ServerMessage::PlayerConnected { player_id } => {
                println!("Player {} connected.", player_id.player_number);

                let phi = 2.0 * std::f32::consts::PI * (player_id.player_number - 1) as f32 / 4.0;

                let x = 150.0 * phi.cos();
                let y = 150.0 * phi.sin();

                let player_entity = commands
                    .spawn((
                        *player_id,
                        Name::new(format!("Player {}", player_id.player_number)),
                        Transform::from_xyz(x, y, 0.0),
                        ChildOf(state.root),
                    ))
                    .id();

                state.player_index.insert(*player_id, player_entity);
                validated.write(ProcessedMessage(msg.clone()));
            }
            ServerMessage::PlayerDisconnected { player_id } => {
                println!("Player {} disconnected.", player_id.player_number);

                let player_entity = *state.player_index.get(player_id).unwrap();
                commands.entity(player_entity).despawn_related::<Children>();
                state.player_index.remove(player_id);

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
                        .insert(Transform::from_xyz(0.0, 0.0, 10.0))
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
                        .insert(Pickable::default())
                        .insert(Transform::from_xyz(2.0, 2.0, 1.0));

                    validated.write(ProcessedMessage(msg.clone()));
                }
            },
        }
    }
}
