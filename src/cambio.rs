use bevy::{platform::collections::HashMap, prelude::*};
use bevy_renet::renet::ClientId;
use serde::{Deserialize, Serialize};

use crate::cards::*;

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
    pub players: HashMap<PlayerId, Entity>,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum CambioAction {
    PickUpCard { card_id: CardId },
}

pub struct CambioPlugin;

impl Plugin for CambioPlugin {
    fn build(&self, app: &mut App) {
        app.add_observer(setup_player);
        app.add_observer(setup_slot);
        app.add_observer(handle_card_being_picked_up);
        app.add_observer(on_spawn_card);
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

            commands
                .entity(trigger.target())
                .insert((
                    Name::new(format!("Slot {}", slot_idx)),
                    Transform::from_xyz(x, y, 0.0),
                ))
                .with_child((
                    SomeCard,
                    CardId::PlayerCard {
                        owned_by: *player_id,
                        slot_idx,
                    },
                    Transform::from_xyz(10.0, 10.0, 1.0),
                ));
        }
    }
}

fn handle_card_being_picked_up(trigger: Trigger<OnAdd, IsHeldBy>, mut commands: Commands) {
    println!("Card picked up");
    commands
        .entity(trigger.target())
        .remove::<ChildOf>()
        .remove::<Pickable>();
}

fn on_spawn_card(
    trigger: Trigger<OnAdd, CardId>,
    card_ids: Query<&CardId>,
    mut state: ResMut<CambioState>,
) {
    let card_id = *card_ids.get(trigger.target()).unwrap();
    state.card_index.insert(card_id, trigger.target());
}
