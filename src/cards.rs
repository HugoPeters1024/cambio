use bevy::{picking::hover::PickingInteraction, prelude::*, window::PrimaryWindow};
use bevy_renet::renet::{ClientId, RenetClient};
use serde::{Deserialize, Serialize};

use crate::{
    assets::{GameAssets, GameState},
    client::ClientExt,
    messages::ClientClaim,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Suit {
    Spades,
    Hearts,
    Clubs,
    Diamonds,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Rank {
    Ace,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
}

// TODO(this needs to specify and index for a specific card in the game)
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Component)]
pub struct CardRef {
    pub client_id: ClientId,
}

#[derive(Debug, Component, PartialEq, Eq, Clone, Copy)]
#[require(Transform)]
pub struct Card {
    pub suit: Suit,
    pub rank: Rank,
}


pub struct CardPlugin;

impl Plugin for CardPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            PreUpdate,
            (on_spawn, sync_sprite_with_card)
                .chain()
                .run_if(in_state(GameState::Playing)),
        );
    }
}

fn on_spawn(mut commands: Commands, q: Query<Entity, Added<Card>>, assets: Res<GameAssets>) {
    for e in q.iter() {
        commands
            .entity(e)
            .insert((
                Sprite::from_atlas_image(
                    assets.card_sheet.clone(),
                    TextureAtlas {
                        layout: assets.card_sheet_layout.clone(),
                        index: 0,
                    },
                ),
                Transform::from_scale(Vec3::splat(0.5)),
                PickingInteraction::default(),
                Pickable::default(),
            ))
            .observe(select_card);
    }
}

fn sync_sprite_with_card(mut q: Query<(&mut Sprite, &Card), Changed<Card>>) {
    for (mut sprite, card) in q.iter_mut() {
        if let Some(texture_atlas) = sprite.texture_atlas.as_mut() {
            texture_atlas.index = card.suit as usize * 13 + card.rank as usize;
        }
    }
}

fn select_card(
    trigger: Trigger<Pointer<Click>>,
    refs: Query<&CardRef, With<Card>>,
    mut client: ResMut<RenetClient>,
) {
    if let Ok(card_ref) = refs.get(trigger.target) {
        let claim = ClientClaim::PickupCard(*card_ref);
        client.send_claim(claim);
    }
}
