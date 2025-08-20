use bevy::{picking::hover::PickingInteraction, prelude::*};
use bevy_renet::renet::RenetClient;

use crate::assets::*;
use crate::cambio::{CambioAction, CardId};
use crate::client::ClientExt;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub enum Suit {
    #[default]
    Spades,
    Hearts,
    Clubs,
    Diamonds,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub enum Rank {
    #[default]
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

#[derive(Debug, Component, PartialEq, Eq, Clone, Copy, Default)]
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
                .run_if(in_state(GamePhase::Playing)),
        );
    }
}

fn on_spawn(mut commands: Commands, q: Query<Entity, Added<Card>>, assets: Res<GameAssets>) {
    for e in q.iter() {
        commands
            .entity(e)
            .insert((
                Sprite {
                    custom_size: Some(Vec2::new(CARD_WIDTH, CARD_HEIGHT) * 0.5),
                    ..Sprite::from_atlas_image(
                        assets.card_sheet.clone(),
                        TextureAtlas {
                            layout: assets.card_sheet_layout.clone(),
                            index: 0,
                        },
                    )
                },
                Transform::from_scale(Vec3::splat(0.5)),
                PickingInteraction::default(),
                Pickable::default(),
            ))
            .observe(select_card);
    }
}

fn sync_sprite_with_card(mut q: Query<(&mut Sprite, &Card)>) {
    for (mut sprite, card) in q.iter_mut() {
        if let Some(texture_atlas) = sprite.texture_atlas.as_mut() {
            texture_atlas.index = card.suit as usize * 13 + card.rank as usize;
        }
    }
}

fn select_card(
    trigger: Trigger<Pointer<Click>>,
    cards: Query<&CardId>,
    mut client: ResMut<RenetClient>,
    camera: Single<(&Camera, &GlobalTransform)>,
) {
    if let Ok(card) = cards.get(trigger.target) {
        if let Ok(world_pos) = camera
            .0
            .viewport_to_world_2d(camera.1, trigger.event().pointer_location.position)
        {
            let claim = CambioAction::PickUpCard {
                card: *card,
            };
            client.send_claim(claim);
        }
    }
}
