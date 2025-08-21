use bevy::{picking::hover::PickingInteraction, prelude::*};

use crate::assets::*;

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
pub struct KnownCard {
    pub suit: Suit,
    pub rank: Rank,
}

#[derive(Debug, Component, PartialEq, Eq, Clone, Copy, Default)]
#[require(Transform, Sprite, PickingInteraction, Pickable)]
pub struct SomeCard;

pub struct CardPlugin;

impl Plugin for CardPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            PreUpdate,
            sync_sprite_with_card.run_if(in_state(GamePhase::Playing)),
        );
    }
}

fn sync_sprite_with_card(
    mut cards: Query<(Entity, &mut Sprite), With<SomeCard>>,
    known_cards: Query<&KnownCard>,
    assets: Res<GameAssets>,
) {
    for (entity, mut sprite) in cards.iter_mut() {
        if let Ok(card) = known_cards.get(entity) {
            *sprite = Sprite {
                custom_size: Some(Vec2::new(CARD_WIDTH, CARD_HEIGHT) * 0.33),
                ..Sprite::from_atlas_image(
                    assets.card_sheet.clone(),
                    TextureAtlas {
                        layout: assets.card_sheet_layout.clone(),
                        index: card.suit as usize * 13 + card.rank as usize,
                    },
                )
            };
        } else {
            *sprite = Sprite {
                custom_size: Some(Vec2::new(CARD_WIDTH, CARD_HEIGHT) * 0.33),
                ..Sprite::from_image(assets.card_back.clone())
            };
        }
    }
}
