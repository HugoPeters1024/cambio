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
#[require(Transform, Sprite, Pickable)]
pub struct SomeCard;

#[derive(Debug, Component, Default)]
#[require(Transform, Sprite, Pickable)]
pub struct CardSlot;

pub struct CardPlugin;

impl Plugin for CardPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            PreUpdate,
            (sync_sprite_with_card).run_if(in_state(GamePhase::Playing)),
        );
        app.add_observer(on_spawn_slot);
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
                custom_size: Some(Vec2::new(DESIRED_CARD_WIDTH, DESIRED_CARD_HEIGHT)),
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
                custom_size: Some(Vec2::new(DESIRED_CARD_WIDTH, DESIRED_CARD_HEIGHT)),
                ..Sprite::from_image(assets.card_back.clone())
            };
        }
    }
}

fn on_spawn_slot(trigger: Trigger<OnAdd, CardSlot>, mut commands: Commands) {
    commands
        .entity(trigger.target())
        .insert((Sprite::from_color(
            Color::linear_rgb(0.0, 0.2, 0.0),
            Vec2::new(DESIRED_CARD_WIDTH, DESIRED_CARD_HEIGHT),
        ),));
}
