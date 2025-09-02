use bevy::prelude::*;
use serde::{Deserialize, Serialize};
use strum_macros::EnumIter;

use crate::assets::*;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, Serialize, Deserialize, EnumIter)]
pub enum Suit {
    #[default]
    Spades,
    Hearts,
    Clubs,
    Diamonds,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, Serialize, Deserialize, EnumIter)]
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

#[derive(Debug, Component, PartialEq, Eq, Clone, Copy, Default, Serialize, Deserialize)]
pub struct KnownCard {
    pub suit: Suit,
    pub rank: Rank,
}

impl KnownCard {
    pub fn penalty_score(&self) -> i32 {
        match self.rank {
            Rank::Ace => 1,
            Rank::Two => 2,
            Rank::Three => 3,
            Rank::Four => 4,
            Rank::Five => 5,
            Rank::Six => 6,
            Rank::Seven => 7,
            Rank::Eight => 8,
            Rank::Nine => 9,
            Rank::Ten => 10,
            Rank::Jack => 10,
            Rank::Queen => 10,
            Rank::King => {
                if self.suit == Suit::Hearts || self.suit == Suit::Diamonds {
                    -1
                } else {
                    10
                }
            }
        }
    }
}

#[derive(Debug, Component, PartialEq, Eq, Clone, Copy, Default)]
#[require(Transform, Sprite, GrowOnHover)]
pub struct SomeCard;

#[derive(Debug, Component, Default)]
#[require(Transform, Sprite, Pickable)]
pub struct CardSlot;

pub struct CardPlugin;

#[derive(Component, Default)]
pub struct GrowOnHover;

impl Plugin for CardPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            PreUpdate,
            (sync_sprite_with_card).run_if(in_state(GamePhase::Playing)),
        );
        app.add_observer(on_spawn_card);
        app.add_observer(on_spawn_slot);
        app.add_observer(setup_grow_on_hover);
    }
}

fn setup_grow_on_hover(trigger: Trigger<OnAdd, GrowOnHover>, mut commands: Commands) {
    fn on_hover_in(trigger: Trigger<Pointer<Over>>, mut t: Query<&mut Transform>) {
        if let Ok(mut transform) = t.get_mut(trigger.target()) {
            transform.scale.x = 1.1;
            transform.scale.y = 1.1;
        }
    }

    fn on_hover_out(trigger: Trigger<Pointer<Out>>, mut t: Query<&mut Transform>) {
        if let Ok(mut transform) = t.get_mut(trigger.target()) {
            transform.scale.x = 1.0;
            transform.scale.y = 1.0;
        }
    }

    commands
        .entity(trigger.target())
        .observe(on_hover_in)
        .observe(on_hover_out);
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

fn on_spawn_card(trigger: Trigger<OnAdd, SomeCard>, mut commands: Commands) {
    // Add a nice outline for constrast
    commands.entity(trigger.target()).with_child((
        Sprite::from_color(
            Color::srgb(0.1, 0.1, 0.1),
            Vec2::new(DESIRED_CARD_WIDTH + 2.0, DESIRED_CARD_HEIGHT + 2.0),
        ),
        Transform::from_translation(Vec3::new(0.0, 0.0, -0.001)),
    ));
}

fn on_spawn_slot(trigger: Trigger<OnAdd, CardSlot>, mut commands: Commands) {
    commands.entity(trigger.target()).insert(Sprite::from_color(
        Color::srgb(0.0, 0.2, 0.0),
        Vec2::new(DESIRED_CARD_WIDTH + 4.0, DESIRED_CARD_HEIGHT + 4.0),
    ));
}
