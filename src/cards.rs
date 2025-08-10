use bevy::{picking::hover::PickingInteraction, prelude::*, window::PrimaryWindow};

use crate::assets::{GameAssets, GameState};

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Suit {
    Spades,
    Hearts,
    Clubs,
    Diamonds,
}

#[derive(PartialEq, Eq, Clone, Copy)]
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

#[derive(Component, PartialEq, Eq, Clone, Copy)]
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
        app.add_systems(PreUpdate, move_with_cursor);
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

#[derive(Component)]
struct Dragging {
    offset: Vec2,
}

fn select_card(
    trigger: Trigger<Pointer<Click>>,
    mut commands: Commands,
    dragging: Query<&Dragging, With<Card>>,
) {
    if dragging.contains(trigger.target) {
        commands.entity(trigger.target).remove::<Dragging>();
    } else {
        commands
            .entity(trigger.target)
            .insert(Dragging { offset: Vec2::ZERO });
    }
}

fn move_with_cursor(
    mut query: Query<(&mut Transform, &Dragging), With<Card>>,
    window: Single<&Window, With<PrimaryWindow>>,
    camera: Single<(&Camera, &GlobalTransform)>,
) {
    if let Some(mpos) = window.cursor_position() {
        for (mut transform, dragging) in query.iter_mut() {
            if let Ok(world_pos) = camera.0.viewport_to_world_2d(camera.1, mpos) {
                transform.translation.x = world_pos.x + dragging.offset.x;
                transform.translation.y = world_pos.y + dragging.offset.y;
            }
        }
    }
}
