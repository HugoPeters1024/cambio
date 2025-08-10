use bevy::prelude::*;

mod assets;
mod cards;
mod logic;

use crate::assets::*;
use crate::cards::*;
use crate::logic::*;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                title: "float_me_pls".to_string(),
                ..default()
            }),
            ..default()
        }))
        .add_plugins((GameAssetPlugin, CardPlugin, LogicPlugin))
        .add_systems(OnEnter(GameState::Playing), setup)
        .run();
}

fn setup(mut commands: Commands) {
    commands.spawn(Camera2d);

    commands.spawn(Card {
        suit: Suit::Diamonds,
        rank: Rank::Five,
    });
}
