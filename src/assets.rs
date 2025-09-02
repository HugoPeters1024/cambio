use bevy::prelude::*;
use bevy_asset_loader::prelude::*;

pub const CARD_WIDTH: f32 = 256.0;
pub const CARD_HEIGHT: f32 = 356.0;

pub const DESIRED_CARD_WIDTH: f32 = CARD_WIDTH / 6.0;
pub const DESIRED_CARD_HEIGHT: f32 = CARD_HEIGHT / 6.0;

#[derive(AssetCollection, Resource)]
pub struct GameAssets {
    #[asset(texture_atlas_layout(tile_size_x = 256, tile_size_y = 356, columns = 13, rows = 4))]
    pub card_sheet_layout: Handle<TextureAtlasLayout>,

    #[asset(path = "cards.jpg")]
    pub card_sheet: Handle<Image>,

    #[asset(path = "card_back.jpg")]
    pub card_back: Handle<Image>,

    #[asset(path = "arrow_down.png")]
    pub arrow_down_sprite: Handle<Image>,

    #[asset(path = "card_sweep.ogg")]
    pub card_sweep: Handle<AudioSource>,

    #[asset(path = "card_swap.ogg")]
    pub card_swap: Handle<AudioSource>,

    #[asset(path = "card_laydown.ogg")]
    pub card_laydown: Handle<AudioSource>,

    #[asset(path = "vo_shuffle.ogg")]
    pub vo_shuffle: Handle<AudioSource>,
}

#[derive(Clone, Eq, PartialEq, Debug, Hash, Default, States)]
pub enum GamePhase {
    #[default]
    AssetLoading,
    Menu,
    Playing,
}

pub struct GameAssetPlugin;

impl Plugin for GameAssetPlugin {
    fn build(&self, app: &mut App) {
        app.init_state::<GamePhase>().add_loading_state(
            LoadingState::new(GamePhase::AssetLoading)
                .continue_to_state(GamePhase::Menu)
                .load_collection::<GameAssets>(),
        );
    }
}
