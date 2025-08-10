use bevy::prelude::*;
use bevy_asset_loader::prelude::*;

#[derive(AssetCollection, Resource)]
pub struct GameAssets {
    #[asset(texture_atlas_layout(tile_size_x = 256, tile_size_y = 356, columns = 13, rows = 4))]
    pub card_sheet_layout: Handle<TextureAtlasLayout>,

    #[asset(path = "cards.jpg")]
    pub card_sheet: Handle<Image>,
}

#[derive(Clone, Eq, PartialEq, Debug, Hash, Default, States)]
pub enum GameState {
    #[default]
    AssetLoading,
    Playing,
}

pub struct GameAssetPlugin;

impl Plugin for GameAssetPlugin {
    fn build(&self, app: &mut App) {
        app.init_state::<GameState>().add_loading_state(
            LoadingState::new(GameState::AssetLoading)
                .continue_to_state(GameState::Playing)
                .load_collection::<GameAssets>(),
        );
    }
}
