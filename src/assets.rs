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

    #[asset(path = "slap_table.png")]
    pub slap_table_sprite: Handle<Image>,

    #[asset(path = "disconnected.png")]
    pub disconnected_sprite: Handle<Image>,

    #[asset(path = "cursor.png")]
    pub cursor_sprite: Handle<Image>,

    #[asset(path = "locked.png")]
    pub locked_sprite: Handle<Image>,

    #[asset(path = "card_sweep.ogg")]
    pub card_sweep: Handle<AudioSource>,

    #[asset(path = "card_swap.ogg")]
    pub card_swap: Handle<AudioSource>,

    #[asset(path = "card_laydown.ogg")]
    pub card_laydown: Handle<AudioSource>,

    #[asset(path = "vo_shuffle.ogg")]
    pub vo_shuffle: Handle<AudioSource>,

    #[asset(path = "vo_finalscores.ogg")]
    pub vo_finalscores: Handle<AudioSource>,

    #[asset(path = "ready_1.ogg")]
    pub slap_table_sound: Handle<AudioSource>,

    #[asset(path = "wrong.ogg")]
    pub wrong_sound: Handle<AudioSource>,
}

#[derive(Clone, Eq, PartialEq, Debug, Hash, Default, States)]
#[states(scoped_entities)]
pub enum GamePhase {
    #[default]
    AssetLoading,
    Menu,
    Connecting,
    Playing,
    ConnectionLost,
}

pub struct GameAssetPlugin;

impl Plugin for GameAssetPlugin {
    fn build(&self, app: &mut App) {
        app.init_state::<GamePhase>()
            .enable_state_scoped_entities::<GamePhase>()
            .add_loading_state(
                LoadingState::new(GamePhase::AssetLoading)
                    .continue_to_state(GamePhase::Menu)
                    .load_collection::<GameAssets>(),
            );
    }
}
