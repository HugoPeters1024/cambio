use bevy::{
    asset::RenderAssetUsages,
    prelude::*,
    render::mesh::{Indices, PrimitiveTopology},
};
use bevy_asset_loader::prelude::*;

pub const CARD_WIDTH: f32 = 256.0;
pub const CARD_HEIGHT: f32 = 356.0;

pub const DESIRED_CARD_WIDTH: f32 = CARD_WIDTH / 5.0;
pub const DESIRED_CARD_HEIGHT: f32 = CARD_HEIGHT / 5.0;

#[derive(AssetCollection, Resource)]
pub struct GameAssets {
    #[asset(texture_atlas_layout(tile_size_x = 256, tile_size_y = 356, columns = 13, rows = 4))]
    pub card_sheet_layout: Handle<TextureAtlasLayout>,

    #[asset(path = "cards.jpg")]
    pub card_sheet: Handle<Image>,

    #[asset(path = "card_back.jpg")]
    pub card_back: Handle<Image>,
}

#[derive(Resource)]
pub struct CustomMeshes {
    pub card_slot: Handle<Mesh>,
    pub card_slot_material: Handle<ColorMaterial>,
}

#[derive(Clone, Eq, PartialEq, Debug, Hash, Default, States)]
pub enum GamePhase {
    #[default]
    AssetLoading,
    Playing,
}

pub struct GameAssetPlugin;

impl Plugin for GameAssetPlugin {
    fn build(&self, app: &mut App) {
        app.init_state::<GamePhase>().add_loading_state(
            LoadingState::new(GamePhase::AssetLoading)
                .continue_to_state(GamePhase::Playing)
                .load_collection::<GameAssets>(),
        );

        app.add_systems(Startup, setup_custom_meshes);
    }
}

fn setup_custom_meshes(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    // Define the properties of our rectangle outline
    let width = DESIRED_CARD_WIDTH;
    let height = DESIRED_CARD_HEIGHT;
    let thickness = 5.0; // The desired line thickness in pixels

    // Calculate the coordinates for the inner and outer rectangles
    let outer_width = width;
    let outer_height = height;
    let inner_width = width - (thickness * 2.0);
    let inner_height = height - (thickness * 2.0);

    // Define the vertices for the outer rectangle (0, 1, 2, 3) and inner rectangle (4, 5, 6, 7).
    let vertices = vec![
        // Outer vertices (clockwise, starting from top-left)
        [-outer_width / 2.0, outer_height / 2.0, 0.0], // 0
        [outer_width / 2.0, outer_height / 2.0, 0.0],  // 1
        [outer_width / 2.0, -outer_height / 2.0, 0.0], // 2
        [-outer_width / 2.0, -outer_height / 2.0, 0.0], // 3
        // Inner vertices (clockwise, starting from top-left)
        [-inner_width / 2.0, inner_height / 2.0, 0.0], // 4
        [inner_width / 2.0, inner_height / 2.0, 0.0],  // 5
        [inner_width / 2.0, -inner_height / 2.0, 0.0], // 6
        [-inner_width / 2.0, -inner_height / 2.0, 0.0], // 7
    ];

    // Define the indices to form the four rectangular sections of the outline.
    // Each section is made of two triangles.
    let indices = Indices::U32(vec![
        // Top section
        0, 1, 5, 0, 5, 4, // Right section
        1, 2, 6, 1, 6, 5, // Bottom section
        2, 3, 7, 2, 7, 6, // Left section
        3, 0, 4, 3, 4, 7,
    ]);

    // Create a new mesh using the TriangleList topology
    let mut mesh = Mesh::new(
        PrimitiveTopology::TriangleList,
        RenderAssetUsages::default(),
    );

    // Insert the vertex positions and indices into the mesh
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, vertices);
    mesh.insert_indices(indices);

    commands.insert_resource(CustomMeshes {
        card_slot: meshes.add(mesh),
        card_slot_material: materials.add(Color::BLACK),
    });
}
