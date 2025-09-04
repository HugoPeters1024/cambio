use bevy::asset::AssetMetaCheck;
use bevy::prelude::*;
use bevy::window::PrimaryWindow;
use bevy::window::WindowResolution;
use bevy::winit::UpdateMode;
use bevy::winit::WinitSettings;
use bevy::winit::WinitWindows;
use std::io::Cursor;
use winit::window::Icon;

mod assets;
mod cambio;
mod cards;
mod client;
mod host_utils;
mod menu;
mod menu_button;
mod messages;
mod transport;
mod utils;

use crate::assets::*;
use crate::cambio::CambioPlugin;
use crate::cards::*;
use crate::client::ClientPlugin;
use crate::transport::TransportPlugin;

fn main() {
    let mut app = App::new();
    app.add_plugins(bevy_rand::plugin::EntropyPlugin::<bevy_rand::prelude::WyRand>::default());

    app.add_plugins(
        DefaultPlugins
            .set(WindowPlugin {
                primary_window: Some(Window {
                    title: "float_me_pls".to_string(),
                    #[cfg(target_arch = "wasm32")]
                    resolution: WindowResolution::new(1280.0, 720.0),
                    #[cfg(not(target_arch = "wasm32"))]
                    resolution: WindowResolution::new(800.0, 600.0),
                    // Bind to canvas included in `index.html`
                    canvas: Some("#bevy".to_owned()),
                    // fill the entire browser window
                    fit_canvas_to_parent: true,
                    // don't hijack keyboard shortcuts like F5, F6, F12, Ctrl+R etc.
                    prevent_default_event_handling: false,
                    ..default()
                }),
                ..default()
            })
            .set(AssetPlugin {
                // Breaks in the browser from some reason otherwise
                meta_check: AssetMetaCheck::Never,
                ..default()
            }),
    );

    // Because we doing networking, there might be updates to the game
    // even when the window is not in focus, so we should ensure our update
    // systems keep running in that case.
    app.insert_resource(WinitSettings {
        unfocused_mode: UpdateMode::Continuous,
        focused_mode: UpdateMode::Continuous,
        ..default()
    });
    app.add_plugins(bevy_tweening::TweeningPlugin);
    app.add_plugins(TransportPlugin);
    app.add_plugins(ClientPlugin);
    app.add_plugins(CambioPlugin);
    app.add_plugins(crate::menu::MenuPlugin);

    app.add_plugins((GameAssetPlugin, CardPlugin));

    app.add_systems(Startup, set_window_icon);

    app.run();
}

// Sets the icon on windows and X11
fn set_window_icon(
    windows: NonSend<WinitWindows>,
    primary_window: Query<Entity, With<PrimaryWindow>>,
) -> Result {
    let primary_entity = primary_window.single()?;
    let Some(primary) = windows.get_window(primary_entity) else {
        return Err(BevyError::from("No primary window!"));
    };
    let icon_buf = Cursor::new(include_bytes!(
        "../build/macos/AppIcon.iconset/icon_256x256.png"
    ));
    if let Ok(image) = image::load(icon_buf, image::ImageFormat::Png) {
        let image = image.into_rgba8();
        let (width, height) = image.dimensions();
        let rgba = image.into_raw();
        let icon = Icon::from_rgba(rgba, width, height).unwrap();
        primary.set_window_icon(Some(icon));
    };

    Ok(())
}
