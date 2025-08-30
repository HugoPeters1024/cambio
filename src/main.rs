use bevy::prelude::*;
use bevy::window::WindowResolution;
use bevy::winit::UpdateMode;
use bevy::winit::WinitSettings;

mod assets;
mod cambio;
mod cards;
mod client;
mod messages;
mod utils;

#[cfg(not(target_arch = "wasm32"))]
mod server;

use crate::assets::*;
use crate::cambio::CambioPlugin;
use crate::cards::*;
use crate::client::ClientPlugin;

#[derive(Component)]
struct PlayerIdxText;

fn main() {
    println!("Usage: run with \"server\" or \"client\" argument");
    let args: Vec<String> = std::env::args().collect();

    let exec_type = if args.len() < 2 { "client" } else { &args[1] };
    let is_host = match exec_type {
        "client" => false,
        "server" => true,
        _ => panic!("Usage: run with \"server\" or \"client\" argument"),
    };

    let mut app = App::new();
    app.add_plugins(bevy_rand::plugin::EntropyPlugin::<bevy_rand::prelude::WyRand>::default());

    if is_host {
        #[cfg(target_arch = "wasm32")]
        panic!("Server is not supported on this platform");

        #[cfg(not(target_arch = "wasm32"))]
        {
            app.add_plugins(MinimalPlugins);
            app.add_plugins(bevy::log::LogPlugin::default());
            app.add_plugins(CambioPlugin);
            app.add_plugins(crate::server::ServerPlugin);
            app.insert_resource(Time::<Fixed>::from_hz(300.0));
        }
    } else {
        app.add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                title: "float_me_pls".to_string(),
                resolution: WindowResolution::new(640.0, 480.0),
                ..default()
            }),
            ..default()
        }));

        // Because we doing networking, there might be updates to the game
        // even when the window is not in focus, so we should ensure our update
        // systems keep running in that case.
        app.insert_resource(WinitSettings {
            unfocused_mode: UpdateMode::Continuous,
            ..default()
        });
        app.add_plugins(bevy_tweening::TweeningPlugin);
        app.add_plugins(CambioPlugin);
        app.add_plugins(ClientPlugin);

        app.add_plugins((GameAssetPlugin, CardPlugin));
    }

    app.run();
}
