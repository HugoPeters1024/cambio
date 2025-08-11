use bevy::prelude::*;
use bevy::window::WindowResolution;
use bevy::winit::UpdateMode;
use bevy::winit::WinitSettings;
use bevy_renet::netcode::NetcodeTransportError;

mod assets;
mod cards;
mod client;
mod messages;
mod server;

use crate::assets::*;
use crate::cards::*;
use crate::client::ClientPlugin;
use crate::server::ServerPlugin;

fn main() {
    println!("Usage: run with \"server\" or \"client\" argument");
    let args: Vec<String> = std::env::args().collect();

    let exec_type = &args[1];
    let is_host = match exec_type.as_str() {
        "client" => false,
        "server" => true,
        _ => panic!("Invalid argument, must be \"client\" or \"server\"."),
    };

    let mut app = App::new();

    if is_host {
        app.add_plugins(MinimalPlugins);
        app.add_plugins(ServerPlugin);
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
        app.add_plugins(ClientPlugin);

        app.add_plugins((GameAssetPlugin, CardPlugin));

        app.add_systems(OnEnter(GameState::Playing), setup);
    }

    app.add_systems(Update, panic_on_error_system);
    app.run();
}

fn setup(mut commands: Commands) {
    commands.spawn((
        Camera2d,
        Projection::Orthographic(OrthographicProjection {
            scaling_mode: bevy::render::camera::ScalingMode::FixedVertical {
                viewport_height: 800.0,
            },
            ..OrthographicProjection::default_2d()
        }),
    ));
}

// If any error is found we just panic
#[allow(clippy::never_loop)]
fn panic_on_error_system(mut renet_error: EventReader<NetcodeTransportError>) {
    for e in renet_error.read() {
        panic!("{}", e);
    }
}
