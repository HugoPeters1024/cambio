use std::{net::UdpSocket, time::SystemTime};

use bevy::{
    platform::collections::{HashMap, HashSet},
    prelude::*,
};
use bevy_renet::{
    RenetServerPlugin,
    netcode::{NetcodeServerPlugin, NetcodeServerTransport, ServerAuthentication, ServerConfig},
    renet::{ClientId, ConnectionConfig, DefaultChannel, RenetServer, ServerEvent},
};

use crate::messages::*;

#[derive(Default, Resource)]
struct ServerState {
    mouse_positions: HashMap<ClientId, Vec2>,
}

#[derive(Default, Resource)]
struct Lobby {
    players: HashSet<ClientId>,
}

pub struct ServerPlugin;

impl Plugin for ServerPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(RenetServerPlugin);
        app.add_plugins(NetcodeServerPlugin);
        let (server, transport) = new_renet_server();
        app.insert_resource(server);
        app.insert_resource(transport);
        app.init_resource::<ServerState>();
        app.init_resource::<Lobby>();

        app.add_systems(Update, (server_update_system, server_sync_clients).chain());
    }
}

fn new_renet_server() -> (RenetServer, NetcodeServerTransport) {
    const PROTOCOL_ID: u64 = 7;
    let public_addr = "127.0.0.1:5000".parse().unwrap();
    let socket = UdpSocket::bind(public_addr).unwrap();
    let current_time = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap();
    let server_config = ServerConfig {
        current_time,
        max_clients: 64,
        protocol_id: PROTOCOL_ID,
        public_addresses: vec![public_addr],
        authentication: ServerAuthentication::Unsecure,
    };

    let transport = NetcodeServerTransport::new(server_config, socket).unwrap();
    let server = RenetServer::new(ConnectionConfig::default());

    (server, transport)
}

fn server_update_system(
    mut server_events: EventReader<ServerEvent>,
    mut server: ResMut<RenetServer>,
    mut lobby: ResMut<Lobby>,
    mut state: ResMut<ServerState>,
) {
    for event in server_events.read() {
        match event {
            ServerEvent::ClientConnected { client_id } => {
                println!("Player {} connected.", client_id);

                // We could send an InitState with all the players id and positions for the client
                // but this is easier to do.
                for &player_id in lobby.players.iter() {
                    let message = bincode::serde::encode_to_vec(
                        &ServerMessages::PlayerConnected { id: player_id },
                        bincode::config::standard(),
                    )
                    .unwrap();
                    server.send_message(*client_id, DefaultChannel::ReliableOrdered, message);
                }

                lobby.players.insert(*client_id);

                let message = bincode::serde::encode_to_vec(
                    &ServerMessages::PlayerConnected { id: *client_id },
                    bincode::config::standard(),
                )
                .unwrap();
                server.broadcast_message(DefaultChannel::ReliableOrdered, message);
            }
            ServerEvent::ClientDisconnected { client_id, reason } => {
                println!("Player {} disconnected: {}", client_id, reason);

                let message = bincode::serde::encode_to_vec(
                    &ServerMessages::PlayerDisconnected { id: *client_id },
                    bincode::config::standard(),
                )
                .unwrap();

                lobby.players.remove(client_id);

                server.broadcast_message(DefaultChannel::ReliableOrdered, message);
            }
        }
    }

    for client_id in server.clients_id() {
        while let Some(message) = server.receive_message(client_id, DefaultChannel::Unreliable) {
            let claim: ClientClaimUnreliable =
                bincode::serde::decode_from_slice(&message, bincode::config::standard())
                    .unwrap()
                    .0;
            match claim {
                ClientClaimUnreliable::MousePosition(vec2) => {
                    state.mouse_positions.insert(client_id, vec2);
                }
            }
        }
    }
}

fn server_sync_clients(mut server: ResMut<RenetServer>, state: Res<ServerState>) {
    let message = ServerMessageUnreliable::MousePositions(state.mouse_positions.clone());
    let message = bincode::serde::encode_to_vec(&message, bincode::config::standard()).unwrap();
    server.broadcast_message(DefaultChannel::Unreliable, message);
}
