use std::{net::UdpSocket, time::SystemTime};

use bevy::prelude::*;
use bevy_renet::{
    RenetServerPlugin,
    netcode::{NetcodeServerPlugin, NetcodeServerTransport, ServerAuthentication, ServerConfig},
    renet::{ClientId, ConnectionConfig, DefaultChannel, RenetServer, ServerEvent},
};

use crate::cambio::*;
use crate::messages::*;

trait ServerExt {
    fn broadcast_message_typed(&mut self, message: ServerMessage);

    fn send_message_typed(&mut self, client_id: ClientId, message: ServerMessage);
}

impl ServerExt for RenetServer {
    fn broadcast_message_typed(&mut self, message: ServerMessage) {
        let encoded = bincode::serde::encode_to_vec(message, bincode::config::standard()).unwrap();
        self.broadcast_message(DefaultChannel::ReliableOrdered, encoded);
    }

    fn send_message_typed(&mut self, client_id: ClientId, message: ServerMessage) {
        let encoded = bincode::serde::encode_to_vec(message, bincode::config::standard()).unwrap();
        self.send_message(client_id, DefaultChannel::ReliableOrdered, encoded);
    }
}

pub struct ServerPlugin;

impl Plugin for ServerPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(RenetServerPlugin);
        app.add_plugins(NetcodeServerPlugin);
        let (server, transport) = new_renet_server();
        app.insert_resource(server);
        app.insert_resource(transport);

        app.add_systems(
            Update,
            (
                server_update_system,
                broadcast_validated_updates.after(process_message),
            )
                .chain(),
        );
    }
}

fn new_renet_server() -> (RenetServer, NetcodeServerTransport) {
    const PROTOCOL_ID: u64 = 7;
    let public_addr = "127.0.0.1:9000".parse().unwrap();
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
    state: ResMut<CambioState>,
    mut players: Query<(&PlayerId, &mut PlayerState)>,
    mut player_seq: Local<u8>,
    mut to_validate: EventWriter<IncomingMessage>,
) {
    for event in server_events.read() {
        match event {
            ServerEvent::ClientConnected { client_id } => {
                println!("Player {} connected.", client_id);

                // We could send an InitState with all the players id and positions for the client
                // but this is easier to do.
                for &existing_client in state.player_index.keys() {
                    server.send_message_typed(
                        *client_id,
                        ServerMessage::PlayerConnected {
                            player_id: existing_client,
                        },
                    );
                }

                *player_seq += 1;

                let new_player_id = PlayerId {
                    player_number: *player_seq,
                    client_id: *client_id,
                };

                to_validate.write(IncomingMessage(ServerMessage::PlayerConnected {
                    player_id: new_player_id,
                }));
            }
            ServerEvent::ClientDisconnected { client_id, .. } => {
                for player in state.player_index.keys() {
                    if player.client_id == *client_id {
                        to_validate.write(IncomingMessage(ServerMessage::PlayerDisconnected {
                            player_id: *player,
                        }));
                    }
                }
            }
        }
    }

    for claimer_id in state.player_index.keys() {
        while let Some(message) =
            server.receive_message(claimer_id.client_id, DefaultChannel::ReliableOrdered)
        {
            let claim: CambioAction =
                bincode::serde::decode_from_slice(&message, bincode::config::standard())
                    .unwrap()
                    .0;

            to_validate.write(IncomingMessage(ServerMessage::StateUpdate {
                claimer_id: *claimer_id,
                action: claim,
            }));
        }
    }

    for (player_id, player_entity) in state.player_index.iter() {
        while let Some(message) =
            server.receive_message(player_id.client_id, DefaultChannel::Unreliable)
        {
            let claim: ClientClaimUnreliable =
                bincode::serde::decode_from_slice(&message, bincode::config::standard())
                    .unwrap()
                    .0;

            match claim {
                ClientClaimUnreliable::MousePosition(mouse_pos) => {
                    if let Ok((_, mut player_state)) = players.get_mut(*player_entity) {
                        player_state.last_mouse_pos_world = mouse_pos;
                    }
                }
            }
        }
    }

    let mouse_update = ServerMessageUnreliable::MousePositions(
        state
            .player_index
            .iter()
            .filter_map(|(&id, player)| {
                players
                    .get(*player)
                    .ok()
                    .map(|p| (id, p.1.last_mouse_pos_world))
            })
            .collect(),
    );

    server.broadcast_message(
        DefaultChannel::Unreliable,
        bincode::serde::encode_to_vec(mouse_update, bincode::config::standard()).unwrap(),
    );
}

fn broadcast_validated_updates(
    mut validated: EventReader<ProcessedMessage>,
    mut server: ResMut<RenetServer>,
) {
    for ProcessedMessage(message) in validated.read() {
        server.broadcast_message_typed(message.clone());
    }
}
