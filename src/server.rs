use std::{net::UdpSocket, time::SystemTime};

use bevy::{platform::collections::HashMap, prelude::*};
use bevy_renet::{
    RenetServerPlugin,
    netcode::{NetcodeServerPlugin, NetcodeServerTransport, ServerAuthentication, ServerConfig},
    renet::{ClientId, ConnectionConfig, DefaultChannel, RenetServer, ServerEvent},
};

use crate::cambio::*;
use crate::cards::*;
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

#[derive(Resource)]
struct ServerState {
    game: CambioState,
}

pub struct ServerPlugin;

impl Plugin for ServerPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(RenetServerPlugin);
        app.add_plugins(NetcodeServerPlugin);
        let (server, transport) = new_renet_server();
        app.insert_resource(server);
        app.insert_resource(transport);

        let table_card = app.world_mut().spawn(Card::default()).id();

        app.insert_resource(ServerState {
            game: CambioState {
                table_card,
                players: HashMap::default(),
            },
        });

        app.add_systems(Update, (server_update_system).chain());
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
    mut commands: Commands,
    mut server_events: EventReader<ServerEvent>,
    mut server: ResMut<RenetServer>,
    mut state: ResMut<ServerState>,
    players: Query<&CambioPlayerState>,
    held: Query<&IsHeldBy>,
    mut player_seq: Local<usize>,
) {
    for event in server_events.read() {
        match event {
            ServerEvent::ClientConnected { client_id } => {
                println!("Player {} connected.", client_id);

                // We could send an InitState with all the players id and positions for the client
                // but this is easier to do.
                for (&existing_client, player) in state.game.players.iter() {
                    let player = players.get(*player).expect("Player has no PlayerState");
                    server.send_message_typed(
                        *client_id,
                        ServerMessage::PlayerConnected {
                            client_id: existing_client,
                            player_idx: player.player_id,
                        },
                    );
                }

                *player_seq += 1;
                let new_player_idx = *player_seq;

                let new_player = commands
                    .spawn(CambioPlayerState {
                        player_id: PlayerId(new_player_idx),
                        cards: vec![],
                    })
                    .id();

                state.game.players.insert(*client_id, new_player);

                server.broadcast_message_typed(ServerMessage::PlayerConnected {
                    client_id: *client_id,
                    player_idx: PlayerId(new_player_idx),
                });
            }
            ServerEvent::ClientDisconnected { client_id, reason } => {
                println!("Player {} disconnected: {}", client_id, reason);
                state.game.players.remove(client_id);
                server
                    .broadcast_message_typed(ServerMessage::PlayerDisconnected { client_id: *client_id });
            }
        }
    }

    for client_id in server.clients_id() {
        while let Some(message) = server.receive_message(client_id, DefaultChannel::ReliableOrdered)
        {
            let Some(claimer) = state.game.players.get(&client_id) else {
                // Ignore messages from unknown clients
                println!("Got message from unknown client: {}", client_id);
                continue;
            };

            let claim: CambioAction =
                bincode::serde::decode_from_slice(&message, bincode::config::standard())
                    .unwrap()
                    .0;
            println!("Got claim from {}: {:?}", client_id, claim);

            match claim {
                CambioAction::PickUpCard { card } => {
                    let card_entity = state.game.get_card(card);
                    let held_by = held.get(card_entity);
                    if held_by.is_err() {
                        commands.entity(card_entity).insert(IsHeldBy(*claimer));
                        server.broadcast_message_typed(ServerMessage::StateUpdate {
                            client_id,
                            action: claim,
                        });
                    }
                }
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
                    //state.players.entry(client_id).and_modify(|player_state| {
                    //    player_state.mouse_pos = vec2;
                    //});
                }
            }
        }
    }
}
