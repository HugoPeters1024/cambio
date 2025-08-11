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

use crate::{
    cards::{Card, CardRef},
    messages::*,
};

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

#[derive(Debug, Default)]
struct PlayerState {
    mouse_pos: Vec2,
    table_cards: Vec<Card>,
    is_dragging: Option<CardRef>,
}

#[derive(Default, Resource)]
struct ServerState {
    players: HashMap<ClientId, PlayerState>,
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

        app.add_systems(Update, (server_update_system, server_sync_clients).chain());
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
    mut state: ResMut<ServerState>,
) {
    for event in server_events.read() {
        match event {
            ServerEvent::ClientConnected { client_id } => {
                println!("Player {} connected.", client_id);

                // We could send an InitState with all the players id and positions for the client
                // but this is easier to do.
                for &player_id in state.players.keys() {
                    server.send_message_typed(
                        *client_id,
                        ServerMessage::PlayerConnected { id: player_id },
                    )
                }

                state.players.insert(*client_id, PlayerState::default());
                server.broadcast_message_typed(ServerMessage::PlayerConnected { id: *client_id });
            }
            ServerEvent::ClientDisconnected { client_id, reason } => {
                println!("Player {} disconnected: {}", client_id, reason);
                state.players.remove(client_id);
                server
                    .broadcast_message_typed(ServerMessage::PlayerDisconnected { id: *client_id });
            }
        }
    }

    for client_id in server.clients_id() {
        while let Some(message) = server.receive_message(client_id, DefaultChannel::ReliableOrdered)
        {
            let claim: ClientClaim =
                bincode::serde::decode_from_slice(&message, bincode::config::standard())
                    .unwrap()
                    .0;
            println!("Got claim from {}: {:?}", client_id, claim);
            handle_claim(&client_id, &claim, &mut state, &mut server);
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
                    state.players.entry(client_id).and_modify(|player_state| {
                        player_state.mouse_pos = vec2;
                    });
                }
            }
        }
    }
}

fn server_sync_clients(mut server: ResMut<RenetServer>, state: Res<ServerState>) {
    let message = ServerMessageUnreliable::MousePositions(
        state
            .players
            .iter()
            .map(|(id, state)| (*id, state.mouse_pos))
            .collect(),
    );
    let message = bincode::serde::encode_to_vec(&message, bincode::config::standard()).unwrap();
    server.broadcast_message(DefaultChannel::Unreliable, message);
}

fn handle_claim(
    client_id: &ClientId,
    claim: &ClientClaim,
    state: &mut ServerState,
    server: &mut RenetServer,
) {
    match claim {
        ClientClaim::ClickCard(card_ref) => {
            if let Some(player_state) = state.players.get_mut(client_id) {
                if player_state.is_dragging.is_some() {
                    player_state.is_dragging = None;
                    server.broadcast_message_typed(ServerMessage::CardDropped { id: *client_id });
                } else {
                    player_state.is_dragging = Some(*card_ref);

                    server.broadcast_message_typed(ServerMessage::CardPickedUp {
                        id: *client_id,
                        card: *card_ref,
                    });
                }
            }
        }
    }
}
