use std::{net::UdpSocket, time::SystemTime};

use bevy::{platform::collections::HashMap, prelude::*, window::PrimaryWindow};
use bevy_renet::{
    RenetClientPlugin, client_connected,
    netcode::{ClientAuthentication, NetcodeClientPlugin, NetcodeClientTransport},
    renet::{ClientId, ConnectionConfig, DefaultChannel, RenetClient},
};

use crate::cards::*;
use crate::messages::*;

pub trait ClientExt {
    fn send_claim(&mut self, claim: ClientClaim);

    fn send_claim_unreliable(&mut self, claim: ClientClaimUnreliable);
}

impl ClientExt for RenetClient {
    fn send_claim(&mut self, claim: ClientClaim) {
        let encoded = bincode::serde::encode_to_vec(&claim, bincode::config::standard()).unwrap();
        self.send_message(DefaultChannel::ReliableOrdered, encoded);
    }

    fn send_claim_unreliable(&mut self, claim: ClientClaimUnreliable) {
        let encoded = bincode::serde::encode_to_vec(&claim, bincode::config::standard()).unwrap();
        self.send_message(DefaultChannel::Unreliable, encoded);
    }
}

#[derive(Debug, Component)]
pub struct IsPickedUp(pub ClientId);

pub struct PlayerState {
    root: Entity,
    mouse_pos: Vec2,
}

#[derive(Default, Resource)]
pub struct ClientState {
    players: HashMap<ClientId, PlayerState>,
}

pub struct ClientPlugin;

impl Plugin for ClientPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(RenetClientPlugin);
        app.add_plugins(NetcodeClientPlugin);
        let (client, transport) = new_renet_client();
        app.insert_resource(client);
        app.insert_resource(transport);

        app.init_resource::<ClientState>();

        app.add_systems(
            Update,
            (client_sync_players, publish_cursor, sync_dragging).run_if(client_connected),
        );
    }
}

fn new_renet_client() -> (RenetClient, NetcodeClientTransport) {
    const PROTOCOL_ID: u64 = 7;
    let server_addr = "127.0.0.1:9000".parse().unwrap();
    let socket = UdpSocket::bind("127.0.0.1:0").unwrap();
    let current_time = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap();
    let client_id = current_time.as_millis() as u64;
    let authentication = ClientAuthentication::Unsecure {
        client_id,
        protocol_id: PROTOCOL_ID,
        server_addr,
        user_data: None,
    };

    let transport = NetcodeClientTransport::new(current_time, authentication, socket).unwrap();
    let client = RenetClient::new(ConnectionConfig::default());

    (client, transport)
}

fn client_sync_players(
    mut commands: Commands,
    mut client: ResMut<RenetClient>,
    mut lobby: ResMut<ClientState>,
) {
    while let Some(message) = client.receive_message(DefaultChannel::ReliableOrdered) {
        let server_message =
            bincode::serde::decode_from_slice(&message, bincode::config::standard())
                .unwrap()
                .0;
        match server_message {
            ServerMessage::PlayerConnected { id } => {
                println!("Player {} connected.", id);
                let player_entity = commands
                    .spawn((
                        Card {
                            suit: Suit::Diamonds,
                            rank: Rank::Five,
                        },
                        CardRef { client_id: id },
                    ))
                    .id();

                lobby.players.insert(
                    id,
                    PlayerState {
                        root: player_entity,
                        mouse_pos: Vec2::ZERO,
                    },
                );
            }
            ServerMessage::PlayerDisconnected { id } => {
                println!("Player {} disconnected.", id);
                if let Some(player_state) = lobby.players.remove(&id) {
                    commands.entity(player_state.root).despawn();
                }
            }
            ServerMessage::CardPickedUp { id, card } => {
                println!("Player {} picked up card {:?}.", id, card);
                if let Some(player_state) = lobby.players.get_mut(&id) {
                    commands.entity(player_state.root).insert(IsPickedUp(id));
                }
            }
            ServerMessage::CardDropped { id } => {
                println!("Player {} dropped its card.", id);
                if let Some(player_state) = lobby.players.get_mut(&id) {
                    commands.entity(player_state.root).remove::<IsPickedUp>();
                }
            }
        }
    }

    while let Some(message) = client.receive_message(DefaultChannel::Unreliable) {
        let message: ServerMessageUnreliable =
            bincode::serde::decode_from_slice(&message, bincode::config::standard())
                .unwrap()
                .0;
        match message {
            ServerMessageUnreliable::MousePositions(hash_map) => {
                for (client_id, cpos) in hash_map.iter() {
                    if let Some(player_state) = lobby.players.get_mut(client_id) {
                        player_state.mouse_pos = *cpos;
                    }
                }
            }
        }
    }
}

fn publish_cursor(
    window: Single<&Window, With<PrimaryWindow>>,
    mut client: ResMut<RenetClient>,
    camera: Single<(&Camera, &GlobalTransform)>,
) {
    if let Some(cpos) = window.cursor_position() {
        if let Ok(world_pos) = camera.0.viewport_to_world_2d(camera.1, cpos) {
            client.send_claim_unreliable(ClientClaimUnreliable::MousePosition(world_pos));
        }
    }
}

fn sync_dragging(mut query: Query<(&mut Transform, &IsPickedUp)>, state: Res<ClientState>) {
    for (mut transform, picked_up) in query.iter_mut() {
        if let Some(player_state) = state.players.get(&picked_up.0) {
            transform.translation.x = player_state.mouse_pos.x;
            transform.translation.y = player_state.mouse_pos.y;
        }
    }
}
