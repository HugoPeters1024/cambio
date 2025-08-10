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

#[derive(Default, Resource)]
struct Lobby {
    players: HashMap<ClientId, Entity>,
}

pub struct ClientPlugin;

impl Plugin for ClientPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(RenetClientPlugin);
        app.add_plugins(NetcodeClientPlugin);
        let (client, transport) = new_renet_client();
        app.insert_resource(client);
        app.insert_resource(transport);

        app.init_resource::<Lobby>();

        app.add_systems(
            Update,
            (client_sync_players, publish_cursor).run_if(client_connected),
        );
    }
}

fn new_renet_client() -> (RenetClient, NetcodeClientTransport) {
    const PROTOCOL_ID: u64 = 7;
    let server_addr = "127.0.0.1:5000".parse().unwrap();
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
    mut lobby: ResMut<Lobby>,
    mut transforms: Query<&mut Transform>,
    transport: Res<NetcodeClientTransport>,
) {
    while let Some(message) = client.receive_message(DefaultChannel::ReliableOrdered) {
        let server_message =
            bincode::serde::decode_from_slice(&message, bincode::config::standard())
                .unwrap()
                .0;
        match server_message {
            ServerMessages::PlayerConnected { id } => {
                println!("Player {} connected.", id);
                let player_entity = commands
                    .spawn(Card {
                        suit: Suit::Diamonds,
                        rank: Rank::Five,
                    })
                    .id();

                lobby.players.insert(id, player_entity);
            }
            ServerMessages::PlayerDisconnected { id } => {
                println!("Player {} disconnected.", id);
                if let Some(player_entity) = lobby.players.remove(&id) {
                    commands.entity(player_entity).despawn();
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
                    if client_id == &transport.client_id() {
                        // These are purely visual and logically unimportant values, we prefer
                        // our local state.
                        continue;
                    }

                    if let Some(target_entity) = lobby.players.get(client_id) {
                        if let Ok(mut transform) = transforms.get_mut(*target_entity) {
                            transform.translation.x = cpos.x;
                            transform.translation.y = cpos.y;
                        }
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
