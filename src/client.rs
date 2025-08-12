use std::{net::UdpSocket, time::SystemTime};

use bevy::{platform::collections::HashMap, prelude::*, window::PrimaryWindow};
use bevy_renet::{
    RenetClientPlugin, client_connected,
    netcode::{ClientAuthentication, NetcodeClientPlugin, NetcodeClientTransport},
    renet::{ClientId, ConnectionConfig, DefaultChannel, RenetClient},
};

use crate::{
    cambio::{CambioPlayerState, CambioState},
    cards::Card,
    messages::*,
};

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

pub struct PlayerState {
    pub player_idx: usize,
    pub mouse_pos: Vec2,
}

#[derive(Resource)]
pub struct ClientState {
    pub me: ClientId,
    pub players: HashMap<ClientId, PlayerState>,
    pub game: CambioState,
}

pub struct ClientPlugin;

impl Plugin for ClientPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(RenetClientPlugin);
        app.add_plugins(NetcodeClientPlugin);
        let (client, transport) = new_renet_client();
        let me = transport.client_id();
        app.insert_resource(client);
        app.insert_resource(transport);

        let table_card = app.world_mut().spawn(Card::default()).id();

        app.insert_resource(ClientState {
            me,
            players: HashMap::default(),
            game: CambioState {
                table_card,
                players: vec![],
            },
        });

        app.add_systems(
            PreUpdate,
            (client_sync_players, publish_cursor).run_if(client_connected),
        );
        app.add_systems(Update, sync_render);
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
    mut state: ResMut<ClientState>,
) {
    while let Some(message) = client.receive_message(DefaultChannel::ReliableOrdered) {
        let server_message =
            bincode::serde::decode_from_slice(&message, bincode::config::standard())
                .unwrap()
                .0;
        match server_message {
            ServerMessage::PlayerConnected { id, player_idx } => {
                println!("Player {} connected.", player_idx);

                state.players.insert(
                    id,
                    PlayerState {
                        mouse_pos: Vec2::ZERO,
                        player_idx,
                    },
                );

                let card_entity = commands.spawn(Card::default()).id();

                let player_entity = commands
                    .spawn(CambioPlayerState {
                        cards: vec![card_entity],
                    })
                    .add_child(card_entity)
                    .id();
                state.game.players.push(player_entity);
            }
            ServerMessage::PlayerDisconnected { id } => {
                println!("Player {} disconnected.", id);
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
                    if let Some(player_state) = state.players.get_mut(client_id) {
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

fn sync_render(state: Res<ClientState>, mut query: Query<(&mut Transform, &CambioPlayerState)>) {
    // layout the players in a square
    for (idx, player) in state.game.players.iter().enumerate() {
        if let Ok((mut transform, _)) = query.get_mut(*player) {
            transform.translation.x = (idx % 2) as f32 * 100.0;
            transform.translation.y = (idx / 2) as f32 * 100.0;
        }
    }
}
