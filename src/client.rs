use std::{net::UdpSocket, time::SystemTime};

use bevy::{platform::collections::HashMap, prelude::*, window::PrimaryWindow};
use bevy_renet::{
    RenetClientPlugin, client_connected,
    netcode::{ClientAuthentication, NetcodeClientPlugin, NetcodeClientTransport},
    renet::{ClientId, ConnectionConfig, DefaultChannel, RenetClient},
};

use crate::{
    cambio::{CambioAction, CambioPlayerState, CambioState, CardId},
    cards::Card,
    messages::*,
};

pub trait ClientExt {
    fn send_claim(&mut self, claim: CambioAction);

    fn send_claim_unreliable(&mut self, claim: ClientClaimUnreliable);
}

impl ClientExt for RenetClient {
    fn send_claim(&mut self, claim: CambioAction) {
        let encoded = bincode::serde::encode_to_vec(&claim, bincode::config::standard()).unwrap();
        self.send_message(DefaultChannel::ReliableOrdered, encoded);
    }

    fn send_claim_unreliable(&mut self, claim: ClientClaimUnreliable) {
        let encoded = bincode::serde::encode_to_vec(&claim, bincode::config::standard()).unwrap();
        self.send_message(DefaultChannel::Unreliable, encoded);
    }
}

#[derive(Resource)]
pub struct ClientState {
    pub client_id: ClientId,
    pub me: Option<Entity>,
    pub game: CambioState,
}

pub struct ClientPlugin;

impl Plugin for ClientPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(RenetClientPlugin);
        app.add_plugins(NetcodeClientPlugin);
        let (client, transport) = new_renet_client();
        let client_id = transport.client_id();
        app.insert_resource(client);
        app.insert_resource(transport);

        let initial_state = init_state(client_id, app.world_mut());
        app.insert_resource(initial_state);

        app.add_systems(
            PreUpdate,
            (client_sync_players, publish_cursor).run_if(client_connected),
        );
    }
}

fn init_state(client_id: ClientId, world: &mut World) -> ClientState {
    let table_card = world.spawn((Card::default(), CardId::StackCard)).id();
    return ClientState {
        me: None,
        client_id,
        game: CambioState {
            table_card,
            players: HashMap::default(),
        },
    };
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
            ServerMessage::PlayerConnected {
                client_id,
                player_idx: player_id,
            } => {
                println!("Player {} connected.", player_id.0);

                let player_entity = commands
                    .spawn(CambioPlayerState {
                        cards: vec![],
                        player_id,
                    })
                    .id();

                if client_id == state.client_id {
                    debug_assert!(state.me.is_none(), "There can only be one me");
                    state.me = Some(player_entity);
                }

                state.game.players.insert(client_id, player_entity);
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
        match message {}
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
