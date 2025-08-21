use std::{net::UdpSocket, time::SystemTime};

use bevy::{platform::collections::HashMap, prelude::*, window::PrimaryWindow};
use bevy_renet::{
    RenetClientPlugin, client_connected,
    netcode::{ClientAuthentication, NetcodeClientPlugin, NetcodeClientTransport},
    renet::{ClientId, ConnectionConfig, DefaultChannel, RenetClient},
};

use crate::cambio::*;
use crate::cards::*;
use crate::messages::*;

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
            (client_sync_players, set_and_publish_cursor_position.chain()).run_if(client_connected),
        );

        app.add_systems(Update, sync_held_by);
    }
}

fn init_state(client_id: ClientId, world: &mut World) -> ClientState {
    let table_card = world
        .spawn((
            SomeCard::default(),
            CardId::StackCard,
            Name::new("Table Card"),
        ))
        .observe(click_card)
        .id();

    return ClientState {
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
    mut players: Query<&mut PlayerState>,
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
                debug_assert!(
                    !state.game.players.contains_key(&client_id),
                    "There can only be one player per client",
                );

                let player_entity = commands
                    .spawn((
                        PlayerState {
                            last_mouse_pos_world: Vec2::ZERO,
                            player_id,
                        },
                    ))
                    .with_children(|parent| {
                        //parent
                        //    .spawn((CardSlot, Transform::from_xyz(0.0, 0.0, 0.0)))
                        //    .observe(click_slot);
                        //parent
                        //    .spawn((
                        //        CardSlot,
                        //        Transform::from_xyz(100.0, 0.0, 0.0),
                        //        Name::new("Slot 2"),
                        //    ))
                        //    .observe(click_slot)
                        //    .with_children(|parent| {});
                    })
                    .id();

                state.game.players.insert(client_id, player_entity);
            }
            ServerMessage::PlayerDisconnected { client_id } => {
                println!("Player {} disconnected.", client_id);
            }
            ServerMessage::StateUpdate {
                claimer_id: client_id,
                action,
            } => {
                println!("Got verified claim that {} did {:?}", client_id, action);
                match action {
                    CambioAction::PickUpCard { card } => {
                        let card_entity = state.game.get_card(card);
                        commands.entity(card_entity).insert(IsHeldBy(client_id));
                    }
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
            ServerMessageUnreliable::MousePositions(items) => {
                for (client_id, mouse_pos) in items {
                    if state.client_id == client_id {
                        // We use the local mouse position to reduce
                        // visual latency
                        continue;
                    }

                    if let Ok(mut player_state) = players.get_mut(state.game.players[&client_id]) {
                        player_state.last_mouse_pos_world = mouse_pos;
                    }
                }
            }
        }
    }
}

fn set_and_publish_cursor_position(
    mut client: ResMut<RenetClient>,
    state: ResMut<ClientState>,
    mut players: Query<&mut PlayerState>,
    window: Single<&Window, With<PrimaryWindow>>,
    camera: Single<(&Camera, &GlobalTransform)>,
) {
    let me = state.game.players.get(&state.client_id).unwrap();
    if let Some(cpos) = window.cursor_position() {
        if let Ok(world_pos) = camera.0.viewport_to_world_2d(camera.1, cpos) {
            if let Ok(mut me) = players.get_mut(*me) {
                me.last_mouse_pos_world = world_pos;
                client.send_claim_unreliable(ClientClaimUnreliable::MousePosition(world_pos));
            }
        }
    }
}

fn sync_held_by(
    state: Res<ClientState>,
    mut held: Query<(&mut Transform, &IsHeldBy)>,
    players: Query<&PlayerState>,
) {
    for (mut transform, held_by) in held.iter_mut() {
        if let Some(player_entity) = state.game.players.get(&held_by.0) {
            if let Ok(player_state) = players.get(*player_entity) {
                transform.translation.x = player_state.last_mouse_pos_world.x;
                transform.translation.y = player_state.last_mouse_pos_world.y;
            }
        }
    }
}

fn click_card(
    trigger: Trigger<Pointer<Click>>,
    cards: Query<&CardId>,
    held: Query<&IsHeldBy>,
    mut client: ResMut<RenetClient>,
) {
    if let Ok(card) = cards.get(trigger.target) {
        if !held.contains(trigger.target) {
            let claim = CambioAction::PickUpCard { card: *card };
            client.send_claim(claim);
        } else {
            println!("Card already held");
        }
    }
}

fn click_slot(
    mut trigger: Trigger<Pointer<Click>>,
    slots: Query<&CardSlot>,
    held: Query<&IsHeldBy>,
    mut client: ResMut<RenetClient>,
) {
    if let Ok(slot) = slots.get(trigger.target) {
        if !held.is_empty() {
            println!("Clicked slot");
        }
    }
}
