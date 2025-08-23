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

pub struct ClientPlugin;

impl Plugin for ClientPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(RenetClientPlugin);
        app.add_plugins(NetcodeClientPlugin);
        let (client, transport) = new_renet_client();
        app.insert_resource(client);
        app.insert_resource(transport);
        app.init_resource::<CambioState>();

        app.add_systems(
            PreUpdate,
            (client_sync_players, set_and_publish_cursor_position.chain())
                .run_if(client_connected)
                .after(process_message),
        );

        app.add_systems(Update, sync_held_by);

        fn check_if_i_spawned(
            trigger: Trigger<OnAdd, PlayerId>,
            player_ids: Query<&PlayerId>,
            transport: Res<NetcodeClientTransport>,
            mut commands: Commands,
        ) {
            let player_id = *player_ids.get(trigger.target()).unwrap();
            if player_id.client_id == transport.client_id() {
                println!("I spawned!");
                commands.entity(trigger.target()).insert(MyPlayer);
            }
        }

        app.add_observer(check_if_i_spawned);
        app.add_observer(click_card);
        app.add_observer(click_slot);
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
    mut writer: EventWriter<IncomingMessage>,
    mut client: ResMut<RenetClient>,
    state: Res<CambioState>,
    mut players: Query<&mut PlayerState>,
    me: Query<&MyPlayer>,
) {
    while let Some(message) = client.receive_message(DefaultChannel::ReliableOrdered) {
        let server_message: ServerMessage =
            bincode::serde::decode_from_slice(&message, bincode::config::standard())
                .unwrap()
                .0;

        writer.write(IncomingMessage(server_message.clone()));
    }

    while let Some(message) = client.receive_message(DefaultChannel::Unreliable) {
        let message: ServerMessageUnreliable =
            bincode::serde::decode_from_slice(&message, bincode::config::standard())
                .unwrap()
                .0;
        match message {
            ServerMessageUnreliable::MousePositions(items) => {
                for (player_id, mouse_pos) in items {
                    if state.players.get(&player_id).is_none() {
                        continue;
                    }
                    if me.contains(state.players[&player_id]) {
                        // We use the local mouse position to reduce
                        // visual latency
                        continue;
                    }

                    if let Ok(mut player_state) = players.get_mut(state.players[&player_id]) {
                        player_state.last_mouse_pos_world = mouse_pos;
                    }
                }
            }
        }
    }
}

fn set_and_publish_cursor_position(
    mut client: ResMut<RenetClient>,
    mut me: Single<&mut PlayerState, With<MyPlayer>>,
    window: Single<&Window, With<PrimaryWindow>>,
    camera: Single<(&Camera, &GlobalTransform)>,
) {
    if let Some(cpos) = window.cursor_position() {
        if let Ok(world_pos) = camera.0.viewport_to_world_2d(camera.1, cpos) {
            me.last_mouse_pos_world = world_pos;
            client.send_claim_unreliable(ClientClaimUnreliable::MousePosition(world_pos));
        }
    }
}

fn sync_held_by(
    state: Res<CambioState>,
    mut held: Query<(&mut Transform, &IsHeldBy)>,
    players: Query<&PlayerState>,
) {
    for (mut transform, held_by) in held.iter_mut() {
        if let Some(player_entity) = state.players.get(&held_by.0) {
            if let Ok(player_state) = players.get(*player_entity) {
                transform.translation.x = player_state.last_mouse_pos_world.x;
                transform.translation.y = player_state.last_mouse_pos_world.y;
            }
        }
    }
}

fn click_card(
    mut trigger: Trigger<Pointer<Click>>,
    cards: Query<&CardId>,
    held: Query<&IsHeldBy>,
    mut client: ResMut<RenetClient>,
) {
    if let Ok(card) = cards.get(trigger.target()) {
        println!("Clicked card");
        if !held.contains(trigger.target()) {
            let claim = CambioAction::PickUpCard { card_id: *card };
            client.send_claim(claim);
            trigger.propagate(false);
        } else {
            println!("Card already held");
        }
    }
}

fn click_slot(
    mut trigger: Trigger<Pointer<Click>>,
    me: Single<&PlayerId, With<MyPlayer>>,
    slots: Query<(Entity, &SlotId), With<CardSlot>>,
    cards: Query<&CardId>,
    children: Query<&Children>,
    holders: Query<(Entity, &IsHeldBy)>,
    mut client: ResMut<RenetClient>,
) {
    if let Ok((slot_entity, slot_id)) = slots.get(trigger.target()) {
        println!("Clicked slot");
        if let Some((holding_card, _)) = holders.iter().filter(|h| h.1.0 == **me).next() {
            if children
                .iter_descendants(slot_entity)
                .filter(|c| cards.contains(*c))
                .next()
                .is_none()
            {
                let card_id = *cards.get(holding_card).unwrap();
                client.send_claim(CambioAction::DropCardOnSlot {
                    card_id: card_id,
                    slot_id: *slot_id,
                });
                trigger.propagate(false);
            }
        }
    }
}
