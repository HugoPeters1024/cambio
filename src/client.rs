use std::{net::UdpSocket, time::SystemTime};

use bevy::{prelude::*, window::PrimaryWindow};
use bevy_renet::{
    RenetClientPlugin, client_connected,
    netcode::{ClientAuthentication, NetcodeClientPlugin, NetcodeClientTransport},
    renet::{ConnectionConfig, DefaultChannel, RenetClient},
};

use crate::assets::*;
use crate::cambio::*;
use crate::messages::*;
use crate::{PlayerIdxText, cards::*};

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

pub struct ClientPlugin;

impl Plugin for ClientPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(RenetClientPlugin);
        app.add_plugins(NetcodeClientPlugin);
        let (client, transport) = new_renet_client();
        app.insert_resource(client);
        app.insert_resource(transport);

        app.add_systems(
            PostUpdate,
            ((
                client_sync_players,
                set_and_publish_cursor_position,
                sync_held_by,
            )
                .chain())
            .run_if(client_connected),
        );

        fn on_player_spawn(
            trigger: Trigger<OnAdd, PlayerId>,
            player_ids: Query<&PlayerId>,
            transport: Res<NetcodeClientTransport>,
            mut commands: Commands,
        ) {
            let player_id = *player_ids.get(trigger.target()).unwrap();

            // Mark this player as the local player
            if player_id.client_id == transport.client_id() {
                commands.entity(trigger.target()).insert(MyPlayer);
            }

            let up_or_down = if player_id.player_index <= 2 {
                1.0
            } else {
                -1.0
            };

            commands.entity(trigger.target()).with_child((
                Text2d::new(format!("Player {}", player_id.player_number())),
                TextFont::from_font_size(14.0),
                Transform::from_xyz(0.0, (DESIRED_CARD_HEIGHT + 15.0) * up_or_down, 1.0),
                TextColor(Color::WHITE),
            ));
        }

        app.add_observer(on_player_spawn);
        app.add_observer(click_card);
        app.add_observer(click_slot);

        app.add_systems(OnEnter(GamePhase::Playing), setup);
        app.add_systems(Update, update_player_idx_text);
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

fn setup(mut commands: Commands, state: Res<CambioState>, assets: Res<GameAssets>) {
    commands.spawn((
        Camera2d,
        Projection::Orthographic(OrthographicProjection {
            scaling_mode: bevy::render::camera::ScalingMode::FixedVertical {
                viewport_height: 480.0,
            },
            ..OrthographicProjection::default_2d()
        }),
    ));

    commands.spawn((Text::from("You are player: ..."), PlayerIdxText));

    commands
        .entity(state.root)
        .with_child((
            Sprite::from_image(assets.reveal_sprite.clone()),
            Transform::from_xyz(90.0, 0.0, 0.0),
            Pickable::default(),
        ))
        .observe(click_reveal_button);
}

fn update_player_idx_text(
    me: Single<&PlayerId, With<MyPlayer>>,
    mut text: Single<&mut Text, With<PlayerIdxText>>,
) {
    text.0 = format!("You are player: {}", me.player_number());
}

fn client_sync_players(
    mut bus: ResMut<MessageBus>,
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

        bus.incoming
            .push_back(IncomingMessage(server_message.clone()));
    }

    while let Some(message) = client.receive_message(DefaultChannel::Unreliable) {
        let message: ServerMessageUnreliable =
            bincode::serde::decode_from_slice(&message, bincode::config::standard())
                .unwrap()
                .0;
        match message {
            ServerMessageUnreliable::MousePositions(items) => {
                for (player_id, mouse_pos) in items {
                    if state.player_index.get(&player_id).is_none() {
                        continue;
                    }
                    if me.contains(state.player_index[&player_id]) {
                        // We use the local mouse position to reduce
                        // visual latency
                        continue;
                    }

                    if let Ok(mut player_state) = players.get_mut(state.player_index[&player_id]) {
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
        if let Some(player_entity) = state.player_index.get(&held_by.0) {
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
        if !held.contains(trigger.target()) {
            client.send_claim(ClientClaim::PickUpCard { card_id: *card });
            trigger.propagate(false);
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
        if let Some((holding_card, _)) = holders.iter().filter(|h| h.1.0 == **me).next() {
            if children
                .iter_descendants(slot_entity)
                .filter(|c| cards.contains(*c))
                .next()
                .is_none()
            {
                let card_id = *cards.get(holding_card).unwrap();
                client.send_claim(ClientClaim::DropCardOnSlot {
                    card_id: card_id,
                    slot_id: *slot_id,
                });
                trigger.propagate(false);
            }
        }
    }
}

fn click_reveal_button(
    mut trigger: Trigger<Pointer<Click>>,
    mut client: ResMut<RenetClient>,
    cards: Query<&CardId>,
    me: Single<&PlayerId, With<MyPlayer>>,
    holders: Query<(Entity, &IsHeldBy)>,
) {
    if let Some((holding_card, _)) = holders.iter().filter(|h| h.1.0 == **me).next() {
        let card_id = *cards.get(holding_card).unwrap();
        client.send_claim(ClientClaim::LookAtCard { card_id });
        trigger.propagate(false);
    }
}
