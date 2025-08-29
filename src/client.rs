use std::{
    net::UdpSocket,
    time::{Duration, SystemTime},
};

use bevy::{prelude::*, window::PrimaryWindow};
use bevy_renet::{
    RenetClientPlugin, client_connected,
    netcode::{ClientAuthentication, NetcodeClientPlugin, NetcodeClientTransport},
    renet::{ConnectionConfig, DefaultChannel, RenetClient},
};
use bevy_tweening::{Animator, RepeatCount, RepeatStrategy, Tween, lens::TransformPositionLens};

use crate::assets::*;
use crate::cambio::*;
use crate::messages::*;
use crate::{PlayerIdxText, cards::*};

#[derive(Component)]
struct PlayerTurnIcon;

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
            .run_if(client_connected)
            .run_if(in_state(GamePhase::Playing)),
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

            commands.entity(trigger.target()).with_child((
                Text2d::new(format!("Player {}", player_id.player_number())),
                TextFont::from_font_size(14.0),
                Transform::from_xyz(
                    0.0,
                    (DESIRED_CARD_HEIGHT + 15.0) * player_id.up_or_down(),
                    1.0,
                ),
                TextColor(Color::WHITE),
            ));
        }

        app.add_observer(on_player_spawn);
        app.add_observer(click_slot_card);
        app.add_observer(click_slot);
        app.add_observer(click_discard_pile);

        app.add_systems(OnEnter(GamePhase::Playing), setup);
        app.add_systems(Update, update_player_idx_text);
        app.add_systems(
            Update,
            on_message_accepted.run_if(in_state(GamePhase::Playing)),
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

fn setup(mut commands: Commands, state: Res<CambioState>, assets: Res<GameAssets>) {
    commands.add_observer(on_player_turn_added);
    commands.add_observer(on_player_turn_removed);

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

    let deck_of_cards = commands
        .spawn((
            Transform::from_xyz(-90.0, 0.0, 0.0),
            InheritedVisibility::default(),
            ChildOf(state.root),
            Pickable::default(),
        ))
        .observe(click_deck)
        .id();

    for x in 0..8 {
        commands.spawn((
            Sprite {
                custom_size: Some(Vec2::new(DESIRED_CARD_WIDTH, DESIRED_CARD_HEIGHT)),
                ..Sprite::from_image(assets.card_back.clone())
            },
            Transform::from_xyz(x as f32, (x % 3) as f32 - 1.0, 0.01 * x as f32)
                .with_rotation(Quat::from_rotation_z(x as f32 / 60.0)),
            Pickable::default(),
            ChildOf(deck_of_cards),
        ));
    }
}

fn update_player_idx_text(
    me: Single<&PlayerId, With<MyPlayer>>,
    turn_state: Query<&PlayerAtTurn, With<MyPlayer>>,
    extra: Query<&MayGiveCardTo, With<MyPlayer>>,
    mut text: Single<&mut Text, With<PlayerIdxText>>,
) {
    let mut turn_description = "".to_string();
    if let Some(player_at_turn) = turn_state.iter().next() {
        turn_description = match player_at_turn {
            PlayerAtTurn::Start => "Turn start",
            PlayerAtTurn::TookDeckCard => "You took a card from the deck",
            PlayerAtTurn::TookDiscardedCard => "You took a card from the discard pile",
            PlayerAtTurn::SwappedCard => "You swapped a card",
            PlayerAtTurn::Finished => "Turn end",
            PlayerAtTurn::HasBuff(buff) => match buff {
                TurnBuff::MayLookAtOwnCard => "Buff! You may look at one of your cards",
                TurnBuff::MayLookAtOtherPlayersCard => "Buff! You may look at someone else's cards",
                TurnBuff::MaySwapTwoCards { .. } => "Buff! You may swap two cards",
                TurnBuff::MayLookAtCardAndThenSwap { .. } => {
                    "Buff! You make look at a card and then swap it with another"
                }
            },
        }
        .to_string()
    };

    if let Some(may_give_card_to) = extra.iter().next() {
        turn_description = format!(
            "You may give a card to player {}!",
            may_give_card_to.0.player_number()
        );
    }

    text.0 = format!(
        "You are player: {} ({})",
        me.player_number(),
        turn_description
    );
}

fn client_sync_players(
    mut commands: Commands,
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

        commands.run_system_cached_with(process_single_event, server_message);
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

fn click_slot_card(
    mut trigger: Trigger<Pointer<Click>>,
    me: Single<&PlayerId, With<MyPlayer>>,
    cards: Query<&CardId>,
    held: Query<(Entity, &IsHeldBy)>,
    slot_ids: Query<&SlotId>,
    parents: Query<&ChildOf>,
    mut client: ResMut<RenetClient>,
) {
    if let Ok(card) = cards.get(trigger.target()) {
        if let Ok(ChildOf(parent_entity)) = parents.get(trigger.target()) {
            if let Ok(slot_id) = slot_ids.get(*parent_entity) {
                match trigger.event().button {
                    PointerButton::Primary => {
                        if let Some((held_card, _)) =
                            held.iter().find(|(_, held_by)| held_by.0 == **me)
                        {
                            let held_card_id = *cards.get(held_card).unwrap();
                            client.send_claim(ClientClaim::SwapHeldCardWithSlotCard {
                                slot_id: *slot_id,
                                held_card_id,
                            });
                        } else {
                            client.send_claim(ClientClaim::PickUpSlotCard {
                                slot_id: *slot_id,
                                card_id: *card,
                            });
                        }

                        trigger.propagate(false);
                    }
                    PointerButton::Secondary => {
                        client.send_claim(ClientClaim::LookAtCardAtSlot {
                            slot_id: *slot_id,
                            card_id: *card,
                        });

                        trigger.propagate(false);
                    }
                    _ => {}
                }
            }
        }
    }
}

fn click_discard_pile(
    trigger: Trigger<Pointer<Click>>,
    discard_pile: Query<Entity, With<DiscardPile>>,
    me: Single<&PlayerId, With<MyPlayer>>,
    held: Query<(Entity, &IsHeldBy, &CardId)>,
    mut client: ResMut<RenetClient>,
) {
    if !discard_pile.contains(trigger.target()) {
        return;
    };

    if let Some((_, _, &card_id)) = held.iter().find(|(_, held_by, _)| &held_by.0 == *me) {
        client.send_claim(ClientClaim::DropCardOnDiscardPile { card_id });
    } else {
        client.send_claim(ClientClaim::TakeCardFromDiscardPile);
    };
}

fn click_deck(_trigger: Trigger<Pointer<Click>>, mut client: ResMut<RenetClient>) {
    client.send_claim(ClientClaim::TakeFreshCardFromDeck);
}

fn click_slot(
    mut trigger: Trigger<Pointer<Click>>,
    me: Single<&PlayerId, With<MyPlayer>>,
    slots: Query<(Entity, &SlotId), With<CardSlot>>,
    cards: Query<&CardId>,
    holders: Query<(Entity, &IsHeldBy)>,
    mut client: ResMut<RenetClient>,
) {
    if let Ok((_, slot_id)) = slots.get(trigger.target()) {
        if let Some((holding_card, _)) = holders.iter().filter(|h| h.1.0 == **me).next() {
            let card_id = *cards.get(holding_card).unwrap();
            client.send_claim(ClientClaim::DropCardOnSlot {
                card_id: card_id,
                slot_id: *slot_id,
            });
            trigger.propagate(false);
        }
    }
}

fn on_player_turn_added(
    trigger: Trigger<OnAdd, PlayerAtTurn>,
    mut commands: Commands,
    players: Query<&PlayerId>,
    assets: Res<GameAssets>,
) {
    let Ok(player_id) = players.get(trigger.target()) else {
        return;
    };

    let up_down_tween = Tween::new(
        EaseFunction::QuadraticInOut,
        Duration::from_millis(500),
        TransformPositionLens {
            start: Vec3::new(0.0, 100.0 * player_id.up_or_down(), 0.0),
            end: Vec3::new(0.0, (110.0 + 15.0) * player_id.up_or_down(), 0.0),
        },
    )
    .with_repeat_strategy(RepeatStrategy::MirroredRepeat)
    .with_repeat_count(RepeatCount::Infinite);

    commands.entity(trigger.target()).with_child((
        PlayerTurnIcon,
        Transform::from_scale(Vec3::splat(0.1)).with_translation(Vec3::new(0.0, 0.0, 30.0)),
        Sprite::from_image(assets.arrow_down_sprite.clone()),
        Animator::new(up_down_tween),
        // drop shadow
        children![(
            Sprite {
                color: Color::linear_rgba(0.0, 0.0, 0.0, 1.0),
                ..Sprite::from_image(assets.arrow_down_sprite.clone())
            },
            Transform::from_xyz(5.0, -5.0, -1.0),
        )],
    ));
}

fn on_player_turn_removed(
    trigger: Trigger<OnRemove, PlayerAtTurn>,
    mut commands: Commands,
    children: Query<&Children>,
    icon: Query<Entity, With<PlayerTurnIcon>>,
) {
    for child in children.iter_descendants(trigger.target()) {
        if let Ok(icon_entity) = icon.get(child) {
            commands.entity(icon_entity).despawn();
        }
    }
}

fn on_message_accepted(
    mut commands: Commands,
    assets: Res<GameAssets>,
    mut drain: ResMut<MessageDrain>,
    mut catched_up: Local<bool>,
) {
    for msg in drain.drain_accepted() {
        match msg {
            ServerMessage::FinishedReplayingHistory => {
                *catched_up = true;
            }
            ServerMessage::TakeFreshCardFromDeck { .. } => {
                if *catched_up {
                    commands.spawn((
                        AudioPlayer::new(assets.card_sweep.clone()),
                        PlaybackSettings::DESPAWN,
                    ));
                }
            }
            ServerMessage::SwapHeldCardWithSlotCard { .. } => {
                if *catched_up {
                    commands.spawn((
                        AudioPlayer::new(assets.card_swap.clone()),
                        PlaybackSettings::DESPAWN,
                    ));
                }
            }
            ServerMessage::DropCardOnSlot { .. } | ServerMessage::DropCardOnDiscardPile { .. } => {
                if *catched_up {
                    commands.spawn((
                        AudioPlayer::new(assets.card_laydown.clone()),
                        PlaybackSettings::DESPAWN,
                    ));
                }
            }
            _ => (),
        }
    }
}
