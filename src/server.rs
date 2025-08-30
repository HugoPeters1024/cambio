use std::net::{Ipv4Addr, SocketAddrV4};

use bevy::prelude::*;
use bevy_matchbox::{matchbox_signaling::SignalingServer, prelude::*};
use bevy_rand::{global::GlobalEntropy, prelude::WyRand};
use rand::Rng;
use strum::IntoEnumIterator;

use crate::cambio::*;
use crate::cards::*;
use crate::messages::*;
use crate::utils::Seq;

trait ServerExt {
    fn send_message_typed(&mut self, client_id: PeerId, message: ServerMessage);
}

impl ServerExt for MatchboxSocket {
    fn send_message_typed(&mut self, peer_id: PeerId, message: ServerMessage) {
        let encoded = bincode::serde::encode_to_vec(message, bincode::config::standard())
            .unwrap()
            .into_boxed_slice();

        self.channel_mut(RELIABLE_CHANNEL).send(encoded, peer_id);
    }
}

#[derive(Default, Resource)]
struct AcceptedHistory(Vec<ServerMessage>);

pub struct ServerPlugin;

impl Plugin for ServerPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<AcceptedHistory>();

        // Make sure the server is all knowing wrt card lookups
        app.insert_resource(CardValueLookup(
            Suit::iter()
                .flat_map(|suit| Rank::iter().map(move |rank| KnownCard { rank, suit }))
                .enumerate()
                .map(|(index, card)| (CardId(index as u64), card))
                .collect(),
        ));

        app.add_systems(
            FixedUpdate,
            (
                server_update_system,
                recover_from_rejected,
                broadcast_validated_updates,
            )
                .chain(),
        );

        app.add_systems(Startup, (start_signaling_server, start_host_socket).chain());
    }
}

fn start_signaling_server(mut commands: Commands) {
    info!("Starting signaling server");
    let addr = SocketAddrV4::new(Ipv4Addr::UNSPECIFIED, 3536);
    let signaling_server = MatchboxServer::from(
        SignalingServer::client_server_builder(addr)
            .on_connection_request(|connection| {
                info!("Connecting: {connection:?}");
                Ok(true) // Allow all connections
            })
            .on_id_assignment(|(socket, id)| info!("{socket} received {id}"))
            .on_host_connected(|id| info!("Host joined: {id}"))
            .on_host_disconnected(|id| info!("Host left: {id}"))
            .on_client_connected(move |id| info!("Client joined: {id}"))
            .on_client_disconnected(|id| info!("Client left: {id}"))
            .cors()
            .trace()
            .build(),
    );
    commands.insert_resource(signaling_server);
}

fn start_host_socket(mut commands: Commands) {
    let rtc_socket = WebRtcSocketBuilder::new("ws://localhost:3536/hello")
        .add_reliable_channel()
        .add_unreliable_channel()
        .build();

    let socket = MatchboxSocket::from(rtc_socket);
    commands.insert_resource(socket);
}

fn server_update_system(
    mut commands: Commands,
    mut server: ResMut<MatchboxSocket>,
    state: Res<CambioState>,
    mut players: Query<(&PlayerId, &mut PlayerState)>,
    mut player_seq: Local<Seq<u8>>,
    mut slot_seq: Local<Seq<u64>>,
    accepted_history: Res<AcceptedHistory>,
    mut entropy: GlobalEntropy<WyRand>,
) {
    let mut to_process: Vec<ServerMessage> = Vec::new();

    for (peer_id, peer_state) in server.update_peers() {
        match peer_state {
            PeerState::Connected => {
                let player_index = player_seq.generate();
                let player_id = PlayerId {
                    peer_id,
                    player_index,
                };

                for msg in accepted_history.0.iter() {
                    server.send_message_typed(peer_id, msg.redacted_for(&player_id));
                }
                server.send_message_typed(peer_id, ServerMessage::FinishedReplayingHistory);
                to_process.push(ServerMessage::PlayerConnected { player_id });

                for i in 0..4 {
                    let slot_id = SlotId(slot_seq.generate());
                    to_process.push(ServerMessage::ReceiveFreshSlot {
                        actor: player_id,
                        slot_id,
                    });
                    to_process.push(ServerMessage::ReceiveFreshCardFromDeck {
                        actor: player_id,
                        slot_id,
                        card_id: state.free_cards[i],
                    });
                }

                if player_index == 0 {
                    to_process.push(ServerMessage::PlayerAtTurn { player_id });
                }
            }
            PeerState::Disconnected => {
                let Some(player_id) = state
                    .player_index
                    .keys()
                    .find(|player_id| player_id.peer_id == peer_id)
                else {
                    continue;
                };
                to_process.push(ServerMessage::PlayerDisconnected {
                    player_id: *player_id,
                });
            }
        }
    }

    for (peer_id, message) in server.channel_mut(RELIABLE_CHANNEL).receive() {
        let claim: ClientClaim =
            bincode::serde::decode_from_slice(&message, bincode::config::standard())
                .unwrap()
                .0;
        println!("Got claim from {peer_id}: {claim:?}");
        let Some(claimer_id) = state
            .player_index
            .keys()
            .find(|player_id| player_id.peer_id == peer_id)
        else {
            warn!("Got claim from unknown peer that is not a player");
            continue;
        };

        let server_message = match claim {
            ClientClaim::PickUpSlotCard { slot_id, card_id } => ServerMessage::PickUpSlotCard {
                actor: *claimer_id,
                slot_id,
                card_id,
            },
            ClientClaim::DropCardOnSlot { card_id, slot_id } => ServerMessage::DropCardOnSlot {
                actor: *claimer_id,
                card_id,
                slot_id,
            },
            ClientClaim::LookAtCardAtSlot { card_id, slot_id } => ServerMessage::RevealCardAtSlot {
                actor: *claimer_id,
                card_id,
                slot_id,
            },
            ClientClaim::TakeFreshCardFromDeck => ServerMessage::TakeFreshCardFromDeck {
                actor: *claimer_id,
                card_id: state.free_cards[0],
            },
            ClientClaim::DropCardOnDiscardPile { card_id } => {
                ServerMessage::DropCardOnDiscardPile {
                    actor: *claimer_id,
                    card_id,
                    offset_x: entropy.random_range(-10.0..10.0),
                    offset_y: entropy.random_range(-10.0..10.0),
                    rotation: entropy.random_range(-0.4..0.4),
                }
            }
            ClientClaim::TakeCardFromDiscardPile => {
                ServerMessage::TakeCardFromDiscardPile { actor: *claimer_id }
            }
            ClientClaim::SwapHeldCardWithSlotCard {
                slot_id,
                held_card_id,
            } => ServerMessage::SwapHeldCardWithSlotCard {
                actor: *claimer_id,
                slot_id,
                held_card_id,
            },
        };

        to_process.push(server_message);
    }

    for msg in to_process.drain(..) {
        commands.run_system_cached_with(process_single_event.pipe(|_: In<bool>| ()), msg);
    }

    for (peer_id, message) in server.channel_mut(UNRELIABLE_CHANNEL).receive() {
        let Some((_, player_entity)) = state
            .player_index
            .iter()
            .find(|(player_id, _)| player_id.peer_id == peer_id)
        else {
            continue;
        };

        let claim: ClientClaimUnreliable =
            bincode::serde::decode_from_slice(&message, bincode::config::standard())
                .unwrap()
                .0;

        match claim {
            ClientClaimUnreliable::MousePosition(mouse_pos) => {
                if let Ok((_, mut player_state)) = players.get_mut(*player_entity) {
                    player_state.last_mouse_pos_world = mouse_pos;
                }
            }
        }
    }

    let mouse_update = ServerMessageUnreliable::MousePositions(
        state
            .player_index
            .iter()
            .filter_map(|(&id, player)| {
                players
                    .get(*player)
                    .ok()
                    .map(|p| (id, p.1.last_mouse_pos_world))
            })
            .collect(),
    );

    let encoded = bincode::serde::encode_to_vec(&mouse_update, bincode::config::standard())
        .unwrap()
        .into_boxed_slice();

    let peers = server.connected_peers().collect::<Vec<_>>();
    for peer_id in peers {
        server.channel_mut(1).send(encoded.clone(), peer_id);
    }
}

fn recover_from_rejected(mut drain: ResMut<MessageDrain>) {
    for _ in drain.drain_rejected() {}
}

fn broadcast_validated_updates(
    mut bus: ResMut<MessageDrain>,
    mut processed_history: ResMut<AcceptedHistory>,
    card_lookup: Res<CardValueLookup>,
    state: Res<CambioState>,
    mut server: ResMut<MatchboxSocket>,
) {
    let mut send = |msg: ServerMessage| {
        let peers = server.connected_peers().collect::<Vec<_>>();
        for peer_id in peers {
            let player_id = state
                .player_index
                .keys()
                .find(|player_id| player_id.peer_id == peer_id)
                .unwrap();
            server.send_message_typed(peer_id, msg.redacted_for(player_id));
        }
        processed_history.0.push(msg.clone());
    };

    for msg in bus.drain_accepted() {
        // JIT publish the values of cards
        match msg {
            ServerMessage::TakeFreshCardFromDeck { actor, card_id }
            | ServerMessage::RevealCardAtSlot { actor, card_id, .. } => {
                send(ServerMessage::PublishCardForPlayer {
                    player_id: actor,
                    card_id,
                    value: Some(card_lookup.0.get(&card_id).unwrap().clone()),
                });
            }
            ServerMessage::DropCardOnDiscardPile { card_id, .. } => {
                send(ServerMessage::PublishCardPublically {
                    card_id,
                    value: card_lookup.0.get(&card_id).unwrap().clone(),
                });
            }
            _ => (),
        }

        send(msg);
    }
}
