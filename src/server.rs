use std::{net::UdpSocket, time::SystemTime};

use bevy::prelude::*;
use bevy_rand::{global::GlobalEntropy, prelude::WyRand};
use bevy_renet::{
    RenetServerPlugin,
    netcode::{NetcodeServerPlugin, NetcodeServerTransport, ServerAuthentication, ServerConfig},
    renet::{ClientId, ConnectionConfig, DefaultChannel, RenetServer, ServerEvent},
};
use rand::Rng;
use strum::IntoEnumIterator;

use crate::cambio::*;
use crate::cards::*;
use crate::messages::*;
use crate::utils::Seq;

trait ServerExt {
    fn send_message_typed(&mut self, client_id: ClientId, message: ServerMessage);
}

impl ServerExt for RenetServer {
    fn send_message_typed(&mut self, client_id: ClientId, message: ServerMessage) {
        let encoded = bincode::serde::encode_to_vec(message, bincode::config::standard()).unwrap();
        self.send_message(client_id, DefaultChannel::ReliableOrdered, encoded);
    }
}

#[derive(Default, Resource)]
struct AcceptedHistory(Vec<ServerMessage>);

pub struct ServerPlugin;

impl Plugin for ServerPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(RenetServerPlugin);
        app.add_plugins(NetcodeServerPlugin);
        let (server, transport) = new_renet_server();
        app.insert_resource(server);
        app.insert_resource(transport);
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
    mut commands: Commands,
    mut server_events: EventReader<ServerEvent>,
    mut server: ResMut<RenetServer>,
    state: Res<CambioState>,
    mut players: Query<(&PlayerId, &mut PlayerState)>,
    mut player_seq: Local<Seq<u8>>,
    mut slot_seq: Local<Seq<u64>>,
    accepted_history: Res<AcceptedHistory>,
    mut entropy: GlobalEntropy<WyRand>,
) {
    let mut to_process: Vec<ServerMessage> = Vec::new();
    for event in server_events.read() {
        match event {
            ServerEvent::ClientConnected { client_id } => {
                println!("Player {} connected.", client_id);

                let new_player_id = PlayerId {
                    player_index: player_seq.generate(),
                    client_id: *client_id,
                };

                // Replay all historical events
                for msg in accepted_history.0.iter() {
                    server.send_message_typed(*client_id, msg.redacted_for(&new_player_id));
                }
                server.send_message_typed(*client_id, ServerMessage::FinishedReplayingHistory);

                to_process.push(ServerMessage::PlayerConnected {
                    player_id: new_player_id,
                });

                for i in 0..4 {
                    let slot_id = SlotId(slot_seq.generate());
                    to_process.push(ServerMessage::ReceiveFreshSlot {
                        actor: new_player_id,
                        slot_id,
                    });

                    to_process.push(ServerMessage::ReceiveFreshCardFromDeck {
                        actor: new_player_id,
                        slot_id,
                        card_id: state.free_cards[i],
                    });
                }

                // If this is the first player, start the game
                if state.player_index.is_empty() {
                    to_process.push(ServerMessage::PlayerAtTurn {
                        player_id: new_player_id,
                    });
                }
            }
            ServerEvent::ClientDisconnected { client_id, .. } => {
                for player in state.player_index.keys() {
                    if player.client_id == *client_id {
                        to_process.push(ServerMessage::PlayerDisconnected { player_id: *player });
                    }
                }
            }
        }
    }

    for claimer_id in state.player_index.keys() {
        while let Some(message) =
            server.receive_message(claimer_id.client_id, DefaultChannel::ReliableOrdered)
        {
            let claim: ClientClaim =
                bincode::serde::decode_from_slice(&message, bincode::config::standard())
                    .unwrap()
                    .0;

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
                ClientClaim::LookAtCardAtSlot { card_id, slot_id } => {
                    ServerMessage::RevealCardAtSlot {
                        actor: *claimer_id,
                        card_id,
                        slot_id,
                    }
                }
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
    }

    for msg in to_process.drain(..) {
        commands.run_system_cached_with(process_single_event.pipe(|_: In<bool>| ()), msg);
    }

    for (player_id, player_entity) in state.player_index.iter() {
        while let Some(message) =
            server.receive_message(player_id.client_id, DefaultChannel::Unreliable)
        {
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

    server.broadcast_message(
        DefaultChannel::Unreliable,
        bincode::serde::encode_to_vec(mouse_update, bincode::config::standard()).unwrap(),
    );
}

fn recover_from_rejected(mut drain: ResMut<MessageDrain>) {
    for _ in drain.drain_rejected() {}
}

fn broadcast_validated_updates(
    mut bus: ResMut<MessageDrain>,
    mut server: ResMut<RenetServer>,
    mut processed_history: ResMut<AcceptedHistory>,
    card_lookup: Res<CardValueLookup>,
    state: Res<CambioState>,
) {
    let mut send = |msg: ServerMessage| {
        for player_id in state.player_index.keys() {
            server.send_message_typed(player_id.client_id, msg.redacted_for(player_id));
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
