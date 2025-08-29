use std::{
    collections::{HashMap, VecDeque},
    net::UdpSocket,
    time::SystemTime,
};

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
struct ProcessedHistory(Vec<ProcessedMessage>);

#[derive(Resource)]
pub struct CardDeck {
    free_cards: VecDeque<CardId>,
    lookup: HashMap<CardId, KnownCard>,
}

impl Default for CardDeck {
    fn default() -> Self {
        let lookup = Suit::iter()
            .flat_map(|suit| Rank::iter().map(move |rank| KnownCard { suit, rank }))
            .enumerate()
            .map(|(i, card)| (CardId(i as u64), card))
            .collect::<HashMap<_, _>>();

        let free_cards = lookup.keys().cloned().collect();

        CardDeck { lookup, free_cards }
    }
}

impl CardDeck {
    fn fresh_card_id(&mut self) -> CardId {
        self.free_cards.pop_front().unwrap()
    }

    fn return_free_card(&mut self, id: CardId) {
        self.free_cards.push_back(id);
    }

    fn reveal(&mut self, id: &CardId) -> KnownCard {
        self.lookup.get(id).unwrap().clone()
    }
}

pub struct ServerPlugin;

impl Plugin for ServerPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(RenetServerPlugin);
        app.add_plugins(NetcodeServerPlugin);
        let (server, transport) = new_renet_server();
        app.insert_resource(server);
        app.insert_resource(transport);
        app.init_resource::<ProcessedHistory>();
        app.init_resource::<CardDeck>();

        app.add_systems(
            FixedUpdate,
            (
                server_update_system.before(process_messages),
                (recover_from_rejected, broadcast_validated_updates).after(process_messages),
            ),
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
    mut server_events: EventReader<ServerEvent>,
    mut server: ResMut<RenetServer>,
    state: ResMut<CambioState>,
    mut players: Query<(&PlayerId, &mut PlayerState)>,
    mut player_seq: Local<Seq<u8>>,
    mut slot_seq: Local<Seq<u64>>,
    mut deck: ResMut<CardDeck>,
    mut bus: ResMut<MessageBus>,
    processed_history: Res<ProcessedHistory>,
    mut entropy: GlobalEntropy<WyRand>,
) {
    for event in server_events.read() {
        match event {
            ServerEvent::ClientConnected { client_id } => {
                println!("Player {} connected.", client_id);

                let new_player_id = PlayerId {
                    player_index: player_seq.generate(),
                    client_id: *client_id,
                };

                // Replay all historical events
                for ProcessedMessage(msg) in processed_history.0.iter() {
                    server.send_message_typed(*client_id, msg.redacted_for(&new_player_id));
                }
                server.send_message_typed(*client_id, ServerMessage::FinishedReplayingHistory);

                bus.speculate(ServerMessage::PlayerConnected {
                    player_id: new_player_id,
                });

                for _ in 0..4 {
                    let slot_id = SlotId(slot_seq.generate());
                    bus.speculate(ServerMessage::ReceiveFreshSlot {
                        actor: new_player_id,
                        slot_id,
                    });

                    bus.speculate(ServerMessage::ReceiveFreshCard {
                        actor: new_player_id,
                        slot_id,
                        card_id: deck.fresh_card_id(),
                    });
                }

                // If this is the first player, start the game
                if state.player_index.is_empty() {
                    bus.speculate(ServerMessage::PlayerAtTurn {
                        player_id: new_player_id,
                    });
                }
            }
            ServerEvent::ClientDisconnected { client_id, .. } => {
                for player in state.player_index.keys() {
                    if player.client_id == *client_id {
                        bus.speculate(ServerMessage::PlayerDisconnected { player_id: *player });
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
                        value: Some(deck.reveal(&card_id)),
                    }
                }
                ClientClaim::TakeFreshCardFromDeck => {
                    let card_id = deck.fresh_card_id();
                    ServerMessage::TakeFreshCardFromDeck {
                        actor: *claimer_id,
                        card_id,
                        value: Some(deck.reveal(&card_id)),
                    }
                }
                ClientClaim::DropCardOnDiscardPile { card_id } => {
                    ServerMessage::DropCardOnDiscardPile {
                        actor: *claimer_id,
                        card_id,
                        value: deck.reveal(&card_id),
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

            bus.speculate(server_message);
        }
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

fn recover_from_rejected(mut bus: ResMut<MessageBus>, mut deck: ResMut<CardDeck>) {
    for msg in bus.drain_rejected() {
        match msg {
            ServerMessage::PlayerConnected { .. } => {}
            ServerMessage::FinishedReplayingHistory => {}
            ServerMessage::PlayerDisconnected { .. } => {}
            ServerMessage::ReceiveFreshSlot { .. } => {}
            ServerMessage::ReceiveFreshCard { card_id, .. } => deck.return_free_card(card_id),
            ServerMessage::RevealCardAtSlot { .. } => {}
            ServerMessage::PickUpSlotCard { .. } => {}
            ServerMessage::SwapHeldCardWithSlotCard { .. } => {}
            ServerMessage::DropCardOnSlot { .. } => {}
            ServerMessage::TakeFreshCardFromDeck { card_id, .. } => deck.return_free_card(card_id),
            ServerMessage::DropCardOnDiscardPile { .. } => {}
            ServerMessage::TakeCardFromDiscardPile { .. } => {}
            ServerMessage::PlayerAtTurn { .. } => {}
        }
    }
}

fn broadcast_validated_updates(
    mut bus: ResMut<MessageBus>,
    mut server: ResMut<RenetServer>,
    mut processed_history: ResMut<ProcessedHistory>,
    state: Res<CambioState>,
) {
    for ProcessedMessage(msg) in bus.drain_processed() {
        processed_history.0.push(ProcessedMessage(msg.clone()));
        for player_id in state.player_index.keys() {
            server.send_message_typed(player_id.client_id, msg.redacted_for(player_id));
        }
    }
}
