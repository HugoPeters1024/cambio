use bevy::{ecs::system::SystemId, platform::collections::HashMap, prelude::*};
use bevy_matchbox::{
    MatchboxSocket,
    prelude::{PeerId, PeerState, WebRtcSocketBuilder},
};
use bevy_rand::{global::GlobalEntropy, prelude::WyRand};
use rand::{Rng, seq::SliceRandom};

use crate::{
    assets::GamePhase,
    cambio::{
        AcceptedMessage, CambioState, CardId, PlayerId, PlayerState, RejectionReason, SlotHasCard,
        process_single_event, spawn_cambio_root,
    },
    cards::KnownCard,
    host_utils::host_eval_event,
    messages::{
        ClientClaim, ClientClaimUnreliable, RELIABLE_CHANNEL, ServerMessage,
        ServerMessageUnreliable, UNRELIABLE_CHANNEL,
    },
    utils::Seq,
};

pub struct TransportPlugin;

pub enum ClaimFrom {
    Host(ClientClaim),
    Client(PeerId, ClientClaim),
}

pub enum ClaimFromUnreliable {
    Host(ClientClaimUnreliable),
    Client(PeerId, ClientClaimUnreliable),
}

pub struct HostTransport {
    pub username: String,
    pub socket_id: Option<PeerId>,
    pub player_seq: Seq<u8>,
    pub slot_seq: Seq<u64>,
    pub socket: MatchboxSocket,
    pub incoming_claims: Vec<ClaimFrom>,
    pub incoming_claims_unreliable: Vec<ClaimFromUnreliable>,
    pub accepted_history: Vec<ServerMessage>,
    pub setup_new_player: SystemId<In<PlayerId>, ()>,
}

pub struct ClientTransport {
    pub username: String,
    pub socket_id: Option<PeerId>,
    pub socket: MatchboxSocket,
}

#[derive(Resource)]
pub enum Transport {
    Host(HostTransport),
    Client(ClientTransport),
}

impl Transport {
    pub fn new_host(
        commands: &mut Commands,
        username: String,
        server_url: String,
        room_id: String,
    ) -> Self {
        let rtc_socket = WebRtcSocketBuilder::new(format!("{server_url}/{room_id}?host"))
            .add_reliable_channel()
            .add_unreliable_channel()
            .build();

        let socket = MatchboxSocket::from(rtc_socket);

        Transport::Host(HostTransport {
            username,
            socket_id: None,
            socket,
            player_seq: Seq::default(),
            slot_seq: Seq::default(),
            incoming_claims: Vec::new(),
            incoming_claims_unreliable: Vec::new(),
            accepted_history: Vec::new(),
            setup_new_player: commands.register_system(crate::host_utils::setup_new_player),
        })
    }

    pub fn new_client(username: String, server_url: String, room_id: String) -> Self {
        let rtc_socket = WebRtcSocketBuilder::new(format!("{server_url}/{room_id}"))
            .add_reliable_channel()
            .add_unreliable_channel()
            .build();

        let socket = MatchboxSocket::from(rtc_socket);
        Transport::Client(ClientTransport {
            username,
            socket_id: None,
            socket,
        })
    }

    pub fn socket_id(&self) -> Option<PeerId> {
        match self {
            Transport::Host(host) => host.socket_id,
            Transport::Client(client) => client.socket_id,
        }
    }

    pub fn username(&self) -> String {
        match self {
            Transport::Host(host) => host.username.clone(),
            Transport::Client(client) => client.username.clone(),
        }
    }

    pub fn queue_claim(&mut self, claim: ClientClaim) {
        match self {
            Transport::Host(host) => host.incoming_claims.push(ClaimFrom::Host(claim)),
            Transport::Client(client) => {
                let encoded = bincode::serde::encode_to_vec(&claim, bincode::config::standard())
                    .unwrap()
                    .into_boxed_slice();

                let peers = client.socket.connected_peers().collect::<Vec<_>>();

                assert!(peers.len() == 1);
                client
                    .socket
                    .channel_mut(RELIABLE_CHANNEL)
                    .send(encoded, peers[0]);
            }
        }
    }

    pub fn queue_claim_unreliable(&mut self, claim: ClientClaimUnreliable) {
        match self {
            Transport::Host(host) => host
                .incoming_claims_unreliable
                .push(ClaimFromUnreliable::Host(claim)),
            Transport::Client(client) => {
                let encoded = bincode::serde::encode_to_vec(&claim, bincode::config::standard())
                    .unwrap()
                    .into_boxed_slice();

                let peers = client.socket.connected_peers().collect::<Vec<_>>();

                assert!(peers.len() == 1);
                client
                    .socket
                    .channel_mut(UNRELIABLE_CHANNEL)
                    .send(encoded, peers[0]);
            }
        }
    }
}

impl Plugin for TransportPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Update,
            wait_for_transport_connected.run_if(in_state(GamePhase::Connecting)),
        );

        app.add_systems(
            OnEnter(GamePhase::Playing),
            (spawn_cambio_root, host_initializes_its_player).chain(),
        );

        app.add_systems(
            Update,
            (
                both_sync_peers,
                both_update_state_timer,
                host_ends_game_on_timer,
                host_receives_claims_from_clients,
                host_processes_reliable_claims,
                host_processes_unreliable_claims,
                host_persists_and_broadcasts_accepted_events,
                client_receives_reliable_results,
                client_receives_unreliable_results,
            )
                .chain()
                .run_if(in_state(GamePhase::Playing)),
        );
    }
}

fn wait_for_transport_connected(
    transport: ResMut<Transport>,
    mut next_state: ResMut<NextState<GamePhase>>,
) {
    match transport.into_inner() {
        Transport::Host(host) => {
            if let Some(id) = host.socket.id()
                && host.socket_id.is_none()
            {
                host.socket_id.replace(id);
                info!("My socket was assigned id: {id}");
                next_state.set(GamePhase::Playing);
            };
        }
        Transport::Client(client) => {
            if let Some(id) = client.socket.id() {
                if client.socket_id.is_none() {
                    client.socket_id.replace(id);
                    info!("My socket was assigned id: {id}");
                }

                for (_, peer_state) in client.socket.update_peers() {
                    match peer_state {
                        PeerState::Connected => {
                            info!("Connection to server established");
                            next_state.set(GamePhase::Playing);
                        }
                        PeerState::Disconnected => {
                            error!("Disconnected from server");
                            next_state.set(GamePhase::ConnectionLost);
                        }
                    }
                }
            };
        }
    }
}

fn host_initializes_its_player(mut commands: Commands, transport: ResMut<Transport>) {
    // For the host itself we won't receive a PeerConnected event,
    // so we need to make sure to initialize our own player manually.
    let Transport::Host(host) = transport.into_inner() else {
        return;
    };

    let peer_id = host.socket_id.unwrap();
    let player_index = host.player_seq.generate();
    let player_id = PlayerId {
        peer_id,
        player_index,
    };
    commands.run_system_with(host.setup_new_player, player_id);
}

fn both_sync_peers(
    mut commands: Commands,
    transport: ResMut<Transport>,
    state: Single<&CambioState>,
    mut next_state: ResMut<NextState<GamePhase>>,
) {
    match transport.into_inner() {
        Transport::Client(client) => {
            for (_, peer_state) in client.socket.update_peers() {
                match peer_state {
                    PeerState::Connected => panic!(
                        "Impossible, the server connection should have happened in wait_for_transport_connected!"
                    ),
                    PeerState::Disconnected => next_state.set(GamePhase::ConnectionLost),
                }
            }
        }
        Transport::Host(host) => {
            for (peer_id, peer_state) in host.socket.update_peers() {
                match peer_state {
                    PeerState::Connected => {
                        info!("A client connected: {peer_id}");
                        let player_index = host.player_seq.generate();
                        let player_id = PlayerId {
                            peer_id,
                            player_index,
                        };
                        commands.run_system_with(host.setup_new_player, player_id);
                    }
                    PeerState::Disconnected => {
                        warn!("A client disconnected: {peer_id}");
                        let player_id = state
                            .player_index
                            .keys()
                            .find(|player_id| player_id.peer_id == peer_id)
                            .unwrap();
                        commands.run_system_cached_with(
                            host_eval_event,
                            ServerMessage::PlayerDisconnected {
                                player_id: *player_id,
                            },
                        );
                    }
                }
            }
        }
    }
}

fn both_update_state_timer(mut state: Single<&mut CambioState>, time: Res<Time>) {
    if let Some(timer) = &mut state.game_will_finish_in {
        timer.tick(time.delta());
    }
}

fn host_ends_game_on_timer(
    mut commands: Commands,
    state: Single<&CambioState>,
    transport: ResMut<Transport>,
    players: Query<&PlayerState>,
    card_ids: Query<&CardId>,
    has_card: Query<&SlotHasCard>,
) {
    let Transport::Host(_) = transport.into_inner() else {
        return;
    };

    if let Some(timer) = state.game_will_finish_in.as_ref() {
        if timer.finished() {
            let mut final_score = HashMap::new();

            for (player_id, player_entity) in state.player_index.iter() {
                let player_state = players.get(*player_entity).unwrap();
                let mut score: i32 = 0;
                for slot_id in player_state.slots.iter() {
                    if let Ok(has_card) = has_card.get(*state.slot_index.get(slot_id).unwrap()) {
                        let card_id = card_ids.get(has_card.card_entity()).unwrap();
                        let known_card = state.card_lookup.0.get(card_id).unwrap();
                        score += known_card.penalty_score();
                    }
                }
                final_score.insert(*player_id, score);
            }

            commands.run_system_cached_with(
                host_eval_event,
                ServerMessage::GameFinished {
                    all_cards: state.card_lookup.0.clone(),
                    final_scores: final_score,
                },
            );
        }
    }
}

fn host_receives_claims_from_clients(transport: ResMut<Transport>) {
    let Transport::Host(host) = transport.into_inner() else {
        return;
    };

    for (peer_id, claim) in host.socket.channel_mut(RELIABLE_CHANNEL).receive() {
        let claim: ClientClaim =
            bincode::serde::decode_from_slice(&claim, bincode::config::standard())
                .unwrap()
                .0;

        host.incoming_claims.push(ClaimFrom::Client(peer_id, claim));
    }

    for (peer_id, claim) in host.socket.channel_mut(UNRELIABLE_CHANNEL).receive() {
        let claim: ClientClaimUnreliable =
            bincode::serde::decode_from_slice(&claim, bincode::config::standard())
                .unwrap()
                .0;

        host.incoming_claims_unreliable
            .push(ClaimFromUnreliable::Client(peer_id, claim));
    }
}

fn host_processes_reliable_claims(
    mut commands: Commands,
    transport: ResMut<Transport>,
    state: Single<&CambioState>,
    mut entropy: GlobalEntropy<WyRand>,
) {
    let Transport::Host(host) = transport.into_inner() else {
        return;
    };

    for claim_from in host.incoming_claims.drain(..) {
        let (peer_id, claim) = match claim_from {
            ClaimFrom::Host(claim) => {
                println!("Received claim from me: {claim:?}");
                (host.socket.id().unwrap(), claim)
            }
            ClaimFrom::Client(peer_id, claim) => {
                println!("Received claim from {peer_id}: {claim:?}");
                (peer_id, claim)
            }
        };

        let claimer_id = state
            .player_index
            .keys()
            .find(|player_id| player_id.peer_id == peer_id)
            .unwrap();

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
                check_turn: true,
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
            ClientClaim::TakeCardFromDiscardPile { card_id } => {
                ServerMessage::TakeCardFromDiscardPile {
                    actor: *claimer_id,
                    card_id,
                }
            }
            ClientClaim::SwapHeldCardWithSlotCard {
                slot_id,
                slot_card_id,
                held_card_id,
            } => ServerMessage::SwapHeldCardWithSlotCard {
                actor: *claimer_id,
                slot_id,
                slot_card_id,
                held_card_id,
            },
            ClientClaim::SlapTable => ServerMessage::SlapTable { actor: *claimer_id },
            ClientClaim::SetUserName { username } => ServerMessage::SetUsername {
                actor: *claimer_id,
                username,
            },
        };

        commands.run_system_cached_with(host_eval_event, server_message);
    }
}

fn host_processes_unreliable_claims(
    transport: ResMut<Transport>,
    state: Single<(Entity, &CambioState)>,
    mut players: Query<&mut PlayerState>,
) {
    let Transport::Host(host) = transport.into_inner() else {
        return;
    };
    let (_, state) = *state;

    for claim_from in host.incoming_claims_unreliable.drain(..) {
        let (peer_id, claim) = match claim_from {
            ClaimFromUnreliable::Host(claim) => (host.socket.id().unwrap(), claim),
            ClaimFromUnreliable::Client(peer_id, claim) => (peer_id, claim),
        };

        let Some((_, player_entity)) = state
            .player_index
            .iter()
            .find(|(player_id, _)| player_id.peer_id == peer_id)
        else {
            continue;
        };

        match claim {
            ClientClaimUnreliable::MousePosition(vec2) => {
                if let Ok(mut player_state) = players.get_mut(*player_entity) {
                    player_state.last_mouse_pos_world = vec2;
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
                    .map(|p| (id, p.last_mouse_pos_world))
            })
            .collect(),
    );

    let encoded = bincode::serde::encode_to_vec(&mouse_update, bincode::config::standard())
        .unwrap()
        .into_boxed_slice();

    let peers = host.socket.connected_peers().collect::<Vec<_>>();
    for peer_id in peers {
        host.socket
            .channel_mut(UNRELIABLE_CHANNEL)
            .send(encoded.clone(), peer_id);
    }
}

fn host_persists_and_broadcasts_accepted_events(
    transport: ResMut<Transport>,
    mut accepted: EventReader<AcceptedMessage>,
    state: Single<&CambioState>,
) {
    let Transport::Host(host) = transport.into_inner() else {
        return;
    };
    let peers = host.socket.connected_peers().collect::<Vec<_>>();

    macro_rules! broadcast_and_store {
        ($msg:expr) => {
            for peer_id in peers.iter() {
                let player_id = state
                    .player_index
                    .keys()
                    .find(|player_id| player_id.peer_id == *peer_id)
                    .unwrap();

                let packet = bincode::serde::encode_to_vec(
                    &$msg.redacted_for(player_id),
                    bincode::config::standard(),
                )
                .unwrap()
                .into_boxed_slice();

                host.socket
                    .channel_mut(RELIABLE_CHANNEL)
                    .send(packet, *peer_id);
            }
            host.accepted_history.push($msg);
        };
    }

    for AcceptedMessage(msg) in accepted.read() {
        // JIT publish the card values to the clients. We are the server so we don't
        // need to process these events ourselves. It is however important that they are
        // persisted so that players that join later can see the discard pile for example.
        match msg {
            ServerMessage::TakeFreshCardFromDeck { actor, card_id }
            | ServerMessage::RevealCardAtSlot { actor, card_id, .. } => {
                broadcast_and_store!(ServerMessage::PublishCardForPlayer {
                    player_id: *actor,
                    card_id: *card_id,
                    value: Some(state.card_lookup.0.get(&*card_id).unwrap().clone()),
                });
            }
            ServerMessage::DropCardOnDiscardPile { card_id, .. } => {
                broadcast_and_store!(ServerMessage::PublishCardPublically {
                    card_id: *card_id,
                    value: state.card_lookup.0.get(&*card_id).unwrap().clone(),
                });
            }
            _ => (),
        }

        broadcast_and_store!(msg.clone());
    }
}

fn client_receives_reliable_results(
    mut commands: Commands,
    transport: ResMut<Transport>,
    root: Single<Entity, With<CambioState>>,
) {
    let Transport::Client(client) = transport.into_inner() else {
        return;
    };

    for (_, message) in client.socket.channel_mut(RELIABLE_CHANNEL).receive() {
        let message: ServerMessage =
            bincode::serde::decode_from_slice(&message, bincode::config::standard())
                .unwrap()
                .0;

        commands.run_system_cached_with(
            process_single_event.pipe(|_: In<Result<ServerMessage, RejectionReason>>| ()),
            (*root, message),
        );
    }
}

fn client_receives_unreliable_results(
    transport: ResMut<Transport>,
    state: Single<&CambioState>,
    mut players: Query<&mut PlayerState>,
) {
    let Transport::Client(client) = transport.into_inner() else {
        return;
    };

    for (_, message) in client.socket.channel_mut(UNRELIABLE_CHANNEL).receive() {
        let message: ServerMessageUnreliable =
            bincode::serde::decode_from_slice(&message, bincode::config::standard())
                .unwrap()
                .0;
        match message {
            ServerMessageUnreliable::MousePositions(items) => {
                for (player_id, mouse_pos) in items.iter() {
                    let Some(player_entity) = state.player_index.get(player_id) else {
                        continue;
                    };

                    if let Ok(mut player_state) = players.get_mut(*player_entity) {
                        player_state.last_mouse_pos_world = *mouse_pos;
                    }
                }
            }
        }
    }
}
