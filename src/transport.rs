use bevy::{ecs::system::SystemId, prelude::*};
use bevy_matchbox::{
    MatchboxSocket,
    prelude::{PeerId, PeerState, WebRtcSocketBuilder},
};
use bevy_rand::{global::GlobalEntropy, prelude::WyRand};
use rand::Rng;

use crate::{
    assets::GamePhase,
    cambio::{CambioState, PlayerId, PlayerState, process_single_event},
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

#[derive(Resource)]
pub enum Transport {
    Host {
        socket_id: Option<PeerId>,
        player_seq: Seq<u8>,
        socket: MatchboxSocket,
        incoming_claims: Vec<ClaimFrom>,
        incoming_claims_unreliable: Vec<ClaimFromUnreliable>,
        accepted_history: Vec<ServerMessage>,
        setup_new_player: SystemId<In<PlayerId>, ()>,
    },
    Client {
        socket_id: Option<PeerId>,
        socket: MatchboxSocket,
    },
}

impl Transport {
    pub fn new_host(commands: &mut Commands, server_url: String, room_id: String) -> Self {
        let rtc_socket = WebRtcSocketBuilder::new(format!("{server_url}/{room_id}?host"))
            .add_reliable_channel()
            .add_unreliable_channel()
            .build();

        let socket = MatchboxSocket::from(rtc_socket);

        Transport::Host {
            socket_id: None,
            socket,
            player_seq: Seq::default(),
            incoming_claims: Vec::new(),
            incoming_claims_unreliable: Vec::new(),
            accepted_history: Vec::new(),
            setup_new_player: commands.register_system(crate::host_utils::setup_new_player),
        }
    }

    pub fn new_client(server_url: String, room_id: String) -> Self {
        let rtc_socket = WebRtcSocketBuilder::new(format!("{server_url}/{room_id}"))
            .add_reliable_channel()
            .add_unreliable_channel()
            .build();

        let socket = MatchboxSocket::from(rtc_socket);
        Transport::Client {
            socket_id: None,
            socket,
        }
    }

    pub fn socket_id(&self) -> Option<PeerId> {
        match self {
            Transport::Host { socket_id, .. } => *socket_id,
            Transport::Client { socket_id, .. } => *socket_id,
        }
    }

    pub fn queue_claim(&mut self, claim: ClientClaim) {
        match self {
            Transport::Host {
                incoming_claims: unverified_claims,
                ..
            } => unverified_claims.push(ClaimFrom::Host(claim)),
            Transport::Client { socket, .. } => {
                let encoded = bincode::serde::encode_to_vec(&claim, bincode::config::standard())
                    .unwrap()
                    .into_boxed_slice();

                let peers = socket.connected_peers().collect::<Vec<_>>();

                assert!(peers.len() == 1);
                socket.channel_mut(RELIABLE_CHANNEL).send(encoded, peers[0]);
            }
        }
    }

    pub fn queue_claim_unreliable(&mut self, claim: ClientClaimUnreliable) {
        match self {
            Transport::Host {
                incoming_claims_unreliable: unverified_claims_unreliable,
                ..
            } => unverified_claims_unreliable.push(ClaimFromUnreliable::Host(claim)),
            Transport::Client { socket, .. } => {
                let encoded = bincode::serde::encode_to_vec(&claim, bincode::config::standard())
                    .unwrap()
                    .into_boxed_slice();

                let peers = socket.connected_peers().collect::<Vec<_>>();

                assert!(peers.len() == 1);
                socket
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
            (host_initializes_its_player).chain(),
        );

        app.add_systems(
            Update,
            (
                both_sync_peers,
                host_receives_claims_from_clients,
                host_processes_reliable_claims,
                host_processes_unreliable_claims,
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
        Transport::Host {
            socket_id: peer_id,
            socket,
            ..
        } => {
            if let Some(id) = socket.id()
                && peer_id.is_none()
            {
                peer_id.replace(id);
                info!("My socket was assigned id: {id}");
                next_state.set(GamePhase::Playing);
            };
        }
        Transport::Client {
            socket_id: peer_id,
            socket,
            ..
        } => {
            if let Some(id) = socket.id()
                && peer_id.is_none()
            {
                peer_id.replace(id);
                info!("My socket was assigned id: {id}");
                next_state.set(GamePhase::Playing);
            };
        }
    }
}

fn host_initializes_its_player(mut commands: Commands, transport: ResMut<Transport>) {
    // For the host itself we won't receive a PeerConnected event,
    // so we need to make sure to initialize our own player manually.

    match transport.into_inner() {
        Transport::Client { .. } => {}
        Transport::Host {
            socket_id,
            player_seq,
            setup_new_player,
            ..
        } => {
            let peer_id = socket_id.unwrap();
            let player_index = player_seq.generate();
            let player_id = PlayerId {
                peer_id,
                player_index,
            };
            commands.run_system_with(*setup_new_player, player_id);
        }
    }
}

fn both_sync_peers(mut commands: Commands, transport: ResMut<Transport>) {
    match transport.into_inner() {
        Transport::Client { socket, .. } => {
            socket.update_peers();
        }
        Transport::Host {
            socket,
            player_seq,
            setup_new_player,
            ..
        } => {
            for (peer_id, peer_state) in socket.update_peers() {
                match peer_state {
                    PeerState::Connected => {
                        info!("A client connected: {peer_id}");
                        let player_index = player_seq.generate();
                        let player_id = PlayerId {
                            peer_id,
                            player_index,
                        };
                        commands.run_system_with(*setup_new_player, player_id);
                    }
                    PeerState::Disconnected => {
                        warn!("A clien disconnected: {peer_id}");
                    }
                }
            }
        }
    }
}

fn host_receives_claims_from_clients(transport: ResMut<Transport>) {
    match transport.into_inner() {
        Transport::Client { .. } => {}
        Transport::Host {
            socket,
            incoming_claims,
            incoming_claims_unreliable,
            ..
        } => {
            for (peer_id, claim) in socket.channel_mut(RELIABLE_CHANNEL).receive() {
                let claim: ClientClaim =
                    bincode::serde::decode_from_slice(&claim, bincode::config::standard())
                        .unwrap()
                        .0;

                incoming_claims.push(ClaimFrom::Client(peer_id, claim));
            }

            for (peer_id, claim) in socket.channel_mut(UNRELIABLE_CHANNEL).receive() {
                let claim: ClientClaimUnreliable =
                    bincode::serde::decode_from_slice(&claim, bincode::config::standard())
                        .unwrap()
                        .0;

                incoming_claims_unreliable.push(ClaimFromUnreliable::Client(peer_id, claim));
            }
        }
    }
}

fn host_processes_reliable_claims(
    mut commands: Commands,
    transport: ResMut<Transport>,
    state: Single<(Entity, &CambioState)>,
    mut entropy: GlobalEntropy<WyRand>,
) {
    let (root, state) = *state;

    match transport.into_inner() {
        Transport::Client { .. } => {}
        Transport::Host {
            socket,
            incoming_claims: unverified_claims,
            ..
        } => {
            for claim_from in unverified_claims.drain(..) {
                let (peer_id, claim) = match claim_from {
                    ClaimFrom::Host(claim) => {
                        println!("Received claim from me: {claim:?}");
                        (socket.id().unwrap(), claim)
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
                    ClientClaim::PickUpSlotCard { slot_id, card_id } => {
                        ServerMessage::PickUpSlotCard {
                            actor: *claimer_id,
                            slot_id,
                            card_id,
                        }
                    }
                    ClientClaim::DropCardOnSlot { card_id, slot_id } => {
                        ServerMessage::DropCardOnSlot {
                            actor: *claimer_id,
                            card_id,
                            slot_id,
                        }
                    }
                    ClientClaim::LookAtCardAtSlot { card_id, slot_id } => {
                        ServerMessage::RevealCardAtSlot {
                            actor: *claimer_id,
                            card_id,
                            slot_id,
                            check_turn: true,
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
                    ClientClaim::WantsToPlay { username } => ServerMessage::SetUsername {
                        actor: *claimer_id,
                        username,
                    },
                };

                commands.run_system_cached_with(host_eval_event, server_message);
            }
        }
    }
}

fn host_processes_unreliable_claims(
    transport: ResMut<Transport>,
    state: Single<(Entity, &CambioState)>,
    mut players: Query<&mut PlayerState>,
) {
    let (_, state) = *state;
    match transport.into_inner() {
        Transport::Client { .. } => {}
        Transport::Host {
            socket_id,
            socket,
            incoming_claims_unreliable: unverified_claims_unreliable,
            ..
        } => {
            for claim_from in unverified_claims_unreliable.drain(..) {
                let (peer_id, claim) = match claim_from {
                    ClaimFromUnreliable::Host(claim) => (socket.id().unwrap(), claim),
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

            let peers = socket.connected_peers().collect::<Vec<_>>();
            for peer_id in peers {
                socket
                    .channel_mut(UNRELIABLE_CHANNEL)
                    .send(encoded.clone(), peer_id);
            }
        }
    }
}

fn client_receives_reliable_results(
    mut commands: Commands,
    transport: ResMut<Transport>,
    root: Single<Entity, With<CambioState>>,
) {
    match transport.into_inner() {
        Transport::Host { .. } => {
            // The host has already processed messages on account of being the auhority
        }
        Transport::Client { socket, .. } => {
            for (peer_id, message) in socket.channel_mut(RELIABLE_CHANNEL).receive() {
                let message: ServerMessage =
                    bincode::serde::decode_from_slice(&message, bincode::config::standard())
                        .unwrap()
                        .0;

                commands.run_system_cached_with(
                    process_single_event.pipe(|_: In<Option<ServerMessage>>| ()),
                    (*root, message),
                );
            }
        }
    }
}

fn client_receives_unreliable_results(
    transport: ResMut<Transport>,
    state: Single<&CambioState>,
    mut players: Query<&mut PlayerState>,
) {
    match transport.into_inner() {
        Transport::Host { .. } => {
            // The host has already processed messages on account of being the auhority
        }
        Transport::Client { socket, .. } => {
            for (peer_id, message) in socket.channel_mut(UNRELIABLE_CHANNEL).receive() {
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
    }
}
