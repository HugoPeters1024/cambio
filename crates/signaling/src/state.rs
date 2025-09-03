use std::{
    collections::HashMap,
    net::SocketAddr,
    sync::{Arc, Mutex},
};

use axum::{extract::ws::Message, response::Response};
use matchbox_protocol::{JsonPeerEvent, PeerId};
use matchbox_signaling::{
    SignalingState,
    common_logic::{SignalingChannel, try_send},
};
use tracing::{error, info};

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct RoomId(pub String);

pub struct Room {
    pub host: (PeerId, SignalingChannel),
    pub clients: HashMap<PeerId, SignalingChannel>,
}

#[derive(Copy, Clone)]
pub enum HostOrClient {
    Host,
    Client,
}

#[derive(Default, Clone)]
pub(crate) struct ServerState {
    // Clients that have connected, requesting a certain room and wether to be a host
    // or a client.
    pub incoming_connections: Arc<Mutex<HashMap<SocketAddr, (RoomId, HostOrClient)>>>,
    // Clients that have been assigned a peer id, we can now process their intentions
    pub clients_in_queue: Arc<Mutex<HashMap<PeerId, (RoomId, HostOrClient)>>>,
    // Rooms currently being hosted.
    pub hosted_rooms: Arc<Mutex<HashMap<RoomId, Room>>>,
}

impl ServerState {
    pub fn on_connection_request(
        &mut self,
        origin: SocketAddr,
        room_id: RoomId,
        is_host: bool,
    ) -> Result<bool, Response> {
        self.incoming_connections.lock().unwrap().insert(
            origin,
            (
                room_id.clone(),
                if is_host {
                    HostOrClient::Host
                } else {
                    HostOrClient::Client
                },
            ),
        );
        Ok(true)
    }

    pub fn on_id_assignment(&mut self, origin: SocketAddr, peer_id: PeerId) {
        let val = self
            .incoming_connections
            .lock()
            .unwrap()
            .remove(&origin)
            .unwrap();
        self.clients_in_queue.lock().unwrap().insert(peer_id, val);
    }

    /// Inform all clients that the host has disconnected.
    pub fn reset_room(&mut self, room_id: &RoomId) {
        let room = { self.hosted_rooms.lock().as_mut().unwrap().remove(&room_id) };
        if let Some(room) = room {
            // Tell each connected peer about the disconnected host.
            let event = Message::Text(JsonPeerEvent::PeerLeft(room.host.0).to_string().into());
            room.clients.iter().for_each(|(peer_id, sender)| {
                match try_send(sender, event.clone()) {
                    Ok(()) => {
                        info!("Sent host peer remove to: {peer_id}")
                    }
                    Err(e) => {
                        error!("Failure sending host peer remove to {peer_id}: {e:?}")
                    }
                }
            });
        }
    }
}

impl SignalingState for ServerState {}
