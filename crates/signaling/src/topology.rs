use std::collections::HashMap;

use axum::extract::ws::Message;
use futures::StreamExt;
use matchbox_protocol::{JsonPeerEvent, PeerRequest};
use tracing::{error, info, warn};

use async_trait::async_trait;
use matchbox_signaling::{
    ClientRequestError, NoCallbacks, SignalingTopology, WsStateMeta,
    common_logic::{parse_request, try_send},
};

use crate::state::{HostOrClient, Room, ServerState};

pub struct RoomedClientServerTopology;

#[async_trait]
impl SignalingTopology<NoCallbacks, ServerState> for RoomedClientServerTopology {
    async fn state_machine(upgrade: WsStateMeta<NoCallbacks, ServerState>) {
        let WsStateMeta {
            peer_id,
            sender,
            mut receiver,
            mut state,
            ..
        } = upgrade;

        let (room_id, host_or_client) = state
            .clients_in_queue
            .lock()
            .unwrap()
            .remove(&peer_id)
            .unwrap();

        macro_rules! on_disconnect {
            () => {{
                match host_or_client {
                    HostOrClient::Host => {
                        info!("Host of room {} disconnected", room_id.0);
                        state.reset_room(&room_id);
                    }
                    HostOrClient::Client => {
                        info!("Client {} disconnected from room {}", peer_id, room_id.0);
                        let mut lock = state.hosted_rooms.lock().unwrap();
                        if let Some(room) = lock.get_mut(&room_id) {
                            room.clients.remove(&peer_id);
                        }
                    }
                }
            }};
        }

        match host_or_client {
            HostOrClient::Host => {
                let mut lock = state.hosted_rooms.lock().unwrap();
                if lock.contains_key(&room_id) {
                    error!("Room {} is already hosted", room_id.0);
                    return;
                }

                lock.insert(
                    room_id.clone(),
                    Room {
                        host: (peer_id, sender.clone()),
                        clients: HashMap::new(),
                    },
                );

                info!("Hosting room {}", room_id.0);
            }
            HostOrClient::Client => {
                let mut lock = state.hosted_rooms.lock().unwrap();
                let Some(room) = lock.get_mut(&room_id) else {
                    error!("Room {} is not hosted", room_id.0);
                    return;
                };

                let message = Message::Text(JsonPeerEvent::NewPeer(peer_id).to_string().into());
                match try_send(&room.host.1, message) {
                    Err(err) => {
                        error!("Failed to send message to host: {}", err);
                    }
                    Ok(_) => {
                        room.clients.insert(peer_id, sender.clone());
                    }
                }

                info!("Peer {} joined room {}", peer_id, room_id.0);
            }
        }

        // The state machine for the data channel established for this websocket.
        while let Some(request) = receiver.next().await {
            let request = match parse_request(request) {
                Ok(request) => request,
                Err(e) => {
                    match e {
                        ClientRequestError::Axum(_) => {
                            // Most likely a ConnectionReset or similar.
                            warn!("Unrecoverable error with {peer_id}: {e:?}");
                        }
                        ClientRequestError::Close => {
                            info!("Connection closed by {peer_id}");
                        }
                        ClientRequestError::Json(_) | ClientRequestError::UnsupportedType(_) => {
                            error!("Error with request: {e:?}");
                            continue; // Recoverable error
                        }
                    };

                    on_disconnect!();
                    return;
                }
            };

            match request {
                PeerRequest::Signal { receiver, data } => {
                    let event = Message::Text(
                        JsonPeerEvent::Signal {
                            sender: peer_id,
                            data,
                        }
                        .to_string()
                        .into(),
                    );
                    let lock = state.hosted_rooms.lock().unwrap();
                    let room = lock.get(&room_id).unwrap();
                    let receiver_channel = match host_or_client {
                        HostOrClient::Host => &room.clients.get(&receiver).unwrap(),
                        HostOrClient::Client => &room.host.1,
                    };

                    if let Err(e) = try_send(receiver_channel, event) {
                        error!("Error sending signal event: {}", e);
                    }
                }
                PeerRequest::KeepAlive => {
                    // Do nothing. KeepAlive packets are used to protect against idle websocket
                    // connections getting automatically disconnected, common for reverse proxies.
                }
            }
        }

        on_disconnect!();
    }
}
