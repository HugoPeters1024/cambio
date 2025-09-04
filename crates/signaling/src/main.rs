use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use tracing::info;

use axum::{http::StatusCode, response::IntoResponse, routing::get};
use matchbox_signaling::SignalingServerBuilder;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

use crate::{
    state::{RoomId, ServerState},
    topology::RoomedClientServerTopology,
};

mod state;
mod topology;

fn setup_logging() {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "matchbox_server=info,tower_http=debug,signaling=info".into()),
        )
        .with(
            tracing_subscriber::fmt::layer()
                .compact()
                .with_file(false)
                .with_target(false),
        )
        .init();
}

#[tokio::main]
async fn main() {
    setup_logging();
    let mut state = ServerState::default();
    let host = SocketAddr::new(IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)), 3536);
    let server = SignalingServerBuilder::new(host, RoomedClientServerTopology, state.clone())
        .on_connection_request({
            let mut state = state.clone();
            move |connection| {
                info!("Connection request from: {}", connection.origin);
                let room_id = RoomId(connection.path.clone().unwrap_or("default".to_string()));
                let is_host = connection.query_params.get("host").is_some();
                state.on_connection_request(connection.origin, room_id, is_host)
            }
        })
        .on_id_assignment({
            move |(origin, peer_id)| {
                info!("Client connected {origin:?}: {peer_id:?}");
                state.on_id_assignment(origin, peer_id);
            }
        })
        .cors()
        .trace()
        .mutate_router(|router| router.route("/health", get(health_handler)))
        .build();

    info!("Starting signaling server");
    server
        .serve()
        .await
        .expect("Unable to run signaling server, is it already running?")
}

pub async fn health_handler() -> impl IntoResponse {
    StatusCode::OK
}
