use std::time::Duration;

use bevy::{app::ScheduleRunnerPlugin, prelude::*};

use bevy_matchbox::{MatchboxSocket, prelude::WebRtcSocketBuilder};

pub fn run() {
    let mut app = App::new();

    app.add_plugins(MinimalPlugins.set(ScheduleRunnerPlugin::run_loop(Duration::from_millis(5))));

    let rtc_socket = WebRtcSocketBuilder::new(format!("wss://hugopeters.me:3536?host"))
        .add_reliable_channel()
        .add_unreliable_channel()
        .build();

    let socket = MatchboxSocket::from(rtc_socket);
    app.insert_resource(socket);
}
