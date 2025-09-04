use bevy::{color::palettes::css::*, prelude::*};
use bevy_ui_text_input::{TextInputContents, TextInputMode, TextInputNode, TextInputPrompt};

use crate::{
    assets::GamePhase,
    client::ConnectionSettings,
    menu_button::{MenuButton, MenuButtonClicked, MenuButtonPlugin},
    transport::Transport,
};

pub struct MenuPlugin;

impl Plugin for MenuPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(MenuButtonPlugin);
        app.add_plugins(bevy_ui_text_input::TextInputPlugin);
        app.add_systems(OnEnter(GamePhase::Menu), setup_menu);
        app.add_systems(OnExit(GamePhase::Menu), remove_menu);
    }
}

#[derive(Component)]
pub struct MenuRoot;

#[derive(Component)]
pub struct ServerUrlInput;

#[derive(Component)]
pub struct RoomIdInput;

#[derive(Component)]
pub struct UsernameInput;

#[derive(Component)]
pub struct HostGameButton;

#[derive(Component)]
pub struct JoinGameButton;

fn setup_menu(mut commands: Commands) {
    commands.spawn((
        Camera2d,
        Projection::Orthographic(OrthographicProjection {
            scaling_mode: bevy::render::camera::ScalingMode::FixedVertical {
                viewport_height: 480.0,
            },
            ..OrthographicProjection::default_2d()
        }),
    ));

    let root = commands
        .spawn((
            MenuRoot,
            Node {
                width: Val::Percent(100.0),
                height: Val::Percent(100.0),
                align_items: AlignItems::Center,
                justify_content: JustifyContent::Center,
                flex_direction: FlexDirection::Column,
                ..default()
            },
        ))
        .id();

    // input section
    commands
        .spawn((
            ChildOf(root),
            Node {
                width: Val::Vw(50.0),
                height: Val::Vh(50.0),
                justify_content: JustifyContent::Center,
                flex_direction: FlexDirection::Column,
                ..default()
            },
        ))
        .with_children(|input_section| {
            input_section.spawn((
                Node {
                    width: Val::Percent(100.0),
                    padding: UiRect::vertical(Val::Px(5.0)),
                    ..default()
                },
                children![
                    (
                        Text::new("server url"),
                        Node {
                            width: Val::Percent(30.0),
                            padding: UiRect::right(Val::Px(10.0)),
                            ..default()
                        }
                    ),
                    (
                        BackgroundColor(BLACK.into()),
                        Node {
                            width: Val::Percent(70.0),
                            height: Val::Px(45.0),
                            padding: UiRect::all(Val::Px(5.0)),
                            justify_content: JustifyContent::Center,
                            ..default()
                        },
                        BorderRadius::all(Val::Px(5.0)),
                        TextInputPrompt {
                            text: "wss://hugopeters.me:3536".to_string(),
                            ..default()
                        },
                        TextInputNode {
                            mode: TextInputMode::SingleLine,
                            clear_on_submit: false,
                            ..default()
                        },
                        TextInputContents::default(),
                        TextFont {
                            font_size: 16.0,
                            ..default()
                        },
                        ServerUrlInput
                    ),
                ],
            ));

            input_section.spawn((
                Node {
                    width: Val::Percent(100.0),
                    padding: UiRect::vertical(Val::Px(5.0)),
                    ..default()
                },
                children![
                    (
                        Text::new("username"),
                        Node {
                            width: Val::Percent(30.0),
                            padding: UiRect::right(Val::Px(10.0)),
                            ..default()
                        }
                    ),
                    (
                        BackgroundColor(BLACK.into()),
                        Node {
                            width: Val::Percent(70.0),
                            height: Val::Px(45.0),
                            padding: UiRect::all(Val::Px(5.0)),
                            justify_content: JustifyContent::Center,
                            ..default()
                        },
                        BorderRadius::all(Val::Px(5.0)),
                        TextInputNode {
                            mode: TextInputMode::SingleLine,
                            clear_on_submit: false,
                            ..default()
                        },
                        TextInputPrompt {
                            text: "John Doe".to_string(),
                            ..default()
                        },
                        TextFont {
                            font_size: 16.0,
                            ..default()
                        },
                        TextInputContents::default(),
                        UsernameInput
                    )
                ],
            ));

            input_section.spawn((
                Node {
                    width: Val::Percent(100.0),
                    padding: UiRect::vertical(Val::Px(5.0)),
                    ..default()
                },
                children![
                    (
                        Text::new("room id"),
                        Node {
                            width: Val::Percent(30.0),
                            padding: UiRect::right(Val::Px(10.0)),
                            ..default()
                        }
                    ),
                    (
                        BackgroundColor(BLACK.into()),
                        Node {
                            width: Val::Percent(70.0),
                            height: Val::Px(45.0),
                            padding: UiRect::all(Val::Px(5.0)),
                            justify_content: JustifyContent::Center,
                            ..default()
                        },
                        BorderRadius::all(Val::Px(5.0)),
                        TextInputNode {
                            mode: TextInputMode::SingleLine,
                            clear_on_submit: false,
                            ..default()
                        },
                        TextInputPrompt {
                            text: "12345".to_string(),
                            ..default()
                        },
                        TextFont {
                            font_size: 16.0,
                            ..default()
                        },
                        TextInputContents::default(),
                        RoomIdInput
                    )
                ],
            ));
        });

    // start button
    commands
        .spawn((
            ChildOf(root),
            MenuButton("Host Game".to_string()),
            HostGameButton,
        ))
        .observe(on_host_game);

    commands
        .spawn((
            ChildOf(root),
            MenuButton("Join Game".to_string()),
            JoinGameButton,
        ))
        .observe(on_join_game);
}

fn remove_menu(mut commands: Commands, query: Query<Entity, With<MenuRoot>>) {
    for entity in query.iter() {
        commands.entity(entity).despawn();
    }
}

fn on_host_game(
    _trigger: Trigger<MenuButtonClicked>,
    mut commands: Commands,
    server_url_input: Single<(&TextInputContents, &TextInputPrompt), With<ServerUrlInput>>,
    username_input: Single<(&TextInputContents, &TextInputPrompt), With<UsernameInput>>,
    room_id_input: Single<(&TextInputContents, &TextInputPrompt), With<RoomIdInput>>,
    mut next_state: ResMut<NextState<GamePhase>>,
) {
    let server_url = if server_url_input.0.get().is_empty() {
        server_url_input.1.text.as_str()
    } else {
        server_url_input.0.get()
    }
    .to_string();

    let username = if username_input.0.get().is_empty() {
        username_input.1.text.as_str()
    } else {
        username_input.0.get()
    }
    .to_string();

    let room_id = if room_id_input.0.get().is_empty() {
        room_id_input.1.text.as_str()
    } else {
        room_id_input.0.get()
    }
    .to_string();

    commands.insert_resource(ConnectionSettings {
        server_url: server_url.clone(),
        username,
    });

    let transport = Transport::new_host(&mut commands, server_url, room_id);
    commands.insert_resource(transport);
    next_state.set(GamePhase::Connecting);
}

fn on_join_game(
    _trigger: Trigger<MenuButtonClicked>,
    mut commands: Commands,
    server_url_input: Single<(&TextInputContents, &TextInputPrompt), With<ServerUrlInput>>,
    username_input: Single<(&TextInputContents, &TextInputPrompt), With<UsernameInput>>,
    room_id_input: Single<(&TextInputContents, &TextInputPrompt), With<RoomIdInput>>,
    mut next_state: ResMut<NextState<GamePhase>>,
) {
    let server_url = if server_url_input.0.get().is_empty() {
        server_url_input.1.text.as_str()
    } else {
        server_url_input.0.get()
    }
    .to_string();

    let username = if username_input.0.get().is_empty() {
        username_input.1.text.as_str()
    } else {
        username_input.0.get()
    }
    .to_string();

    let room_id = if room_id_input.0.get().is_empty() {
        room_id_input.1.text.as_str()
    } else {
        room_id_input.0.get()
    }
    .to_string();

    commands.insert_resource(ConnectionSettings {
        server_url: server_url.clone(),
        username,
    });

    let transport = Transport::new_client(server_url, room_id);
    commands.insert_resource(transport);
    next_state.set(GamePhase::Connecting);
}
