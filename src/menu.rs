use bevy::{color::palettes::css::*, prelude::*};
use bevy_ui_text_input::{
    TextInputContents, TextInputMode, TextInputNode, TextInputPrompt,
};

use crate::{assets::GamePhase, client::ConnectionSettings};

const NORMAL_BUTTON: Color = Color::srgb(0.15, 0.15, 0.15);
const HOVERED_BUTTON: Color = Color::srgb(0.25, 0.25, 0.25);
const PRESSED_BUTTON: Color = Color::srgb(0.35, 0.75, 0.35);

pub struct MenuPlugin;

impl Plugin for MenuPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(bevy_ui_text_input::TextInputPlugin);
        app.add_systems(OnEnter(GamePhase::Menu), setup_menu);
        app.add_systems(OnExit(GamePhase::Menu), remove_menu);
        app.add_systems(Update, button_system);
    }
}

#[derive(Component)]
pub struct MenuRoot;

#[derive(Component)]
pub struct ServerUrlInput;

#[derive(Component)]
pub struct UsernameInput;

#[derive(Component)]
pub struct StartButton;

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
        });

    // start button
    commands.spawn((
        ChildOf(root),
        Button,
        StartButton,
        Node {
            width: Val::Px(180.0),
            height: Val::Px(65.0),
            border: UiRect::all(Val::Px(5.0)),
            // horizontally center child text
            justify_content: JustifyContent::Center,
            // vertically center child text
            align_items: AlignItems::Center,
            ..default()
        },
        BorderColor(Color::BLACK),
        BorderRadius::MAX,
        BackgroundColor(NORMAL_BUTTON),
        children![(
            Text::new("Play Cambio"),
            TextFont {
                font_size: 21.0,
                ..default()
            },
            TextColor(Color::srgb(0.9, 0.9, 0.9)),
            TextShadow::default(),
        )],
    ));
}

fn remove_menu(mut commands: Commands, query: Query<Entity, With<MenuRoot>>) {
    for entity in query.iter() {
        commands.entity(entity).despawn();
    }
}

fn button_system(
    mut commands: Commands,
    mut interaction_query: Query<
        (&Interaction, &mut BackgroundColor, &mut BorderColor),
        (Changed<Interaction>, With<Button>),
    >,
    server_url_input: Single<(&TextInputContents, &TextInputPrompt), With<ServerUrlInput>>,
    username_input: Single<(&TextInputContents, &TextInputPrompt), With<UsernameInput>>,
    mut next_state: ResMut<NextState<GamePhase>>,
) {
    for (interaction, mut color, mut border_color) in &mut interaction_query {
        match *interaction {
            Interaction::Pressed => {
                *color = PRESSED_BUTTON.into();
                border_color.0 = RED.into();
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

                commands.insert_resource(ConnectionSettings { server_url });
                next_state.set(GamePhase::Playing);
            }
            Interaction::Hovered => {
                *color = HOVERED_BUTTON.into();
                border_color.0 = Color::WHITE;
            }
            Interaction::None => {
                *color = NORMAL_BUTTON.into();
                border_color.0 = Color::BLACK;
            }
        }
    }
}
