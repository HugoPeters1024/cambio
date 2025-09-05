use std::time::Duration;

use bevy::{prelude::*, window::PrimaryWindow};
use bevy_tweening::lens::TransformRotateZLens;
use bevy_tweening::{Animator, RepeatCount, RepeatStrategy, Tween, lens::*};

use crate::assets::*;
use crate::cambio::*;
use crate::cards::*;
use crate::messages::*;
use crate::transport::Transport;

#[derive(Component)]
struct PlayerTurnIcon;

#[derive(Component)]
struct PlayerIdxText;

#[derive(Component)]
struct PlayerBuffText;

#[derive(Component)]
struct GameOverCountDown;

#[derive(Component)]
struct PlayerNameText;

#[derive(Component)]
struct SlapTableButton;

#[derive(Component)]
struct CursorIconFor(PlayerId);

pub struct ClientPlugin;

impl Plugin for ClientPlugin {
    fn build(&self, app: &mut App) {
        app.add_observer(on_player_spawn);
        app.add_observer(on_discard_pile_spawn);
        app.add_observer(click_slot_card);
        app.add_observer(click_slot);
        app.add_observer(click_discard_pile);

        app.add_systems(
            OnEnter(GamePhase::Playing),
            (spawn_cambio_root, setup).chain(),
        );

        app.add_systems(OnEnter(GamePhase::ConnectionLost), setup_connection_lost);

        app.add_systems(
            Update,
            ((
                set_and_publish_cursor_position,
                sync_cursors_from_state,
                sync_held_by,
            )
                .chain())
            .run_if(in_state(GamePhase::Playing)),
        );

        app.add_systems(
            Update,
            (
                hud_update_player_idx,
                hud_update_turn_buf,
                hud_update_slap_button,
                hud_game_finish_countdown,
            )
                .run_if(in_state(GamePhase::Playing)),
        );
        app.add_systems(
            Update,
            (
                effects_for_accepted_messages.run_if(in_state(GamePhase::Playing)),
                on_player_state_change,
            ),
        );
    }
}

fn setup(mut commands: Commands, state: Single<(Entity, &CambioState)>, assets: Res<GameAssets>) {
    commands.spawn((
        Node {
            width: Val::Percent(100.0),
            height: Val::Percent(100.0),
            ..default()
        },
        Pickable::IGNORE,
        children![
            (
                Node {
                    width: Val::Percent(50.0),
                    flex_direction: FlexDirection::Column,
                    ..default()
                },
                Pickable::IGNORE,
                children![
                    (
                        PlayerIdxText,
                        Text::new("You are player ..."),
                        TextFont {
                            font_size: 14.0,
                            ..default()
                        },
                        TextShadow::default()
                    ),
                    (PlayerBuffText, Text::new(""), TextShadow::default())
                ],
            ),
            (
                Node {
                    width: Val::Percent(50.0),
                    justify_content: JustifyContent::FlexEnd,
                    ..default()
                },
                Pickable::IGNORE,
                children![(GameOverCountDown, Text::new(""), TextShadow::default())],
            )
        ],
    ));

    commands.add_observer(on_player_turn_added);
    commands.add_observer(on_player_turn_removed);

    let deck_of_cards = commands
        .spawn((
            Transform::from_xyz(-90.0, 0.0, 0.0),
            InheritedVisibility::default(),
            ChildOf(state.0),
            Pickable::default(),
            GrowOnHover,
        ))
        .observe(click_deck)
        .id();

    for x in 0..8 {
        commands.spawn((
            Sprite {
                custom_size: Some(Vec2::new(DESIRED_CARD_WIDTH, DESIRED_CARD_HEIGHT)),
                ..Sprite::from_image(assets.card_back.clone())
            },
            Transform::from_xyz(x as f32, (x % 3) as f32 - 1.0, 0.01 * x as f32)
                .with_rotation(Quat::from_rotation_z(x as f32 / 60.0)),
            Pickable::default(),
            ChildOf(deck_of_cards),
        ));
    }

    commands
        .spawn((
            ChildOf(state.0),
            SlapTableButton,
            Sprite {
                custom_size: Some(Vec2::splat(DESIRED_CARD_WIDTH)),
                ..Sprite::from_image(assets.slap_table_sprite.clone())
            },
            Pickable::default(),
            Transform::from_xyz(90.0, 0.0, 0.0),
            GrowOnHover,
        ))
        .observe(
            |_: Trigger<Pointer<Click>>, mut client: ResMut<Transport>| {
                client.queue_claim(ClientClaim::SlapTable);
            },
        );
}

fn on_player_spawn(
    trigger: Trigger<OnAdd, PlayerId>,
    player_ids: Query<&PlayerId>,
    mut commands: Commands,
    mut client: ResMut<Transport>,
    mut window: Single<&mut Window, With<PrimaryWindow>>,
    assets: Res<GameAssets>,
) {
    let player_id = *player_ids.get(trigger.target()).unwrap();

    // Mark this player as the local player and send
    // a claim to update our username
    if player_id.peer_id == client.socket_id().unwrap() {
        commands.entity(trigger.target()).insert(MyPlayer);
        window.cursor_options.visible = false;
        let claim = ClientClaim::SetUserName {
            username: client.username(),
        };
        client.queue_claim(claim);
    }

    // spawn cursor
    commands.spawn((
        CursorIconFor(player_id),
        Sprite::from_image(assets.cursor_sprite.clone()),
        Transform::from_scale(Vec3::new(0.1, 0.1, 1.0))
            .with_translation(Vec3::new(0.0, 0.0, 100.0)),
    ));

    commands
        .entity(trigger.target())
        .with_child((
            PlayerNameText,
            Text2d::new(format!("Player {}", player_id.player_number())),
            TextFont::from_font_size(14.0),
            Transform::from_xyz(
                0.0,
                (DESIRED_CARD_HEIGHT + 15.0) * player_id.up_or_down(),
                1.0,
            ),
            TextColor(Color::WHITE),
        ))
        .observe(
            |trigger: Trigger<OnAdd, HasImmunity>,
             mut commands: Commands,
             assets: Res<GameAssets>| {
                commands.entity(trigger.target()).with_child((
                    Sprite::from_image(assets.locked_sprite.clone()),
                    Transform::from_translation(Vec3::new(0.0, 0.0, 10.0))
                        .with_scale(Vec3::splat(0.2)),
                ));
            },
        );
}

fn on_discard_pile_spawn(trigger: Trigger<OnAdd, DiscardPile>, mut commands: Commands) {
    commands.entity(trigger.target()).insert((
        Name::new("Discard Pile"),
        GrowOnHover,
        Transform::from_xyz(0.0, 0.0, 0.0),
        Sprite::from_color(
            Color::srgb(0.0, 0.0, 0.2),
            Vec2::new(DESIRED_CARD_WIDTH, DESIRED_CARD_HEIGHT),
        ),
        Pickable::default(),
    ));
}

fn sync_cursors_from_state(
    mut cursors: Query<(&mut Transform, &CursorIconFor)>,
    state: Single<&CambioState>,
    players: Query<&PlayerState>,
) {
    for (mut t, CursorIconFor(player_id)) in cursors.iter_mut() {
        if let Some(player) = state.player_index.get(player_id) {
            if let Ok(player_state) = players.get(*player) {
                t.translation.x = player_state.last_mouse_pos_world.x;
                t.translation.y = player_state.last_mouse_pos_world.y;
            }
        }
    }
}

fn hud_update_slap_button(
    mut commands: Commands,
    slap_table_button: Single<Entity, With<SlapTableButton>>,
    immunity: Query<&HasImmunity>,
    turn_state: Query<&TurnState, (With<MyPlayer>, Without<HasImmunity>)>,
) {
    // Revealed if it's at the start of your turn and nobody's done it yet
    if let Some(TurnState::Start) = turn_state.iter().next()
        && immunity.is_empty()
    {
        commands
            .entity(*slap_table_button)
            .insert(Visibility::Inherited);
    } else {
        commands
            .entity(*slap_table_button)
            .insert(Visibility::Hidden);
    }
}

fn hud_update_player_idx(
    me: Single<&PlayerId, With<MyPlayer>>,
    mut hud_text: Single<&mut Text, With<PlayerIdxText>>,
) {
    hud_text.0 = format!("You are player: {}", me.player_number(),);
}

fn hud_update_turn_buf(
    turn_state: Query<&TurnState, With<MyPlayer>>,
    extra: Query<&MayGiveCardTo, With<MyPlayer>>,
    mut buff_text: Single<&mut Text, With<PlayerBuffText>>,
) {
    let mut turn_description = "".to_string();
    if let Some(player_at_turn) = turn_state.iter().next() {
        turn_description = match player_at_turn {
            TurnState::Start => "Turn start",
            TurnState::TookDeckCard => "You took a card from the deck",
            TurnState::TookDiscardedCard => "You took a card from the discard pile",
            TurnState::SwappedCard => "You swapped a card",
            TurnState::Finished => "Turn end",
            TurnState::HasBuff(buff) => match buff {
                TurnBuff::MayLookAtOwnCard => "Buff! You may look at one of your cards",
                TurnBuff::MayLookAtOtherPlayersCard => "Buff! You may look at someone else's cards",
                TurnBuff::MaySwapTwoCards { .. } => "Buff! You may swap two cards",
                TurnBuff::MayLookAtCardAndThenSwap { .. } => {
                    "Buff! You make look at a card and then swap it with another"
                }
            },
        }
        .to_string();
    };

    // This buff always takes precedence
    if let Some(may_give_card_to) = extra.iter().next() {
        turn_description = format!(
            "You may give a card to player {}!",
            may_give_card_to.0.player_number()
        );
    }

    buff_text.0 = turn_description;
}

fn hud_game_finish_countdown(
    state: Single<&CambioState>,
    mut finish_countdown_text: Single<&mut Text, With<GameOverCountDown>>,
) {
    finish_countdown_text.0 = "".to_string();
    if let Some(timer) = state.game_will_finish_in.as_ref() {
        if !timer.finished() {
            finish_countdown_text.0 =
                format!("Game will end in {} seconds", timer.remaining_secs().ceil());
        }
    }
}

fn set_and_publish_cursor_position(
    mut me: Single<&mut PlayerState, With<MyPlayer>>,
    window: Single<&Window, With<PrimaryWindow>>,
    camera: Single<(&Camera, &GlobalTransform)>,
    mut client: ResMut<Transport>,
) {
    if let Some(cpos) = window.cursor_position() {
        if let Ok(world_pos) = camera.0.viewport_to_world_2d(camera.1, cpos) {
            me.last_mouse_pos_world = world_pos;
            client.queue_claim_unreliable(ClientClaimUnreliable::MousePosition(world_pos));
        }
    }
}

fn sync_held_by(mut held: Query<(&mut Transform, &IsHeldBy)>, players: Query<&PlayerState>) {
    for (mut transform, IsHeldBy(player_entity)) in held.iter_mut() {
        if let Ok(player_state) = players.get(*player_entity) {
            transform.translation.x = player_state.last_mouse_pos_world.x;
            transform.translation.y = player_state.last_mouse_pos_world.y;
        }
    }
}

fn click_slot_card(
    mut trigger: Trigger<Pointer<Click>>,
    me: Single<Entity, With<MyPlayer>>,
    cards: Query<&CardId>,
    is_holding: Query<&IsHoldingCard>,
    belongs_to_slot: Query<&BelongsToSlot>,
    slot_ids: Query<&SlotId>,
    mut client: ResMut<Transport>,
) {
    let Ok(card_id) = cards.get(trigger.target()) else {
        return;
    };

    let card_entity = trigger.target();

    if let Ok(BelongsToSlot(slot_entity)) = belongs_to_slot.get(card_entity) {
        let slot_id = slot_ids.get(*slot_entity).unwrap();
        match trigger.event().button {
            PointerButton::Primary => {
                if let Ok(is_holding) = is_holding.get(*me) {
                    let held_card_id = cards.get(is_holding.card_entity()).unwrap();
                    client.queue_claim(ClientClaim::SwapHeldCardWithSlotCard {
                        slot_id: *slot_id,
                        slot_card_id: *card_id,
                        held_card_id: *held_card_id,
                    });
                } else {
                    client.queue_claim(ClientClaim::PickUpSlotCard {
                        slot_id: *slot_id,
                        card_id: *card_id,
                    });
                }

                trigger.propagate(false);
            }
            PointerButton::Secondary => {
                client.queue_claim(ClientClaim::LookAtCardAtSlot {
                    slot_id: *slot_id,
                    card_id: *card_id,
                });

                trigger.propagate(false);
            }
            _ => {}
        }
    }
}

fn click_discard_pile(
    trigger: Trigger<Pointer<Click>>,
    discard_pile: Query<Entity, With<DiscardPile>>,
    state: Single<&CambioState>,
    me: Single<Entity, With<MyPlayer>>,
    card_ids: Query<&CardId>,
    is_holding: Query<&IsHoldingCard>,
    mut client: ResMut<Transport>,
) {
    if !discard_pile.contains(trigger.target()) {
        return;
    };

    if let Ok(is_holding) = is_holding.get(*me) {
        let card_id = card_ids.get(is_holding.card_entity()).unwrap();
        client.queue_claim(ClientClaim::DropCardOnDiscardPile { card_id: *card_id });
    } else {
        if state.discard_pile.len() >= 1 {
            client.queue_claim(ClientClaim::TakeCardFromDiscardPile {
                card_id: state.discard_pile[0],
            });
        }
    }
}

fn click_deck(_trigger: Trigger<Pointer<Click>>, mut client: ResMut<Transport>) {
    client.queue_claim(ClientClaim::TakeFreshCardFromDeck);
}

fn click_slot(
    mut trigger: Trigger<Pointer<Click>>,
    me: Single<Entity, With<MyPlayer>>,
    card_ids: Query<&CardId>,
    slot_ids: Query<&SlotId>,
    is_holding: Query<&IsHoldingCard>,
    mut client: ResMut<Transport>,
) {
    let Ok(slot_id) = slot_ids.get(trigger.target()) else {
        return;
    };

    let Ok(is_holding) = is_holding.get(*me) else {
        return;
    };
    let card_id = card_ids.get(is_holding.card_entity()).unwrap();
    client.queue_claim(ClientClaim::DropCardOnSlot {
        card_id: *card_id,
        slot_id: *slot_id,
    });
    trigger.propagate(false);
}

fn on_player_turn_added(
    trigger: Trigger<OnAdd, TurnState>,
    mut commands: Commands,
    players: Query<&PlayerId>,
    assets: Res<GameAssets>,
) {
    let Ok(player_id) = players.get(trigger.target()) else {
        return;
    };

    for i in 0..2 {
        let left_or_right = if i == 0 { -1.0 } else { 1.0 };
        commands.entity(trigger.target()).with_child((
            PlayerTurnIcon,
            Transform::from_rotation(Quat::from_rotation_z(
                -std::f32::consts::PI / 2.0 * left_or_right,
            )),
            Sprite {
                custom_size: Some(Vec2::splat(20.0)),
                ..Sprite::from_image(assets.arrow_down_sprite.clone())
            },
            Animator::new(
                Tween::new(
                    EaseFunction::QuadraticInOut,
                    Duration::from_millis(500),
                    TransformPositionLens {
                        start: Vec3::new(70.0 * left_or_right, 75.0 * player_id.up_or_down(), 0.0),
                        end: Vec3::new(60.0 * left_or_right, 75.0 * player_id.up_or_down(), 0.0),
                    },
                )
                .with_repeat_strategy(RepeatStrategy::MirroredRepeat)
                .with_repeat_count(RepeatCount::Infinite),
            ),
            // drop shadow
            children![(
                Sprite {
                    color: Color::linear_rgba(0.0, 0.0, 0.0, 1.0),
                    custom_size: Some(Vec2::splat(20.0)),
                    ..Sprite::from_image(assets.arrow_down_sprite.clone())
                },
                Transform::from_xyz(1.0, -1.0, -1.0),
            )],
        ));
    }
}

fn on_player_turn_removed(
    trigger: Trigger<OnRemove, TurnState>,
    mut commands: Commands,
    children: Query<&Children>,
    icon: Query<Entity, With<PlayerTurnIcon>>,
) {
    for child in children.iter_descendants(trigger.target()) {
        if let Ok(icon_entity) = icon.get(child) {
            commands.entity(icon_entity).despawn();
        }
    }
}

fn on_player_state_change(
    q: Query<(Entity, &PlayerId, &PlayerState), Changed<PlayerState>>,
    mut text: Query<&mut Text2d, With<PlayerNameText>>,
    children: Query<&Children>,
) {
    for (entity, player_id, player_state) in q.iter() {
        for child in children.iter_descendants(entity) {
            if let Ok(mut text) = text.get_mut(child) {
                text.0 = format!(
                    "{} ({})",
                    player_state.username.clone(),
                    player_id.player_number()
                );
            }
        }
    }
}

fn effects_for_accepted_messages(
    mut commands: Commands,
    assets: Res<GameAssets>,
    me: Single<Entity, With<MyPlayer>>,
    player_ids: Query<(Entity, &PlayerId)>,
    players: Query<&PlayerState>,
    state: Single<&CambioState>,
    mut catched_up: Local<bool>,
    mut accepted: EventReader<AcceptedMessage>,
) {
    for AcceptedMessage(msg) in accepted.read() {
        match msg {
            ServerMessage::FinishedReplayingHistory { player_id } => {
                if player_id == player_ids.get(*me).unwrap().1 {
                    *catched_up = true;
                }
            }
            ServerMessage::ReceiveFreshCardFromDeck { card_id, .. } => {
                if *catched_up && let Some(card_entity) = state.card_index.get(card_id) {
                    commands
                        .entity(*card_entity)
                        .insert(Animator::new(Tween::new(
                            EaseFunction::CubicOut,
                            Duration::from_millis(500),
                            TransformScaleLens {
                                start: Vec3::ZERO,
                                end: Vec3::ONE,
                            },
                        )));
                }
            }
            ServerMessage::TakeFreshCardFromDeck { .. } => {
                if *catched_up {
                    commands.spawn((
                        AudioPlayer::new(assets.card_sweep.clone()),
                        PlaybackSettings::DESPAWN,
                    ));
                }
            }
            ServerMessage::SwapHeldCardWithSlotCard { slot_card_id, .. } => {
                if *catched_up {
                    commands.spawn((
                        AudioPlayer::new(assets.card_swap.clone()),
                        PlaybackSettings::DESPAWN,
                    ));

                    if let Some(card_entity) = state.card_index.get(slot_card_id) {
                        commands.entity(*card_entity).insert(Animator::new(
                            Tween::new(
                                EaseFunction::Linear,
                                Duration::from_millis(50),
                                TransformRotateZLens {
                                    start: -std::f32::consts::PI / 12.0,
                                    end: std::f32::consts::PI / 12.0,
                                },
                            )
                            .with_repeat_strategy(RepeatStrategy::MirroredRepeat)
                            .with_repeat_count(Duration::from_millis(175)),
                        ));
                    }
                }
            }
            ServerMessage::DropCardOnSlot { .. } | ServerMessage::DropCardOnDiscardPile { .. } => {
                if *catched_up {
                    commands.spawn((
                        AudioPlayer::new(assets.card_laydown.clone()),
                        PlaybackSettings::DESPAWN,
                    ));
                }
            }
            ServerMessage::ShuffleDiscardPile { .. } => {
                if *catched_up {
                    commands.spawn((
                        AudioPlayer::new(assets.vo_shuffle.clone()),
                        PlaybackSettings::DESPAWN,
                    ));
                }
            }
            ServerMessage::SlapTable { .. } => {
                if *catched_up {
                    commands.spawn((
                        AudioPlayer::new(assets.slap_table_sound.clone()),
                        PlaybackSettings::DESPAWN,
                    ));
                }
            }
            ServerMessage::PlayerDisconnected { player_id, .. } => {
                // the player is already not in the state anymore
                let player_entity = player_ids
                    .iter()
                    .find(|(_, id)| *id == player_id)
                    .unwrap()
                    .0;
                commands.entity(player_entity).with_child((
                    Sprite {
                        color: Color::linear_rgba(1.0, 1.0, 1.0, 0.8),
                        ..Sprite::from_image(assets.disconnected_sprite.clone())
                    },
                    Transform::from_xyz(0.0, 0.0, 15.0).with_scale(Vec3::splat(0.07)),
                ));
            }
            ServerMessage::GameFinished { final_scores, .. } => {
                if *catched_up {
                    commands.spawn((
                        AudioPlayer::new(assets.vo_finalscores.clone()),
                        PlaybackSettings::DESPAWN,
                    ));
                }

                let mut final_scores = final_scores.iter().collect::<Vec<_>>();
                final_scores.sort_by_key(|(_, score)| *score);

                commands
                    .spawn((
                        Node {
                            width: Val::Percent(100.0),
                            height: Val::Percent(100.0),
                            align_items: AlignItems::Center,
                            justify_content: JustifyContent::Center,
                            flex_direction: FlexDirection::Column,
                            ..default()
                        },
                        BackgroundColor(Color::srgba(0.0, 0.0, 0.0, 0.5)),
                    ))
                    .with_children(|parent| {
                        for (i, (player_id, score)) in final_scores.iter().enumerate() {
                            let player_entity = state.player_index.get(*player_id).unwrap();
                            let player_state = players.get(*player_entity).unwrap();
                            parent.spawn((
                                TextShadow::default(),
                                Text::new(format!(
                                    "{}: {} - {}",
                                    i + 1,
                                    player_state.username,
                                    score
                                )),
                                TextFont {
                                    font_size: 30.0,
                                    ..default()
                                },
                            ));
                        }
                    });
            }
            _ => (),
        }
    }
}

fn setup_connection_lost(
    mut commands: Commands,
    mut window: Single<&mut Window, With<PrimaryWindow>>,
) {
    window.cursor_options.visible = true;

    commands.spawn((
        Node {
            width: Val::Percent(100.0),
            height: Val::Percent(100.0),
            align_items: AlignItems::Center,
            justify_content: JustifyContent::Center,
            flex_direction: FlexDirection::Column,
            ..default()
        },
        BackgroundColor(Color::srgba(0.0, 0.0, 0.0, 0.6)),
        children![TextShadow::default(), Text::new("Connection Lost")],
    ));
}
