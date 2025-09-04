use bevy::{platform::collections::HashMap, prelude::*};
use bevy_rand::{global::GlobalEntropy, prelude::WyRand};
use rand::seq::SliceRandom;
use strum::IntoEnumIterator;

use crate::{
    cambio::*,
    cards::{KnownCard, Rank, Suit},
    messages::*,
    transport::Transport,
    utils::Seq,
};

pub fn host_eval_event(
    In(msg): In<ServerMessage>,
    mut commands: Commands,
    state: Single<(Entity, &mut CambioState)>,
    mut entropy: GlobalEntropy<WyRand>,
) {
    let (root, mut state) = state.into_inner();

    // Make sure the server knowns all cards
    if state.card_lookup.0.is_empty() {
        let mut values: Vec<KnownCard> = Suit::iter()
            .flat_map(|suit| Rank::iter().map(move |rank| KnownCard { rank, suit }))
            .collect();

        // shuffle the cards
        values.shuffle(&mut entropy);

        state.card_lookup = CardValueLookup(
            values
                .into_iter()
                .enumerate()
                .map(|(i, card)| (CardId(i as u64), card))
                .collect(),
        );
    }

    fn accept_and_broadcast(
        msg: In<ServerMessage>,
        transport: ResMut<Transport>,
        state: Single<&CambioState>,
    ) {
        let Transport::Host {
            socket,
            accepted_history,
            ..
        } = transport.into_inner()
        else {
            panic!("impossible");
        };
        let peers = socket.connected_peers().collect::<Vec<_>>();

        let mut go = |msg: ServerMessage| {
            for peer_id in peers.iter() {
                let player_id = state
                    .player_index
                    .keys()
                    .find(|player_id| player_id.peer_id == *peer_id)
                    .unwrap();

                let packet = bincode::serde::encode_to_vec(
                    &msg.redacted_for(player_id),
                    bincode::config::standard(),
                )
                .unwrap()
                .into_boxed_slice();

                socket.channel_mut(RELIABLE_CHANNEL).send(packet, *peer_id);
            }
            accepted_history.push(msg.clone());
        };

        match *msg {
            ServerMessage::TakeFreshCardFromDeck { actor, card_id }
            | ServerMessage::RevealCardAtSlot { actor, card_id, .. } => {
                go(ServerMessage::PublishCardForPlayer {
                    player_id: actor,
                    card_id: card_id,
                    value: Some(state.card_lookup.0.get(&card_id).unwrap().clone()),
                });
            }
            ServerMessage::DropCardOnDiscardPile { card_id, .. } => {
                go(ServerMessage::PublishCardPublically {
                    card_id: card_id,
                    value: state.card_lookup.0.get(&card_id).unwrap().clone(),
                });
            }
            _ => (),
        }

        go(msg.clone());
    }

    commands.run_system_cached_with(
        process_single_event.pipe(
            |In(msg): In<Option<ServerMessage>>, mut commands: Commands| {
                if let Some(msg) = msg {
                    commands.run_system_cached_with(accept_and_broadcast, msg);
                }
            },
        ),
        (root, msg),
    );

    commands.run_system_cached_with(trigger_host_server_events, root);
}

pub fn setup_new_player(
    In(player_id): In<PlayerId>,
    mut commands: Commands,
    mut slot_seq: Local<Seq<u64>>,
    state: Single<&CambioState>,
    transport: ResMut<Transport>,
) {
    let Transport::Host {
        socket,
        accepted_history,
        ..
    } = transport.into_inner()
    else {
        panic!("impossible");
    };

    // Replay the history if the target is not the host itself
    if player_id.peer_id != socket.id().unwrap() {
        for msg in accepted_history.iter() {
            let packet = bincode::serde::encode_to_vec(
                &msg.redacted_for(&player_id),
                bincode::config::standard(),
            )
            .unwrap()
            .into_boxed_slice();
            socket
                .channel_mut(RELIABLE_CHANNEL)
                .send(packet, player_id.peer_id);
        }
    }

    commands.run_system_cached_with(
        host_eval_event,
        ServerMessage::PlayerConnected { player_id },
    );

    commands.run_system_cached_with(
        host_eval_event,
        ServerMessage::FinishedReplayingHistory {
            player_id: player_id,
        },
    );

    for i in 0..4 {
        let slot_id = SlotId(slot_seq.generate());

        commands.run_system_cached_with(
            host_eval_event,
            ServerMessage::ReceiveFreshSlot {
                actor: player_id,
                slot_id,
            },
        );

        commands.run_system_cached_with(give_card_to_player, (player_id, slot_id, i < 2));
    }

    if state.player_index.is_empty() {
        commands.run_system_cached_with(host_eval_event, ServerMessage::PlayerAtTurn { player_id });
    }
}

pub fn give_card_to_player(
    In((player_id, slot_id, reveal)): In<(PlayerId, SlotId, bool)>,
    mut commands: Commands,
    state: Single<(Entity, &CambioState)>,
) {
    let (_, state) = *state;
    let front_card = state.free_cards[0];
    commands.run_system_cached_with(
        host_eval_event,
        ServerMessage::ReceiveFreshCardFromDeck {
            actor: player_id,
            slot_id,
            card_id: front_card,
        },
    );

    if reveal {
        commands.run_system_cached_with(
            host_eval_event,
            ServerMessage::RevealCardAtSlot {
                actor: player_id,
                slot_id,
                card_id: front_card,
                check_turn: false,
            },
        );
    };
}

// Special host events that are triggered when a certain condition is met
fn trigger_host_server_events(
    root: In<Entity>,
    mut commands: Commands,
    states: Query<&CambioState>,
    player_at_turn: Query<(Entity, &PlayerAtTurn)>,
    players: Query<&PlayerState>,
    card_ids: Query<&CardId>,
    children: Query<&Children>,
    immunity: Query<&HasImmunity>,
) {
    let state = states.get(*root).unwrap();
    if state.free_cards.is_empty() {
        commands.run_system_cached_with(
            // TODO: actually update the card value lookup
            host_eval_event,
            ServerMessage::ShuffleDiscardPile {
                card_ids: state.discard_pile.iter().skip(1).cloned().collect(),
            },
        );
    }

    for (current_player_at_turn, turn_state) in player_at_turn.iter() {
        if *turn_state == PlayerAtTurn::Finished {
            println!("Moving to next player");
            let mut all_players = state.player_index.iter().collect::<Vec<_>>();
            all_players.sort_by_key(|(p, _)| p.player_number());

            let current_player_idx = all_players
                .iter()
                .position(|(_, pe)| **pe == current_player_at_turn)
                .unwrap();

            let next_player_idx = (current_player_idx + 1) % all_players.len();

            let (next_player_id, next_player) = all_players[next_player_idx];

            let event = if immunity.contains(*next_player) {
                let mut final_score = HashMap::new();

                for (player_id, player_entity) in state.player_index.iter() {
                    let player_state = players.get(*player_entity).unwrap();
                    let mut score: i32 = 0;
                    for slot_entity in player_state.slots.iter() {
                        if let Some(card_id) = children
                            .iter_descendants(*slot_entity)
                            .filter_map(|c| card_ids.get(c).ok())
                            .next()
                        {
                            let known_card = state.card_lookup.0.get(card_id).unwrap();
                            score += known_card.penalty_score();
                        }
                    }
                    final_score.insert(*player_id, score);
                }

                ServerMessage::GameFinished {
                    all_cards: state.card_lookup.0.clone(),
                    final_scores: final_score,
                }
            } else {
                ServerMessage::PlayerAtTurn {
                    player_id: *next_player_id,
                }
            };

            commands.run_system_cached_with(host_eval_event, event);
        }
    }
}
