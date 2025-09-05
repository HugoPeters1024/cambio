use std::time::Duration;

use bevy::prelude::*;
use bevy_rand::{global::GlobalEntropy, prelude::WyRand};
use rand::seq::SliceRandom;
use strum::IntoEnumIterator;

use crate::{
    cambio::*,
    cards::{KnownCard, Rank, Suit},
    messages::*,
    transport::Transport,
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

    commands.run_system_cached_with(
        process_single_event.pipe(|_: In<Option<ServerMessage>>| ()),
        (root, msg),
    );

    commands.run_system_cached_with(trigger_host_server_events, root);
}

pub fn setup_new_player(
    In(player_id): In<PlayerId>,
    mut commands: Commands,
    state: Single<&CambioState>,
    transport: ResMut<Transport>,
) {
    let Transport::Host(host) = transport.into_inner() else {
        panic!("impossible");
    };

    // Replay the history if the target is not the host itself
    if player_id.peer_id != host.socket.id().unwrap() {
        for msg in host.accepted_history.iter() {
            let packet = bincode::serde::encode_to_vec(
                &msg.redacted_for(&player_id),
                bincode::config::standard(),
            )
            .unwrap()
            .into_boxed_slice();
            host.socket
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
        let slot_id = SlotId(host.slot_seq.generate());

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

pub fn give_penalty_card(
    In(player_id): In<PlayerId>,
    mut commands: Commands,
    state: Single<&CambioState>,
    players: Query<&PlayerState>,
    transport: ResMut<Transport>,
    children: Query<&Children>,
    card_ids: Query<&CardId>,
    has_card: Query<&HasCard>,
) {
    let Transport::Host(host) = transport.into_inner() else {
        panic!("impossible");
    };

    let Some(player_entity) = state.player_index.get(&player_id) else {
        warn!("Player not found: {:?}", player_id);
        return;
    };

    let player = players.get(*player_entity).unwrap();

    let free_slot = player
        .slots
        .iter()
        .filter(|slot_id| {
            // Filter out any slots that have cards attached to them but are currently being held.
            let slot_entity = *state.slot_index.get(*slot_id).unwrap();
            !has_card.contains(slot_entity)
        })
        .find(|slot_id| {
            children
                .iter_descendants(*state.slot_index.get(*slot_id).unwrap())
                .filter_map(|c| card_ids.get(c).ok())
                .next()
                .is_none()
        });

    if let Some(free_slot) = free_slot {
        commands.run_system_cached_with(
            host_eval_event,
            ServerMessage::ReceiveFreshCardFromDeck {
                actor: player_id,
                slot_id: *free_slot,
                card_id: state.free_cards[0],
            },
        );
    } else {
        if player.slots.len() >= 8 {
            return;
        }

        let fresh_slot_id = host.slot_seq.generate();
        commands.run_system_cached_with(
            host_eval_event,
            ServerMessage::ReceiveFreshSlot {
                actor: player_id,
                slot_id: SlotId(fresh_slot_id),
            },
        );

        commands.run_system_cached_with(
            host_eval_event,
            ServerMessage::ReceiveFreshCardFromDeck {
                actor: player_id,
                slot_id: SlotId(fresh_slot_id),
                card_id: state.free_cards[0],
            },
        );
    }
}

pub fn give_card_to_player(
    In((player_id, slot_id, reveal)): In<(PlayerId, SlotId, bool)>,
    mut commands: Commands,
    state: Single<&CambioState>,
) {
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
    player_at_turn: Query<(Entity, &TurnState)>,
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
        if *turn_state == TurnState::Finished {
            let mut all_players = state.player_index.iter().collect::<Vec<_>>();
            all_players.sort_by_key(|(p, _)| p.player_number());

            let current_player_idx = all_players
                .iter()
                .position(|(_, pe)| **pe == current_player_at_turn)
                .unwrap();

            let next_player_idx = (current_player_idx + 1) % all_players.len();

            let (next_player_id, next_player) = all_players[next_player_idx];

            if immunity.contains(*next_player) && state.game_will_finish_in.is_none() {
                commands.run_system_cached_with(
                    host_eval_event,
                    ServerMessage::GameWillFinishIn(Duration::from_secs(5)),
                );
            } else {
                commands.run_system_cached_with(
                    host_eval_event,
                    ServerMessage::PlayerAtTurn {
                        player_id: *next_player_id,
                    },
                );
            };
        }
    }
}
