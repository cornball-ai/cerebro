<!--
%\VignetteEngine{simplermarkdown::mdweave_to_html}
%\VignetteIndexEntry{Conversation lifecycle}
-->
---
title: Conversation lifecycle
---

# Conversation lifecycle

A chat surface is one rolling timeline. The conversations in it are
a graph: a thread about a trip, a thread about a bug, each picked up
and dropped over weeks. The lifecycle gives those conversations rooms
of their own while they are fresh, folds them into topic rooms when
they go quiet, and continues them when someone replies later.

Three parties implement it against this page:

- **corteza** runs the bot. At `/clear` it archives the conversation
  and, through a hook cerebro supplies, opens a segment room. When a
  message arrives in a thread, it asks cerebro which archived
  conversation the thread continues and seeds the session from it.
- **cerebro** owns the policy: when a segment folds, where it files,
  and the state events that record all of it. The fold runs as the
  `lifecycle_sweep` routine (`sweep_segments()`).
- **The client** renders rooms by their lifecycle state. Nothing here
  depends on a particular client; a stock Matrix client shows the
  rooms, the tombstone handoff, and the notices.

## The model

1. **Home room.** One rolling room per persona. The user talks there.
2. **Segment.** At `/clear`, the conversation that just ended gets a
   room of its own. Its first message is a notice carrying a pointer
   to the archived transcript, and it carries a lifecycle state event
   from the moment it exists. The sidebar is then rooms by recency: a
   fresh segment sits near the top and sinks as others arrive.
   Replying in a segment room continues that conversation.
3. **Fold.** A daily sweep folds stale segments into topic rooms. The
   fold posts a root notice in the topic room, writes an index entry
   keyed by that root, tombstones the segment room toward the topic
   room, and flips the segment's state to `folded`.
4. **Rehydrate.** A reply in the thread under a fold root continues the
   folded conversation: the bot reads the index entry for the root,
   loads the archived transcript, and seeds the session from its tail.

## State events

Both events are Matrix state events, written through
`chat.api::chat_set_state()` and read through `chat_get_state()`. A
transport without durable channel state (`chat_capabilities()$set_state`
is FALSE) has no lifecycle; the sweep errors rather than pretending.

### `ai.cornball.lifecycle`

State key `""`. Absence means the room is an ordinary room.

| `state` | Written by | Content |
|---|---|---|
| `segment` | cerebro's `segment_open()`, when corteza clears a conversation | `segment_of` (home room id), `vault` (archive path, relative to the vault root; absent when nothing was archived), `since` (ISO 8601 UTC) |
| `folded` | cerebro at fold | the segment fields unchanged, plus `folded_into` (`room`: topic room id, `thread`: root event id) and `folded_at` (ISO 8601 UTC). `since` stays the segment's own timestamp. |
| `topic` | cerebro when it creates a topic room | `topic` (the name at creation). The room's current display name is the label; the stored name is provenance and the fallback when the name cannot be read. Renaming the room is how a topic is corrected. |
| `parked` | reserved | `since`, `reason`. A generic demotion for rooms that are not segments. Nothing writes it yet. |

A folded segment room also gets `m.room.tombstone` with
`replacement_room` set to the topic room, so clients that know nothing
of the lifecycle still render the handoff.

### `ai.cornball.fold`

Written into the **topic room**, state key = the fold root's event id.
Content: `segment` (the segment room id) and `vault` (the archive path,
absent when the segment archived nothing).

This is the index rehydration reads: a reply carries its thread root,
and the root's entry names the archive. The alternative, scanning the
topic room's history for the root, grows with the room and sinks with
every later fold. The segment's own `folded_into` pointer is the
reverse mapping and is kept for clients.

The root notice itself carries the segment's name, the archive path,
and a `Segment: <room id>` line. That line is how a retried fold finds
the root it already posted, so a fold that failed after posting does
not post a second one.

## Fold policy

Both triggers key on the last **human** message in the segment room.
Bot traffic keeps nothing alive, and a room the human never spoke in
ages from its `since`. A segment with no datable activity at all is
skipped, never folded on a guess.

| Knob | Default | Meaning |
|---|---|---|
| `fold_after_days` | 7 | fold a segment idle longer than this |
| `max_segments` | 12 | sidebar pressure: above this many segments, the oldest fold first |
| `fold_min_days` | 2 | pressure never folds a segment touched this recently |
| `topic_confidence` | 0.7 | minimum classification confidence to file into, or create, a topic |

The knobs are fields on the `lifecycle_sweep` routine entry in the
instance's `config.yaml`, and arguments of `sweep_segments()`.

The state flip to `folded` is the commit point and comes last. Every
earlier step tolerates a rerun: the tombstone and the index are state
writes that overwrite themselves, and the root is found again by its
`Segment:` line.

## Topic assignment

The fold never asks a question. Each segment is classified against the
existing topic rooms with a self-rated confidence:

- at or above `topic_confidence`, it files silently;
- a proposed new topic that itself clears the bar is created and
  announced with a notice, so it can be renamed or vetoed at leisure;
- anything else (low confidence, an unreadable reply, a provider
  outage) files into the `Unsorted` topic, silently.

The confidence is the model's own estimate, ordinal rather than
calibrated. The knob is what to turn when misfiles annoy.

## Threads and encryption

Rehydration keys on a real `m.thread` relation. A rich reply
(`m.in_reply_to` without a thread relation) does not continue a fold;
it is an ordinary message in the topic room.

chat.api's Matrix adapter cannot send into a thread in an encrypted
room: the encrypted send path carries no relation, so the reply would
land in the main timeline. `chat_capabilities()$threads` is FALSE on an
e2ee client for that reason. Topic rooms are created unencrypted by
`ensure_matrix_room()`, which is what keeps the reply-in-thread flow
working; a topic room that is encrypted by hand loses it.

## What a client reads

To group and demote rooms by state, a client reads one state event per
room, `ai.cornball.lifecycle`, and switches on `state`:

- `segment`: a live conversation, ordered by recency with everything
  else;
- `folded`: demote or hide; `folded_into` says where it went, and the
  tombstone points the same way;
- `topic`: a durable room; its threads are folded conversations.

The fold root is an `m.notice`, so it renders without a notification
and reads as bookkeeping. Replying to it in a thread is how a user
reopens the conversation.
