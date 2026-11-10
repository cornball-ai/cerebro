% tinyrox says don't edit this manually, but it can't stop you!
\name{sweep_segments}
\alias{sweep_segments}
\title{Fold stale conversation segments into topic rooms}
\usage{
sweep_segments(
  fold_after_days = 7L,
  max_segments = 12L,
  fold_min_days = 2L,
  topic_confidence = 0.7,
  unsorted_topic = "Unsorted",
  model,
  provider = NULL,
  room = NULL,
  matrix_config = NULL,
  instance_dir = NULL,
  space = NULL,
  invite = character(),
  dry_run = FALSE,
  client = NULL
)
}
\arguments{
\item{fold_after_days}{Integer. Idle days before a segment folds.}

\item{max_segments}{Integer. Segments allowed before pressure folds
the oldest.}

\item{fold_min_days}{Integer. Pressure never folds a segment idle
fewer days than this.}

\item{topic_confidence}{Numeric. Minimum classification confidence
to file into (or create) a topic; below it, folds go to
\code{unsorted_topic}.}

\item{unsorted_topic}{Character. Topic room for unclassified folds.}

\item{model}{Character. The classifier model id, as in
\code{llm.api::chat}. Required; there is no default.}

\item{provider}{Character or NULL. The classifier provider. NULL uses
\code{"anthropic"}.}

\item{room}{Character or NULL. Room for the sweep's summary and
new-topic notices (sent via \code{\link{matrix_send}}, so
\code{m.notice}); NULL sends none.}

\item{matrix_config}{Character or NULL. Explicit corteza Matrix JSON
config, as in \code{\link{matrix_send}}.}

\item{instance_dir}{Character or NULL. State cache for topic-room
resolution.}

\item{space}{Character or NULL. Name of a Matrix space to file new topic
rooms under, as in
\code{\link{sync_general_conversation_threads}}. Without one a topic
room created here has no members but the persona, so it is not
reachable at all; with one it appears in the space.}

\item{invite}{Character vector of Matrix user IDs, invited to
\code{space} once when it is created.}

\item{dry_run}{Logical. Report the plan without network writes.}

\item{client}{A chat.api client, or NULL to build one from
\code{matrix_config}. The transport needs durable channel state and
history (\code{chat_capabilities()$set_state} and \code{$history});
one without them errors on first use.}
}
\value{
Invisibly, a list with \code{folded} (list of
  \code{list(id, topic)}) and \code{skipped} (character ids).
}
\description{
The fold half of the conversation lifecycle. Segment rooms (opened by
\code{\link{segment_open}} when corteza clears a conversation, marked
by an \code{ai.cornball.lifecycle} state event with
\code{state: "segment"}) sink by recency and are
eventually folded: a root message carrying the segment's name and
archived-transcript pointer is posted to a topic room as an
\code{m.notice}, the segment's lifecycle event flips to
\code{state: "folded"} with a \code{folded_into} pointer, and the
segment room is tombstoned toward the topic room so stock clients
render the handoff.
}
\details{
Each fold also writes an \code{ai.cornball.fold} state event into the
topic room, keyed by the thread root and holding the segment room and
its archive pointer. That index is what lets a reply in the thread be
answered with the conversation it continues: one state read, rather
than a scan of the topic room's history for a root that sinks further
back with every later fold.

A segment folds when either trigger fires, both keyed to the last
\emph{human} message in the segment room (bot traffic keeps nothing
alive; a segment no one ever spoke in ages from its creation):
\itemize{
\item idle longer than \code{fold_after_days}, or
\item more than \code{max_segments} segments exist (sidebar
pressure) — the oldest fold first, but never one idle less than
\code{fold_min_days}.
}

Topic assignment never asks a question. Each fold is classified
against the existing topic rooms with a self-rated confidence: at or
above \code{topic_confidence} it files silently, a proposed new
topic that itself clears the bar is created and announced as a
notice (veto or rename at leisure), and everything else — low
confidence, unreadable replies, a provider outage — lands in
\code{unsorted_topic}, silently. The confidence is the model's own
estimate, ordinal rather than calibrated: the knob to turn when
misfiles annoy, not a probability.

Each segment folds independently; one failure logs and leaves the
rest of the sweep running. Segments whose activity cannot be dated
(no messages readable and no \code{since} on the state event) are
skipped with a message rather than folded on a guess.

}
