% tinyrox says don't edit this manually, but it can't stop you!
\name{fold_lookup}
\alias{fold_lookup}
\title{Find the archive a folded thread stands for}
\usage{
fold_lookup(chat, room_id, thread_root)
}
\arguments{
\item{chat}{A chat.api client.}

\item{room_id}{Character. The topic room the thread is in.}

\item{thread_root}{Character. The thread's root event id.}
}
\value{
\code{list(segment, vault)}: the folded segment room's id and
  the archive's path, relative to the vault root. NULL when the thread
  is not a fold (an ordinary thread someone started), the transport
  has no durable state, or the index could not be read, which is
  reported. Never an error: an unseeded reply is a worse conversation,
  a failed one is none.
}
\description{
corteza's \code{thread_context} hook. \code{\link{sweep_segments}}
folds a segment into a topic room as a thread and indexes it there,
as an \code{ai.cornball.fold} state event keyed by the thread's root
event id. A reply in that thread continues the archived conversation,
and this is where corteza asks which one. Read rather than derived,
and O(1): scanning the room's history for the root would grow with
the room, which is the work the index exists to avoid.
}
\examples{
cl <- chat.api::chat_loopback()
fold_lookup(cl, "topic", "$root") # NULL: nothing folded here yet
chat.api::chat_set_state(cl, "topic", "ai.cornball.fold",
                         list(segment = "!seg:ex",
                              vault = "raw/matrix/seg.md"),
                         state_key = "$root")
fold_lookup(cl, "topic", "$root")
}
