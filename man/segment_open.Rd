% tinyrox says don't edit this manually, but it can't stop you!
\name{segment_open}
\alias{segment_open}
\title{Open a segment room for a conversation that just ended}
\usage{
segment_open(chat, cfg, room_id, title, vault_ref = NULL)
}
\arguments{
\item{chat}{A chat.api client, the one corteza runs on.}

\item{cfg}{The bot config corteza loaded. \code{cfg$segment_rooms} is
the character vector of room ids whose conversations segment.}

\item{room_id}{Character. The room that was cleared.}

\item{title}{Character. The segment room's name; corteza derives it
from the conversation's opening line.}

\item{vault_ref}{Character or NULL. The archived transcript's path,
relative to the vault root, or NULL when nothing was archived.}
}
\value{
\code{list(id, name)} for the room created, invisibly, or
  NULL for none. An error from the room creation propagates: a
  segment the user is told about must exist, so corteza reports the
  failure and acknowledges the clear without one.
}
\description{
corteza's \code{on_clear} hook. When the cleared room is one the bot
config lists in \code{segment_rooms}, the ended conversation gets a
room of its own: a segment the user can reopen from the room list
while it is fresh. The room holds a notice pointing at the archived
transcript and the \code{ai.cornball.lifecycle} state event
(\code{state: "segment"}) that \code{\link{sweep_segments}} later folds
by. Everyone in the home room except the bot is invited.
}
\details{
A room not listed, or a transport that cannot create rooms or set
state, gets no segment, and corteza acknowledges the clear without
one.

}
\examples{
cl <- chat.api::chat_loopback()
seg <- segment_open(cl, list(segment_rooms = "home"), "home",
                    "Fold formula (2026-08-19)", "raw/matrix/seg.md")
chat.api::chat_get_state(cl, seg$id, "ai.cornball.lifecycle")$state
}
