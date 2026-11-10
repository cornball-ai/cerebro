% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_send}
\alias{matrix_send}
\title{Send a message to a chat room}
\usage{
matrix_send(
  text,
  room = NULL,
  dry_run = FALSE,
  instance_dir = NULL,
  matrix_config = NULL,
  markdown = TRUE,
  msgtype = "m.notice",
  client = NULL
)
}
\arguments{
\item{text}{Character. Message body.}

\item{room}{Character or NULL. Room id (\code{!...}) or display name.}

\item{dry_run}{Logical. If TRUE, prints to stdout and returns NULL
without sending.}

\item{instance_dir}{Character or NULL.}

\item{matrix_config}{Character or NULL. Explicit corteza Matrix JSON
config. NULL uses \code{CORTEZA_MATRIX_CONFIG} and then corteza's
default location.}

\item{markdown}{Logical. If TRUE, the body is rendered as markdown.}

\item{msgtype}{Character. Matrix msgtype. Defaults to \code{"m.notice"}:
everything cerebro sends through this wrapper is proactive persona
traffic (briefings, sync summaries, reconcile reports), which clients
render without a notification ping. Pass \code{"m.text"} for a message
that should notify.}

\item{client}{A chat.api client, or NULL to build one from
\code{matrix_config}. Supplying one is how tests run without a
homeserver.}
}
\value{
Event id of the sent message, or NULL on dry-run.
}
\description{
Sends through chat.api with two conveniences for capability scripts:
room name resolution (cached in instance state) and dry-run support.
}
\details{
Resolution order for \code{room}:
\enumerate{
\item If \code{room} starts with \code{!}, treated as a literal id.
\item Else looked up against
\code{state.json:matrix_rooms[[room]]}, the per-instance cache.
\item Else queried by display name against the bot's joined rooms;
hit cached on success.
\item Else falls back to \code{room_id} in the Matrix config (the
default DM).
}

}
