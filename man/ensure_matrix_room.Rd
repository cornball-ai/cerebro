% tinyrox says don't edit this manually, but it can't stop you!
\name{ensure_matrix_room}
\alias{ensure_matrix_room}
\title{Ensure a persistent Matrix conversation room exists}
\usage{
ensure_matrix_room(
  name,
  topic = NULL,
  invite = character(),
  matrix_config = NULL,
  instance_dir = NULL,
  dry_run = FALSE,
  legacy_name = NULL,
  space = NULL,
  client = NULL
)
}
\arguments{
\item{name}{Non-empty room display name.}

\item{topic}{Character or NULL.}

\item{invite}{Character vector of Matrix user IDs. Ignored for a newly
created room when \code{space} is supplied: membership comes from the
space instead, which is the whole point of using one.}

\item{matrix_config}{Character or NULL. Persona-specific Matrix config.}

\item{instance_dir}{Character or NULL. State cache location.}

\item{dry_run}{Logical. Report the intended action without network writes.}

\item{legacy_name}{Character or NULL. Prior display name to look up when
\code{name} has no match; a hit is renamed to \code{name}.}

\item{space}{Character or NULL. A space room ID from
\code{\link{ensure_matrix_space}}. A newly created room is filed under
it and made joinable by its members rather than invited to
individually.}

\item{client}{A chat.api client, or NULL to build one from
\code{matrix_config}.}
}
\value{
The room ID, or the room name during a dry-run creation.
}
\description{
Reuses an exact display-name match or creates a private room, optionally
setting a topic and inviting users. When \code{name} has no match but
\code{legacy_name} does, the matched room is renamed to \code{name} and
reused. Topics beginning with a filesystem path
use corteza's existing \code{"<path> | <description>"} convention and
therefore scope that room's tools to the path.
}
