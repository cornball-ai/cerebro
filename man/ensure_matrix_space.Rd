% tinyrox says don't edit this manually, but it can't stop you!
\name{ensure_matrix_space}
\alias{ensure_matrix_space}
\title{Ensure a Matrix space exists}
\usage{
ensure_matrix_space(
  name,
  topic = NULL,
  invite = character(),
  matrix_config = NULL,
  instance_dir = NULL,
  dry_run = FALSE,
  client = NULL
)
}
\arguments{
\item{name}{Non-empty space display name.}

\item{topic}{Character or NULL.}

\item{invite}{Character vector of Matrix user IDs, invited once at
creation.}

\item{matrix_config}{Character or NULL. Persona-specific Matrix config.}

\item{instance_dir}{Character or NULL. State cache location.}

\item{dry_run}{Logical. Report the intended action without network writes.}

\item{client}{A chat.api client, or NULL to build one from
\code{matrix_config}.}
}
\value{
The space room ID, or the name during a dry-run creation.
}
\description{
A space is a room that holds other rooms. One invite to the space
replaces an invite per room, which is the difference between a persona
that files a dozen topic rooms overnight and a readable morning.
}
\details{
A space cannot be converted from an ordinary room afterwards --
\code{type} is fixed in \code{m.room.create} -- so a name that already
resolves to a non-space room is an error rather than something to
repair in place.

}
