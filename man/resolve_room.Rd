% tinyrox says don't edit this manually, but it can't stop you!
\name{resolve_room}
\alias{resolve_room}
\title{Resolve a Matrix room name or id to an id}
\usage{
resolve_room(room, instance_dir = NULL, matrix_config = NULL, client = NULL)
}
\arguments{
\item{room}{Character or NULL.}

\item{instance_dir}{Character or NULL.}

\item{matrix_config}{Character or NULL. Explicit Matrix config path.}

\item{client}{A chat.api client, or NULL to build one from
\code{matrix_config} when a display name has to be looked up.}
}
\value{
Character room id.
}
\description{
See \code{\link{matrix_send}} for the resolution order.
}
