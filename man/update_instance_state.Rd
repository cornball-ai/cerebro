% tinyrox says don't edit this manually, but it can't stop you!
\name{update_instance_state}
\alias{update_instance_state}
\title{Update one or more keys in an instance's state.json}
\usage{
update_instance_state(..., instance_dir = NULL)
}
\arguments{
\item{...}{Named arguments: \code{key = value}.}

\item{instance_dir}{Character or NULL.}
}
\value{
Updated state, invisibly.
}
\description{
Read-modify-write convenience. Pass \code{NULL} as a value to remove a
key.
}
