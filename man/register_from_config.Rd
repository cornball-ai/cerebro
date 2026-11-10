% tinyrox says don't edit this manually, but it can't stop you!
\name{register_from_config}
\alias{register_from_config}
\title{Register routines from an instance's config.yaml}
\usage{
register_from_config(instance_dir = NULL)
}
\arguments{
\item{instance_dir}{Character or NULL.}
}
\value{
Integer count of routines registered, invisibly.
}
\description{
Invoked by \code{\link{run}}; exported for tests and ad-hoc use. Each
\code{routines:} entry must have \code{name}, \code{schedule}, and a
\code{capability} naming one of the registered builders,
\code{names(cerebro:::.capabilities())}; a missing or unknown
capability is an error that lists the valid names. Entries with
\code{enabled: false} are skipped, as are entries their builder
declines with a message (an unknown briefing key, a shell routine
without a command). An optional \code{pre:} expression, or list of
expressions, is evaluated in the global environment before the
capability each time the routine fires; a failing \code{pre} is
reported and the capability still runs. Briefing entries reference a
key in the top-level \code{briefings:} map.
}
