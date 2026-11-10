% tinyrox says don't edit this manually, but it can't stop you!
\name{instantiate_agent}
\alias{instantiate_agent}
\title{Create or activate a named cerebro agent instance}
\usage{
instantiate_agent(name, root = "~", create = TRUE, set_active = TRUE)
}
\arguments{
\item{name}{Character. Agent/instance name under \code{root}.}

\item{root}{Character. Parent directory for named agents.}

\item{create}{Logical. Create the standard directories.}

\item{set_active}{Logical. Set option/env so this becomes the active
cerebro instance for the current R process.}
}
\value{
Named list from \code{\link{agent_paths}}.
}
\description{
Creates the standard directory layout used by instance-relative
briefing defaults: \code{todo/this_week}, \code{todo/archive},
\code{wiki/wiki}, and \code{wiki/raw}.
}
