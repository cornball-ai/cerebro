% tinyrox says don't edit this manually, but it can't stop you!
\name{run}
\alias{run}
\title{Boot a cerebro instance from its YAML config}
\usage{
run(
  instance_dir,
  tick_seconds = 30L,
  state_path = routines_state_path(basename(instance_dir)),
  matrix = NULL,
  renviron = .default_renviron()
)
}
\arguments{
\item{instance_dir}{Character. Path to the assistant instance dir.}

\item{tick_seconds}{Integer. Scheduler tick cadence. Also the default
Matrix long-poll timeout when Matrix is enabled, unless the config's
\code{matrix.timeout_ms} overrides it.}

\item{state_path}{Character or NULL. Where to persist
\code{last_fired} for routines. Defaults to an instance-scoped
file (\code{routines-<instance>.json}) so two instances on one
machine don't clobber each other. NULL disables persistence.}

\item{matrix}{Logical or NULL. Whether to drive the corteza Matrix
long-poll in this loop. NULL (default) reads \code{matrix$enabled}
from the instance config.}

\item{renviron}{Character or NULL. Renviron file loaded via
\code{readRenviron()} before routines register or run. Daemon
frontends skip R's startup files (littler never reads
\code{~/.Renviron}), so API keys living there would otherwise be
invisible to a service. When not supplied, a top-level
\code{renviron:} key in \code{config.yaml} wins, then a non-empty
\code{R_ENVIRON_USER} environment variable (littler blanks it to
skip startup files, so empty falls through), then
\code{~/.Renviron}. Passing NULL, or setting \code{renviron: false}
in config, skips loading. Values in the file override inherited
environment variables.}
}
\value{
Never returns under normal use; loops until interrupted.
}
\description{
Reads \code{<instance_dir>/config.yaml}, registers each routine via
\code{\link{add_routine}}, restores any persisted \code{last_fired}
state, and ticks the scheduler in a loop.
}
\details{
For backwards compatibility, also sources a \code{routines.R} file in
the instance dir if one exists (sourcing happens after YAML routines
are registered).

When the instance enables Matrix (a \code{matrix:} block in
\code{config.yaml} with \code{enabled: true}, or \code{matrix = TRUE}),
the same loop also drives the corteza Matrix long-poll: each iteration
fires due routines, then runs one \code{corteza::bot_run_step()},
whose long-poll blocks until a message arrives or \code{tick_seconds}
elapses. One process serves both the chat bot and the scheduler. Left
off (the default), the loop is a plain ticker and routines reach a
separately-running bot via the signal-file APIs like
\code{corteza::bot_request_flush()}.

The Matrix connection itself (homeserver, token, room) still comes
from corteza's own config (\code{CORTEZA_MATRIX_CONFIG} or its default
path); the \code{matrix:} block only toggles the loop and optionally
overrides \code{system}/\code{model}/\code{provider}/\code{tools_filter}
for the bot's turns.

}
