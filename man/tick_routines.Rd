% tinyrox says don't edit this manually, but it can't stop you!
\name{tick_routines}
\alias{tick_routines}
\title{Fire any routines whose next scheduled time has passed}
\usage{
tick_routines(now = Sys.time(), state_path = routines_state_path())
}
\arguments{
\item{now}{POSIXct. Current time (default \code{Sys.time()}).}

\item{state_path}{Character or NULL. Where to persist
\code{last_fired} state. NULL skips disk persistence.}
}
\value{
Integer count of routines fired this tick, invisibly.
}
\description{
Walks the registry. For each routine, if \code{now >= next_fire(parsed,
after = last_fired)}, runs the routine's function and updates
\code{last_fired}. Errors inside a routine are caught and printed; the
tick keeps going.
}
\details{
Routines whose \code{last_fired} is NA (just registered, no persisted
state) are watermarked to \code{now} on this tick without firing. They
fire on their next scheduled occurrence. Backfilling past-due
schedules on first sight would surprise users at startup, and
polling-based ticking can land tens of seconds after the scheduled
second, which the previous \code{now - 1} baseline would silently miss.

}
