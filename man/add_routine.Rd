% tinyrox says don't edit this manually, but it can't stop you!
\name{add_routine}
\alias{add_routine}
\title{Register a scheduled routine}
\usage{
add_routine(name, schedule, fn)
}
\arguments{
\item{name}{Character. Unique name for the routine.}

\item{schedule}{Character. Cadence string: \code{"hourly"},
\code{"daily HH:MM"}, \code{"DAY HH:MM"}, or
\code{"DAY1-DAY2 HH:MM"} (DAY in Mon..Sun); or five-field cron
as Kubernetes CronJob and Vixie cron spell it,
\code{"minute hour day-of-month month day-of-week"}, e.g.
\code{"*/30 * * * *"} or \code{"0,30 9-17 * * Mon-Fri"}. Fields
take \code{*}, values, lists, ranges and \code{/step}; months and
weekdays accept names; Sunday is 0 or 7. With both day fields
restricted, either matching fires (Vixie's rule). The named forms
cannot express anything sub-hourly; cron can.}

\item{fn}{Function with no required arguments. The action to run.}
}
\value{
The routine entry, invisibly.
}
\description{
Adds an R callback to the routine registry. The callback fires on the
given schedule when \code{\link{tick_routines}} is called (typically
from inside the \code{\link{run}} loop).
}
\details{
Two ways to register:
\itemize{
\item Persistent: write \code{add_routine(...)} calls in a
\code{routines.R} file inside your instance directory.
\code{\link{run}} sources this file at startup.
\item Ad-hoc: call \code{add_routine(...)} from a live R session.
Useful for testing; gone on restart.
}

}
