% tinyrox says don't edit this manually, but it can't stop you!
\name{send_prepared_briefing}
\alias{send_prepared_briefing}
\title{Send a prepared briefing}
\usage{
send_prepared_briefing(
  name,
  room = NULL,
  force = FALSE,
  dry_run = FALSE,
  instance_dir = NULL,
  date = Sys.Date()
)
}
\arguments{
\item{name}{Character. Prepared briefing key.}

\item{room}{Character or NULL. Matrix room name or id.}

\item{force}{Logical. Override the once-per-day send guard.}

\item{dry_run}{Logical. Print instead of sending.}

\item{instance_dir}{Character or NULL.}

\item{date}{Date. Required draft date; defaults to today.}
}
\value{
The prepared text, invisibly. NULL when no valid draft exists or the
  send was already completed.
}
\description{
Sends only a non-empty draft whose stored date matches \code{date}. Missing
or stale drafts are refused rather than regenerated or sent.
}
