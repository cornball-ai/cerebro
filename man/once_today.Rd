% tinyrox says don't edit this manually, but it can't stop you!
\name{once_today}
\alias{once_today}
\title{Run a function at most once per local date}
\usage{
once_today(key, fn, force = FALSE, instance_dir = NULL)
}
\arguments{
\item{key}{Character. Unique key identifying the capability.}

\item{fn}{Function (no required args).}

\item{force}{Logical. Override the once-per-day guard.}

\item{instance_dir}{Character or NULL.}
}
\value{
The function's return value, or invisible NULL when skipped.
}
\description{
Idempotency primitive for daily capabilities. Persists the last-fired
date per \code{key} in the instance state file. Subsequent calls on
the same date are no-ops unless \code{force = TRUE}.
}
