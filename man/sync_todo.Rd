% tinyrox says don't edit this manually, but it can't stop you!
\name{sync_todo}
\alias{sync_todo}
\title{Sync a todo subtree from a source repo into this instance}
\usage{
sync_todo(
  name = "sync",
  source_repo = NULL,
  source_root,
  target_root,
  source_path = NULL,
  target_path = NULL,
  period = "Week",
  room = NULL,
  pull = TRUE,
  force = FALSE,
  dry_run = FALSE,
  instance_dir = NULL,
  date = Sys.Date()
)
}
\arguments{
\item{name}{Character. Idempotency key + log label.}

\item{source_repo}{Character or NULL. Git repo to fast-forward pull
before mirroring (NULL to skip the pull).}

\item{source_root}{Directory holding the source week file.}

\item{target_root}{Directory holding this instance's week file.}

\item{source_path}{Non-empty source subtree breadcrumb.}

\item{target_path}{Non-empty target subtree breadcrumb.}

\item{period}{Character period label.}

\item{room}{Character or NULL. Matrix room for the summary.}

\item{pull}{Logical. Fast-forward pull \code{source_repo} first.}

\item{force}{Logical. Override once-per-day guard.}

\item{dry_run}{Logical. Print a unified diff of what would change and
write/send nothing (and never pull).}

\item{instance_dir}{Character or NULL.}

\item{date}{Date. For testing; defaults to today.}
}
\value{
The mirror plan (list of \code{added}/\code{restatused}),
  invisibly. NULL when skipped.
}
\description{
Pulls \code{source_repo} (fast-forward only, so a dirty or diverged
source never blocks the routine), then mirrors the \code{source_path}
subtree of the source's current week file down into \code{target_path}
of this instance's week file via the optional
\code{\link[hacer]{mirror_subtree}} integration. The source wins on shared
paths; nothing is deleted.
}
