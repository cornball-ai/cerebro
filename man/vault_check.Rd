% tinyrox says don't edit this manually, but it can't stop you!
\name{vault_check}
\alias{vault_check}
\title{Run health checks for one or more pensar vaults}
\usage{
vault_check(
  vaults = NULL,
  room = NULL,
  notify = c("findings", "always", "never"),
  min_cluster_size = 3L,
  max_items = 5L,
  fail_on_findings = FALSE,
  dry_run = FALSE,
  instance_dir = NULL
)
}
\arguments{
\item{vaults}{Character vector or named list of vault paths. If NULL,
uses \code{<instance_dir>/vault} when present, otherwise
\code{<instance_dir>/wiki}.}

\item{room}{Matrix room name/id for notifications.}

\item{notify}{One of \code{"findings"}, \code{"always"}, or
\code{"never"}.}

\item{min_cluster_size}{Passed to \code{pensar::lint()}.}

\item{max_items}{Maximum example items per finding type in the report.}

\item{fail_on_findings}{Logical. If TRUE, throw an error when any vault
has findings or a lint error.}

\item{dry_run}{Logical. Print Matrix sends instead of sending.}

\item{instance_dir}{Character or NULL.}
}
\value{
A list with class \code{cerebro_vault_check}.
}
\description{
Runs \code{pensar::lint()} for each configured vault, summarizes
findings, and optionally sends the report to Matrix.
}
