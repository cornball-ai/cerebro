% tinyrox says don't edit this manually, but it can't stop you!
\name{dispatch_args}
\alias{dispatch_args}
\title{Parse CLI flags from argv / commandArgs / env vars}
\usage{
dispatch_args(args = NULL)
}
\arguments{
\item{args}{Character vector. Defaults to \code{argv} (littler) or
\code{commandArgs(trailingOnly = TRUE)}.}
}
\value{
A list with \code{force}, \code{dry_run}, \code{instance_dir}.
}
\description{
Common flags every capability script wants. Recognizes
\code{--force}, \code{--dry-run}, and their env equivalents
(\code{CEREBRO_FORCE}, \code{CEREBRO_DRY_RUN}). Also supports
\code{--instance <path>} / \code{CEREBRO_INSTANCE_DIR}.
}
