% tinyrox says don't edit this manually, but it can't stop you!
\name{routines_state_path}
\alias{routines_state_path}
\title{Path to the cerebro routines state file}
\usage{
routines_state_path(instance = NULL)
}
\arguments{
\item{instance}{Character or NULL. Instance name (usually
\code{basename(instance_dir)}) to scope the state file per
instance.}
}
\value{
Character path. Created lazily when first written.
}
\description{
Resolves under \code{$CEREBRO_STATE_DIR} when set (useful for
tests), else \code{tools::R_user_dir("cerebro", "data")/state}.
(\code{R_user_dir} only accepts data/config/cache, hence the subdir.)
With an \code{instance} name the file is
\code{routines-<instance>.json}, so instances don't clobber each
other's \code{last_fired}; without one it is the legacy shared
\code{routines.json}.
}
