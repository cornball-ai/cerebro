% tinyrox says don't edit this manually, but it can't stop you!
\name{instance_dir}
\alias{instance_dir}
\title{Resolve the active instance directory}
\usage{
instance_dir(instance_dir = NULL)
}
\arguments{
\item{instance_dir}{Character or NULL.}
}
\value{
Normalized absolute path.
}
\description{
Order: \code{instance_dir} arg, \code{getOption("cerebro.instance_dir")},
env \code{CEREBRO_INSTANCE_DIR}. Errors if none of those resolve to an
existing directory.
}
