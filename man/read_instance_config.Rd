% tinyrox says don't edit this manually, but it can't stop you!
\name{read_instance_config}
\alias{read_instance_config}
\title{Read an instance's config.yaml}
\usage{
read_instance_config(instance_dir = NULL)
}
\arguments{
\item{instance_dir}{Character or NULL.}
}
\value{
Parsed config list.
}
\description{
Resolves \code{$instance_dir/...} placeholders in string values to the
instance's absolute path. Returns the parsed list as-is otherwise.
}
