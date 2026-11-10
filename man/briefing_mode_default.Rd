% tinyrox says don't edit this manually, but it can't stop you!
\name{briefing_mode_default}
\alias{briefing_mode_default}
\title{Default mode detector for briefings}
\usage{
briefing_mode_default(d = Sys.Date())
}
\arguments{
\item{d}{Date.}
}
\value{
Character scalar mode.
}
\description{
Maps a date to a mode string:
\itemize{
\item \code{quarterly}: first Monday of Jan/Apr/Jul/Oct
\item \code{monthly}: first Monday of any other month
\item \code{weekly}: any other Monday
\item \code{sunday}: Sunday (end-of-week retrospective)
\item \code{daily}: Tue-Sat
}
}
\details{
Override via \code{mode_fn} arg in \code{\link{briefing}} for instances
that want a different cadence.

}
