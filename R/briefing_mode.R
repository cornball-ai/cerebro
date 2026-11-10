#' Default mode detector for briefings
#'
#' Maps a date to a mode string:
#' \itemize{
#'   \item \code{quarterly}: first Monday of Jan/Apr/Jul/Oct
#'   \item \code{monthly}: first Monday of any other month
#'   \item \code{weekly}: any other Monday
#'   \item \code{sunday}: Sunday (end-of-week retrospective)
#'   \item \code{daily}: Tue-Sat
#' }
#'
#' Override via \code{mode_fn} arg in \code{\link{briefing}} for instances
#' that want a different cadence.
#'
#' @param d Date.
#' @return Character scalar mode.
#' @export
briefing_mode_default <- function(d = Sys.Date()) {
    lt <- as.POSIXlt(d)
    if (lt$wday == 0) {
        return("sunday")
    }
    if (lt$wday != 1) {
        return("daily")
    }
    mon <- lt$mon + 1L
    if (mon %in% c(1L, 4L, 7L, 10L) && lt$mday <= 7L) {
        return("quarterly")
    }
    if (lt$mday <= 7L) {
        return("monthly")
    }
    "weekly"
}
