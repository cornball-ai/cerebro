#' Run a function at most once per local date
#'
#' Idempotency primitive for daily capabilities. Persists the last-fired
#' date per \code{key} in the instance state file. Subsequent calls on
#' the same date are no-ops unless \code{force = TRUE}.
#'
#' @param key Character. Unique key identifying the capability.
#' @param fn Function (no required args).
#' @param force Logical. Override the once-per-day guard.
#' @param instance_dir Character or NULL.
#' @return The function's return value, or invisible NULL when skipped.
#' @export
once_today <- function(key, fn, force = FALSE, instance_dir = NULL) {
    stopifnot(is.character(key), length(key) == 1L, nzchar(key))
    stopifnot(is.function(fn))
    today <- as.character(Sys.Date())
    state <- read_instance_state(instance_dir)
    fired <- state$once_today[[key]]
    if (!isTRUE(force) && identical(fired, today)) {
        message(sprintf("cerebro: %s already fired today; skipping", key))
        return(invisible(NULL))
    }
    out <- fn()
    state$once_today[[key]] <- today
    write_instance_state(state, instance_dir)
    invisible(out)
}
