# Registry of scheduled routines. Mutable in-memory env, optionally
# backed by a JSON file at <state_dir>/routines.json that records
# `last_fired` per routine name so a restart picks up where it left off.

.routines <- new.env(parent = emptyenv())

#' Register a scheduled routine
#'
#' Adds an R callback to the routine registry. The callback fires on the
#' given schedule when \code{\link{tick_routines}} is called (typically
#' from inside the \code{\link{run}} loop).
#'
#' Two ways to register:
#' \itemize{
#'   \item Persistent: write \code{add_routine(...)} calls in a
#'     \code{routines.R} file inside your instance directory.
#'     \code{\link{run}} sources this file at startup.
#'   \item Ad-hoc: call \code{add_routine(...)} from a live R session.
#'     Useful for testing; gone on restart.
#' }
#'
#' @param name Character. Unique name for the routine.
#' @param schedule Character. Cadence string: \code{"hourly"},
#'   \code{"daily HH:MM"}, \code{"DAY HH:MM"}, or
#'   \code{"DAY1-DAY2 HH:MM"} (DAY in Mon..Sun); or five-field cron
#'   as Kubernetes CronJob and Vixie cron spell it,
#'   \code{"minute hour day-of-month month day-of-week"}, e.g.
#'   \code{"*/30 * * * *"} or \code{"0,30 9-17 * * Mon-Fri"}. Fields
#'   take \code{*}, values, lists, ranges and \code{/step}; months and
#'   weekdays accept names; Sunday is 0 or 7. With both day fields
#'   restricted, either matching fires (Vixie's rule). The named forms
#'   cannot express anything sub-hourly; cron can.
#' @param fn Function with no required arguments. The action to run.
#'
#' @return The routine entry, invisibly.
#' @export
add_routine <- function(name, schedule, fn) {
    stopifnot(is.character(name), length(name) == 1L, nzchar(name))
    stopifnot(is.character(schedule), length(schedule) == 1L)
    stopifnot(is.function(fn))
    parsed <- parse_schedule(schedule)
    entry <- list(name = name, schedule = schedule, parsed = parsed,
                  fn = fn, last_fired = NA_real_)
    .routines[[name]] <- entry
    invisible(entry)
}

#' List registered routines
#'
#' @return A data.frame with columns: name, schedule, last_fired, next_fire.
#' @export
list_routines <- function() {
    names_ <- ls(.routines, all.names = TRUE)
    if (length(names_) == 0L) {
        return(data.frame(name = character(), schedule = character(),
                          last_fired = as.POSIXct(character()),
                          next_fire = as.POSIXct(character()),
                          stringsAsFactors = FALSE))
    }
    rows <- lapply(names_, function(n) {
        e <- .routines[[n]]
        lf <- if (is.na(e$last_fired)) {
            NA_real_
        } else {
            e$last_fired
        }
        nf <- next_fire(e$parsed,
                        after = if (is.na(lf)) {
                Sys.time()
            } else {
                as.POSIXct(lf, origin = "1970-01-01")
            })
        data.frame(name = e$name,
                   schedule = e$schedule,
                   last_fired = .as_posix(lf),
                   next_fire = nf,
                   stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
}

#' Remove a routine from the registry
#'
#' @param name Character. Name passed to \code{\link{add_routine}}.
#' @return TRUE if the routine was present, FALSE otherwise. Invisible.
#' @export
remove_routine <- function(name) {
    if (!exists(name, envir = .routines, inherits = FALSE)) {
        return(invisible(FALSE))
    }
    rm(list = name, envir = .routines)
    invisible(TRUE)
}

#' Fire any routines whose next scheduled time has passed
#'
#' Walks the registry. For each routine, if \code{now >= next_fire(parsed,
#' after = last_fired)}, runs the routine's function and updates
#' \code{last_fired}. Errors inside a routine are caught and printed; the
#' tick keeps going.
#'
#' Routines whose \code{last_fired} is NA (just registered, no persisted
#' state) are watermarked to \code{now} on this tick without firing. They
#' fire on their next scheduled occurrence. Backfilling past-due
#' schedules on first sight would surprise users at startup, and
#' polling-based ticking can land tens of seconds after the scheduled
#' second, which the previous \code{now - 1} baseline would silently miss.
#'
#' @param now POSIXct. Current time (default \code{Sys.time()}).
#' @param state_path Character or NULL. Where to persist
#'   \code{last_fired} state. NULL skips disk persistence.
#'
#' @return Integer count of routines fired this tick, invisibly.
#' @export
tick_routines <- function(now = Sys.time(),
                          state_path = routines_state_path()) {
    fired <- 0L
    initialized <- 0L
    for (n in ls(.routines, all.names = TRUE)) {
        e <- .routines[[n]]
        if (is.na(e$last_fired)) {
            e$last_fired <- as.numeric(now)
            .routines[[n]] <- e
            initialized <- initialized + 1L
            next
        }
        baseline <- as.POSIXct(e$last_fired, origin = "1970-01-01")
        nf <- next_fire(e$parsed, after = baseline)
        if (now >= nf) {
            tryCatch(e$fn(),
                     error = function(err) {
                message(sprintf("[routine %s] error: %s", n,
                                conditionMessage(err)))
            })
            e$last_fired <- as.numeric(now)
            .routines[[n]] <- e
            fired <- fired + 1L
        }
    }
    if (!is.null(state_path) && (fired > 0L || initialized > 0L)) {
        save_routines_state(state_path)
    }
    invisible(fired)
}

# Internal: empty the registry. For tests.
.clear_routines <- function() {
    rm(list = ls(.routines, all.names = TRUE), envir = .routines)
    invisible(NULL)
}

# Internal: set last_fired for a routine. For tests; routines update
# their own last_fired through tick_routines() under normal use.
.set_last_fired <- function(name, when) {
    if (!exists(name, envir = .routines, inherits = FALSE)) {
        return(invisible(FALSE))
    }
    e <- .routines[[name]]
    e$last_fired <- as.numeric(when)
    .routines[[name]] <- e
    invisible(TRUE)
}

# Internal: read last_fired (numeric or NA). For tests.
.get_last_fired <- function(name) {
    if (!exists(name, envir = .routines, inherits = FALSE)) {
        return(NA_real_)
    }
    .routines[[name]]$last_fired
}

.as_posix <- function(x) {
    if (is.na(x)) {
        return(as.POSIXct(NA))
    }
    as.POSIXct(x, origin = "1970-01-01")
}
