# Persist routine `last_fired` timestamps so a restart picks up where
# it left off rather than firing every routine on boot.

#' Path to the cerebro routines state file
#'
#' Resolves under \code{$CEREBRO_STATE_DIR} when set (useful for
#' tests), else \code{tools::R_user_dir("cerebro", "data")/state}.
#' (\code{R_user_dir} only accepts data/config/cache, hence the subdir.)
#' With an \code{instance} name the file is
#' \code{routines-<instance>.json}, so instances don't clobber each
#' other's \code{last_fired}; without one it is the legacy shared
#' \code{routines.json}.
#'
#' @param instance Character or NULL. Instance name (usually
#'   \code{basename(instance_dir)}) to scope the state file per
#'   instance.
#' @return Character path. Created lazily when first written.
#' @export
routines_state_path <- function(instance = NULL) {
    env <- Sys.getenv("CEREBRO_STATE_DIR", "")
    dir <- if (nzchar(env)) {
        env
    } else {
        file.path(tools::R_user_dir("cerebro", "data"), "state")
    }
    fname <- if (is.null(instance) || !nzchar(instance)) {
        "routines.json"
    } else {
        sprintf("routines-%s.json", instance)
    }
    file.path(dir, fname)
}

# Write last_fired for every routine in the registry.
save_routines_state <- function(path = routines_state_path()) {
    if (!dir.exists(dirname(path))) {
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    }
    state <- list()
    for (n in ls(.routines, all.names = TRUE)) {
        e <- .routines[[n]]
        if (!is.na(e$last_fired)) {
            state[[n]] <- list(last_fired = unname(e$last_fired))
        }
    }
    json <- jsonlite::toJSON(state, auto_unbox = TRUE, pretty = TRUE,
                             null = "null")
    writeLines(json, path)
    invisible(path)
}

# Load last_fired into the registry. Routines not present in the
# registry are silently ignored.
load_routines_state <- function(path = routines_state_path()) {
    if (!file.exists(path)) {
        return(invisible(0L))
    }
    state <- tryCatch(
                      jsonlite::fromJSON(path, simplifyVector = TRUE,
            simplifyDataFrame = FALSE),
                      error = function(e) {
        message("cerebro: state file unreadable, ignoring: ",
                conditionMessage(e))
        list()
    }
    )
    loaded <- 0L
    for (n in names(state)) {
        if (!exists(n, envir = .routines, inherits = FALSE)) {
            next
        }
        e <- .routines[[n]]
        e$last_fired <- as.numeric(state[[n]]$last_fired %||% NA_real_)
        .routines[[n]] <- e
        loaded <- loaded + 1L
    }
    invisible(loaded)
}
