#' Boot a cerebro instance from its YAML config
#'
#' Reads \code{<instance_dir>/config.yaml}, registers each routine via
#' \code{\link{add_routine}}, restores any persisted \code{last_fired}
#' state, and ticks the scheduler in a loop.
#'
#' For backwards compatibility, also sources a \code{routines.R} file in
#' the instance dir if one exists (sourcing happens after YAML routines
#' are registered).
#'
#' When the instance enables Matrix (a \code{matrix:} block in
#' \code{config.yaml} with \code{enabled: true}, or \code{matrix = TRUE}),
#' the same loop also drives the corteza Matrix long-poll: each iteration
#' fires due routines, then runs one \code{corteza::bot_run_step()},
#' whose long-poll blocks until a message arrives or \code{tick_seconds}
#' elapses. One process serves both the chat bot and the scheduler. Left
#' off (the default), the loop is a plain ticker and routines reach a
#' separately-running bot via the signal-file APIs like
#' \code{corteza::bot_request_flush()}.
#'
#' The Matrix connection itself (homeserver, token, room) still comes
#' from corteza's own config (\code{CORTEZA_MATRIX_CONFIG} or its default
#' path); the \code{matrix:} block only toggles the loop and optionally
#' overrides \code{system}/\code{model}/\code{provider}/\code{tools_filter}
#' for the bot's turns.
#'
#' @param instance_dir Character. Path to the assistant instance dir.
#' @param tick_seconds Integer. Scheduler tick cadence. Also the default
#'   Matrix long-poll timeout when Matrix is enabled, unless the config's
#'   \code{matrix.timeout_ms} overrides it.
#' @param state_path Character or NULL. Where to persist
#'   \code{last_fired} for routines. Defaults to an instance-scoped
#'   file (\code{routines-<instance>.json}) so two instances on one
#'   machine don't clobber each other. NULL disables persistence.
#' @param matrix Logical or NULL. Whether to drive the corteza Matrix
#'   long-poll in this loop. NULL (default) reads \code{matrix$enabled}
#'   from the instance config.
#' @param renviron Character or NULL. Renviron file loaded via
#'   \code{readRenviron()} before routines register or run. Daemon
#'   frontends skip R's startup files (littler never reads
#'   \code{~/.Renviron}), so API keys living there would otherwise be
#'   invisible to a service. When not supplied, a top-level
#'   \code{renviron:} key in \code{config.yaml} wins, then a non-empty
#'   \code{R_ENVIRON_USER} environment variable (littler blanks it to
#'   skip startup files, so empty falls through), then
#'   \code{~/.Renviron}. Passing NULL, or setting \code{renviron: false}
#'   in config, skips loading. Values in the file override inherited
#'   environment variables.
#'
#' The loop ends on an interrupt and nothing else, so a service stops the
#' process with SIGINT. R has no handler for SIGTERM, and as a container's
#' PID 1 the process gets no default action for it either, so SIGTERM is
#' dropped and the runtime falls back to SIGKILL after its timeout. SIGINT
#' is R's own interrupt: it is raised at the next safe point (the Matrix
#' long-poll, the idle sleep, a routine's R code; a shell routine's child
#' finishes first), unwound through \code{on.exit} handlers, and caught by
#' the loop, so the process exits normally. That needs R's own signal
#' handlers, which \code{Rscript} installs and littler's \code{r} does
#' not (it sets \code{R_SignalHandlers = 0}): a service started with
#' \code{r -e} drops the signal outside \code{Sys.sleep()} and ends in
#' the runtime's SIGKILL after all.
#'
#' @return \code{NULL}, invisibly, once interrupted; until then it loops.
#' @export
run <- function(instance_dir, tick_seconds = 30L,
                state_path = routines_state_path(basename(instance_dir)),
                matrix = NULL, renviron = .default_renviron()) {
    instance_dir <- normalizePath(instance_dir, mustWork = TRUE)
    options(cerebro.instance_dir = instance_dir)
    Sys.setenv(CEREBRO_INSTANCE_DIR = instance_dir)
    suppressMessages(library(cerebro))

    config_file <- file.path(instance_dir, "config.yaml")
    cfg <- list()
    if (file.exists(config_file)) {
        message("cerebro: loading ", config_file)
        cfg <- read_instance_config(instance_dir)
    }

    # Load the Renviron between parsing the config (which needs no
    # credentials) and registering routines / sourcing routines.R, so
    # any eager code there already sees the keys.
    if (missing(renviron)) {
        renviron <- .resolve_renviron(cfg$renviron, renviron)
    }
    .load_renviron(renviron)

    if (file.exists(config_file)) {
        register_from_config(instance_dir = instance_dir)
    }

    routines_file <- file.path(instance_dir, "routines.R")
    if (file.exists(routines_file)) {
        message("cerebro: sourcing ", routines_file)
        sys.source(routines_file, envir = globalenv())
    }
    if (!file.exists(config_file) && !file.exists(routines_file)) {
        message("cerebro: no config.yaml or routines.R in ", instance_dir,
                " (running with whatever's already registered)")
    }

    if (!is.null(state_path)) {
        n <- load_routines_state(state_path)
        if (n > 0L) {
            message("cerebro: restored last_fired for ", n, " routine(s)")
        }
    }

    n_reg <- length(ls(.routines, all.names = TRUE))
    message("cerebro: starting scheduler with ", n_reg,
            " routine(s), tick every ", tick_seconds, "s")

    # Optionally fold the corteza Matrix long-poll into this loop, so a
    # single process serves both the scheduler and the chat bot. The
    # poll's timeout doubles as the scheduler tick: it blocks until a
    # message arrives or tick_seconds elapses, replacing the dead sleep.
    mstate <- NULL
    mtimeout <- tick_seconds * 1000L
    if (.matrix_enabled(matrix, cfg)) {
        mcfg <- cfg$matrix %||% list()
        mstate <- .start_matrix_loop(mcfg)
        # Apply identity only once the loop is live: a non-NULL mstate
        # means corteza is present and init succeeded, so the creds are
        # good and the homeserver answered. The upload rides corteza's
        # own client rather than opening a second one on the same device.
        if (!is.null(mstate)) {
            .apply_matrix_avatar(mcfg$avatar, instance_dir,
                                 client = mstate$chat)
        }
        mtimeout <- .matrix_timeout(mcfg, tick_seconds)
    }

    tryCatch(.run_loop(mstate, mtimeout, tick_seconds, state_path),
             interrupt = function(i) {
        message("cerebro: interrupted; stopping")
    })
    invisible(NULL)
}

# The scheduler loop: fire due routines, then wait out the tick in the
# Matrix long-poll when there is one and in a sleep otherwise. Errors
# in a step are reported and the loop goes on; only the interrupt run()
# catches ends it.
.run_loop <- function(mstate, mtimeout, tick_seconds, state_path) {
    repeat {
        tryCatch(tick_routines(state_path = state_path),
                 error = function(e) {
            message("cerebro: routine tick error: ", conditionMessage(e))
        })
        if (is.null(mstate)) {
            Sys.sleep(tick_seconds)
        } else {
            t0 <- proc.time()[["elapsed"]]
            replied <- tryCatch(
                                corteza::bot_run_step(mstate, timeout = mtimeout),
                                error = function(e) {
                message("cerebro: matrix step error: ", conditionMessage(e))
                -1L
            })
            # Floor the iteration rate so a poll that did no work can't
            # hot-spin: with timeout_ms = 0 a successful empty poll
            # returns instantly, and an errored poll (downed homeserver)
            # returns instantly too. Only back off when nothing was
            # handled -- an active conversation loops straight into the
            # next poll with no added latency.
            if (is.null(replied) || replied <= 0L) {
                idle <- proc.time()[["elapsed"]] - t0
                if (idle < .MATRIX_POLL_FLOOR) {
                    Sys.sleep(.MATRIX_POLL_FLOOR - idle)
                }
            }
        }
    }
}

# Default Renviron path: honor R_ENVIRON_USER only when non-empty.
# littler sets it to "" to suppress R's own user-file startup, so
# treating empty as set would silently disable loading under exactly
# the frontend this feature exists for.
.default_renviron <- function() {
    reu <- Sys.getenv("R_ENVIRON_USER")
    if (nzchar(reu)) {
        reu
    } else {
        "~/.Renviron"
    }
}

# Resolve run()'s Renviron path when the caller didn't pass one: a
# top-level `renviron:` key in config.yaml wins over the
# R_ENVIRON_USER / ~/.Renviron default. `renviron: false` disables
# loading.
.resolve_renviron <- function(cfg_value, default) {
    if (is.null(cfg_value)) {
        return(default)
    }
    if (isFALSE(cfg_value)) {
        return(NULL)
    }
    if (is.character(cfg_value) && length(cfg_value) == 1L &&
        nzchar(cfg_value)) {
        return(cfg_value)
    }
    stop("config renviron must be a single path or false", call. = FALSE)
}

# Load user-level environment variables (API keys) from an Renviron
# file. Values in the file override inherited environment variables
# (readRenviron semantics). Missing file is not an error: a container
# or CI run may legitimately supply keys through the environment.
.load_renviron <- function(path) {
    if (is.null(path) || !file.exists(path)) {
        return(invisible(FALSE))
    }
    readRenviron(path)
    message("cerebro: loaded ", path)
    invisible(TRUE)
}

# Decide whether to run the Matrix loop: an explicit `matrix` arg wins,
# otherwise read `matrix.enabled` from the instance config.
.matrix_enabled <- function(matrix, cfg) {
    if (is.null(matrix)) {
        isTRUE(cfg$matrix$enabled)
    } else {
        isTRUE(matrix)
    }
}

# Resolve the Matrix poll timeout in milliseconds: config
# `matrix.timeout_ms`, falling back to the scheduler tick. Validated as
# a non-negative integer.
.matrix_timeout <- function(mcfg, tick_seconds) {
    .config_integer(mcfg$timeout_ms, tick_seconds * 1000L,
                    "matrix.timeout_ms", min = 0L)
}

# Minimum seconds between Matrix polls that did no work. Bounds the loop
# rate so a low/zero timeout_ms, or an immediately-erroring poll, can't
# burn a core; a poll that handled messages skips the floor.
.MATRIX_POLL_FLOOR <- 1

# Build the corteza Matrix long-poll state, or NULL if it can't start.
# Kept separate so run()'s loop stays readable and the corteza coupling
# lives in one place.
.start_matrix_loop <- function(mcfg) {
    if (!requireNamespace("corteza", quietly = TRUE)) {
        message("cerebro: matrix enabled but 'corteza' is not installed; ",
                "running the scheduler only")
        return(NULL)
    }
    # The lifecycle's open half rides corteza's loop as hooks: the
    # segment room a /clear opens, and the archive a folded thread
    # continues. corteza runs the conversation; what becomes of it is
    # cerebro's.
    state <- tryCatch(
                      corteza::bot_run_init(system = mcfg$system,
            model = mcfg$model,
            provider = mcfg$provider,
            tools_filter = mcfg$tools_filter,
            hooks = list(on_clear = segment_open,
                         thread_context = fold_lookup)),
                      error = function(e) {
        message("cerebro: matrix init failed: ", conditionMessage(e))
        NULL
    })
    if (!is.null(state)) {
        message("cerebro: Matrix long-poll folded into the scheduler loop")
    }
    state
}

#' Register routines from an instance's config.yaml
#'
#' Invoked by \code{\link{run}}; exported for tests and ad-hoc use. Each
#' \code{routines:} entry must have \code{name}, \code{schedule}, and a
#' \code{capability} naming one of the registered builders,
#' \code{names(cerebro:::.capabilities())}; a missing or unknown
#' capability is an error that lists the valid names. Entries with
#' \code{enabled: false} are skipped, as are entries their builder
#' declines with a message (an unknown briefing key, a shell routine
#' without a command). An optional \code{pre:} expression, or list of
#' expressions, is evaluated in the global environment before the
#' capability each time the routine fires; a failing \code{pre} is
#' reported and the capability still runs. Briefing entries reference a
#' key in the top-level \code{briefings:} map.
#'
#' @param instance_dir Character or NULL.
#' @return Integer count of routines registered, invisibly.
#' @export
register_from_config <- function(instance_dir = NULL) {
    cfg <- read_instance_config(instance_dir)
    routines <- cfg$routines %||% list()
    briefings <- cfg$briefings %||% list()
    n <- 0L
    for (r in routines) {
        if (isFALSE(r$enabled)) {
            message("cerebro: routine ", r$name %||% "(unnamed)",
                    " disabled; skipping")
            next
        }
        fn <- .build_routine_fn(r, briefings, cfg, instance_dir)
        if (is.null(fn)) {
            next
        }
        add_routine(r$name, r$schedule, fn)
        n <- n + 1L
    }
    invisible(n)
}

.config_integer <- function(x, default, name, min = NULL) {
    if (is.null(x) || !length(x)) {
        return(as.integer(default))
    }
    if (length(x) != 1L) {
        value <- NA_real_
    } else {
        scalar <- x[[1L]]
        if (length(scalar) != 1L || is.logical(scalar)) {
            value <- NA_real_
        } else {
            value <- suppressWarnings(tryCatch(as.numeric(scalar),
                    error = function(e) NA_real_))
        }
    }
    if (!is.finite(value) || value != floor(value)) {
        stop("cerebro: ", name, " must be an integer", call. = FALSE)
    }
    out <- as.integer(value)
    if (!is.null(min) && out < min) {
        stop("cerebro: ", name, " must be at least ", min, call. = FALSE)
    }
    out
}

.config_number <- function(x, default, name, min = NULL, max = NULL) {
    if (is.null(x) || !length(x)) {
        return(as.numeric(default))
    }
    if (length(x) != 1L) {
        value <- NA_real_
    } else {
        scalar <- x[[1L]]
        if (length(scalar) != 1L || is.logical(scalar)) {
            value <- NA_real_
        } else {
            value <- suppressWarnings(tryCatch(as.numeric(scalar),
                    error = function(e) NA_real_))
        }
    }
    if (!is.finite(value)) {
        stop("cerebro: ", name, " must be a number", call. = FALSE)
    }
    if (!is.null(min) && value < min) {
        stop("cerebro: ", name, " must be at least ", min, call. = FALSE)
    }
    if (!is.null(max) && value > max) {
        stop("cerebro: ", name, " must be at most ", max, call. = FALSE)
    }
    value
}

# Prompts can be inline strings or `file: path/to/prompt.md` references.
.resolve_prompt <- function(p, instance_dir) {
    if (is.null(p)) {
        return("")
    }
    if (is.list(p) && !is.null(p$file)) {
        path <- p$file
        if (!startsWith(path, "/")) {
            path <- file.path(instance_dir(instance_dir), path)
        }
        if (!file.exists(path)) {
            stop("prompt file not found: ", path, call. = FALSE)
        }
        return(paste(readLines(path, warn = FALSE), collapse = "\n"))
    }
    as.character(p)
}
