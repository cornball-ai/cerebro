#' Run a briefing capability
#'
#' The orchestrator: select a mode, run the configured sources, assemble
#' a prompt, synthesize via llm.api, send via matrix. Idempotent per day
#' on \code{name}, with \code{force} / \code{dry_run} overrides.
#'
#' Sources are run in order. Each gets a shared \code{ctx} environment
#' so later sources can read state stashed by earlier ones (for
#' instance, \code{saber_git} reads \code{ctx$todo_text} populated by
#' \code{hacer_todos}).
#'
#' @param name Character. Idempotency key + log label.
#' @param room Character or NULL. Matrix room name or id (see
#'   \code{\link{matrix_send}}).
#' @param sources List of source specs. Each is a list with at least
#'   \code{type = "<source_name>"} plus the source's own params.
#' @param system_prompt Character. Base system prompt.
#' @param mode_hints Named list. mode -> text appended to system prompt.
#' @param mode_fn Function (Date) -> character. Defaults to
#'   \code{\link{briefing_mode_default}}.
#' @param model Character. LLM model id. Required; there is no default.
#' @param provider Character. LLM provider id.
#' @param max_tokens Integer.
#' @param force Logical. Override once-per-day guard.
#' @param dry_run Logical. Print synthesized briefing instead of sending.
#' @param instance_dir Character or NULL.
#' @param date Date. For testing; defaults to today.
#' @return The synthesized text, invisibly. NULL when skipped.
#' @export
briefing <- function(name, room = NULL, sources = list(), system_prompt,
                     mode_hints = list(), mode_fn = briefing_mode_default,
                     model, provider = "anthropic",
                     max_tokens = 800L, force = FALSE, dry_run = FALSE,
                     instance_dir = NULL, date = Sys.Date()) {
    .require_model(model)
    runner <- function() {
        reply <- .briefing_generate(name = name, sources = sources,
                                    system_prompt = system_prompt,
                                    mode_hints = mode_hints,
                                    mode_fn = mode_fn, model = model,
                                    provider = provider,
                                    max_tokens = max_tokens,
                                    instance_dir = instance_dir, date = date)

        if (isTRUE(dry_run)) {
            cat("=== BRIEFING ", name, " (dry-run) ===\n", reply, "\n",
                sep = "")
        } else if (!nzchar(trimws(reply %||% ""))) {
            warn <- sprintf(
                            "[briefing %s] empty response from %s/%s \u2014 check logs",
                            name, provider %||% "?", model %||% "?"
            )
            message("cerebro: ", warn)
            matrix_send(warn, room = room, instance_dir = instance_dir)
        } else {
            matrix_send(reply, room = room, instance_dir = instance_dir)
            message(sprintf("cerebro: briefing %s sent", name))
        }
        invisible(reply)
    }

    if (isTRUE(dry_run)) {
        invisible(runner())
    } else {
        once_today(paste0("briefing_", name), runner,
                   force = force, instance_dir = instance_dir)
    }
}

#' Prepare a briefing for later delivery
#'
#' Synthesizes a briefing immediately and stores the dated result in the
#' instance state. It does not send a Matrix message. A second call on the same
#' date is skipped unless \code{force} is true.
#'
#' @inheritParams briefing
#' @return The synthesized text, invisibly. NULL when skipped.
#' @export
prepare_briefing <- function(name, sources = list(), system_prompt,
                             mode_hints = list(),
                             mode_fn = briefing_mode_default,
                             model, provider = "anthropic",
                             max_tokens = 800L, force = FALSE,
                             dry_run = FALSE, instance_dir = NULL,
                             date = Sys.Date()) {
    .require_model(model)
    state <- read_instance_state(instance_dir)
    draft <- state$prepared_briefings[[name]]
    today <- as.character(date)
    if (!isTRUE(force) && identical(draft$date, today)) {
        message("cerebro: prepared briefing ", name, " already exists for ",
                today, "; skipping")
        return(invisible(NULL))
    }

    reply <- .briefing_generate(
                                name = name,
                                sources = sources,
                                system_prompt = system_prompt,
                                mode_hints = mode_hints,
                                mode_fn = mode_fn,
                                model = model,
                                provider = provider,
                                max_tokens = max_tokens,
                                instance_dir = instance_dir,
                                date = date
    )
    if (!nzchar(trimws(reply %||% ""))) {
        stop("cerebro: prepared briefing was empty", call. = FALSE)
    }
    if (isTRUE(dry_run)) {
        cat("=== PREPARED BRIEFING ", name, " (dry-run) ===\n",
            reply, "\n", sep = "")
        return(invisible(reply))
    }

    state <- read_instance_state(instance_dir)
    if (is.null(state$prepared_briefings)) {
        state$prepared_briefings <- list()
    }
    state$prepared_briefings[[name]] <- list(
        date = today,
        prepared_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        text = reply
    )
    write_instance_state(state, instance_dir)
    message("cerebro: prepared briefing ", name, " for ", today)
    invisible(reply)
}

#' Send a prepared briefing
#'
#' Sends only a non-empty draft whose stored date matches \code{date}. Missing
#' or stale drafts are refused rather than regenerated or sent.
#'
#' @param name Character. Prepared briefing key.
#' @param room Character or NULL. Matrix room name or id.
#' @param force Logical. Override the once-per-day send guard.
#' @param dry_run Logical. Print instead of sending.
#' @param instance_dir Character or NULL.
#' @param date Date. Required draft date; defaults to today.
#' @return The prepared text, invisibly. NULL when no valid draft exists or the
#'   send was already completed.
#' @export
send_prepared_briefing <- function(name, room = NULL, force = FALSE,
                                   dry_run = FALSE, instance_dir = NULL,
                                   date = Sys.Date()) {
    state <- read_instance_state(instance_dir)
    draft <- state$prepared_briefings[[name]]
    today <- as.character(date)
    if (is.null(draft) || !identical(draft$date, today) ||
        !nzchar(trimws(draft$text %||% ""))) {
        message("cerebro: no prepared briefing ", name, " for ", today,
                "; not sending")
        return(invisible(NULL))
    }

    runner <- function() {
        matrix_send(draft$text, room = room, dry_run = dry_run,
                    instance_dir = instance_dir)
        if (!isTRUE(dry_run)) {
            message("cerebro: prepared briefing ", name, " sent")
        }
        invisible(draft$text)
    }
    if (isTRUE(dry_run)) {
        return(invisible(runner()))
    }
    once_today(paste0("prepared_briefing_", name), runner,
               force = force, instance_dir = instance_dir)
}

# Capability builders. The routine's `briefing:` key (default: its name)
# names an entry in the config's `briefings:` map. A `file:` prompt is
# read when the routine fires, so editing one needs no restart.
.cap_briefing <- function(r, briefings, cfg, instance_dir) {
    b <- .briefing_entry(r, briefings)
    if (is.null(b)) {
        return(NULL)
    }
    .require_model(b$cfg$model, sprintf("briefing '%s'", b$key))
    idir <- instance_dir
    function() {
        do.call(briefing, c(list(name = b$key, instance_dir = idir),
                            .briefing_args_from_cfg(b$cfg, idir)))
    }
}

.cap_briefing_prepare <- function(r, briefings, cfg, instance_dir) {
    b <- .briefing_entry(r, briefings)
    if (is.null(b)) {
        return(NULL)
    }
    .require_model(b$cfg$model, sprintf("briefing '%s'", b$key))
    idir <- instance_dir
    function() {
        args <- .briefing_args_from_cfg(b$cfg, idir)
        args$room <- NULL
        do.call(prepare_briefing,
                c(list(name = b$key, instance_dir = idir), args))
    }
}

.cap_briefing_send <- function(r, briefings, cfg, instance_dir) {
    b <- .briefing_entry(r, briefings)
    if (is.null(b)) {
        return(NULL)
    }
    room <- r$room %||% b$cfg$room %||% NULL
    idir <- instance_dir
    function() {
        send_prepared_briefing(name = b$key, room = room, instance_dir = idir)
    }
}

# The `briefings:` entry a routine points at, as list(key, cfg), or NULL
# with a message when the key is unknown.
.briefing_entry <- function(r, briefings) {
    key <- r$briefing %||% r$name
    bcfg <- briefings[[key]]
    if (is.null(bcfg)) {
        message("cerebro: routine ", r$name,
                " references unknown briefing '", key, "'; skipping")
        return(NULL)
    }
    list(key = key, cfg = bcfg)
}

.briefing_args_from_cfg <- function(bcfg, instance_dir) {
    sys_prompt <- .resolve_prompt(bcfg$system_prompt, instance_dir)
    args <- list(room = bcfg$room, sources = bcfg$sources %||% list(),
                 system_prompt = sys_prompt,
                 mode_hints = bcfg$mode_hints %||% list())
    if (!is.null(bcfg$model)) {
        args$model <- bcfg$model
    }
    if (!is.null(bcfg$provider)) {
        args$provider <- bcfg$provider
    }
    if (!is.null(bcfg$max_tokens)) {
        args$max_tokens <- as.integer(bcfg$max_tokens)
    }
    args
}

.briefing_generate <- function(name, sources, system_prompt, mode_hints,
                               mode_fn, model, provider, max_tokens,
                               instance_dir, date) {
    mode <- mode_fn(date)
    message(sprintf("cerebro: briefing %s mode=%s date=%s", name, mode,
                    as.character(date)))

    ctx <- new.env(parent = emptyenv())
    ctx$date <- date
    ctx$instance_dir <- tryCatch(instance_dir(instance_dir),
                                 error = function(e) NULL)

    section_blocks <- list(
                           sprintf("=== DATE ===\n%s (%s, mode: %s)",
                                   format(date, "%Y-%m-%d"), weekdays(date), mode)
    )
    for (spec in sources) {
        out <- tryCatch(.run_source(spec, mode, ctx),
                        error = function(e) {
            message(sprintf("cerebro: source %s error: %s",
                            spec$type, conditionMessage(e)))
            NULL
        })
        if (is.null(out)) {
            next
        }
        for (label in names(out)) {
            txt <- out[[label]]
            if (is.null(txt) || !nzchar(txt)) {
                next
            }
            section_blocks <- c(section_blocks,
                                sprintf("=== %s ===\n%s", label, txt))
        }
    }
    prompt <- paste(section_blocks, collapse = "\n\n")
    sys <- paste(c(system_prompt, mode_hints[[mode]]), collapse = "\n\n")

    if (!requireNamespace("llm.api", quietly = TRUE)) {
        stop("llm.api required for briefing", call. = FALSE)
    }
    chat_args <- list(prompt = prompt, system = sys,
                      model = model, provider = provider)
    if (!identical(provider, "openai_codex") && !is.null(max_tokens)) {
        chat_args$max_tokens <- as.integer(max_tokens)
    }
    resp <- do.call(llm.api::chat, chat_args)
    if (is.list(resp) && !is.null(resp$content)) {
        resp$content
    } else {
        as.character(resp)[1]
    }
}
