#' Reconcile today's todo list against what actually shipped
#'
#' A weekday-morning capability: read today's section of the daily todo,
#' gather the same git evidence the briefing uses, and ask the LLM for a
#' best guess of which open items actually shipped. The model returns only
#' structured ops (line numbers to mark done, items it is unsure about, and
#' off-list shipped work); R applies them deterministically with surgical
#' bracket edits. The model cannot reword, reorder, delete, or touch
#' \code{[!]} blocked items.
#'
#' Whatever it cannot judge confidently is left untouched and reported under
#' "unclear" in the Matrix summary. Idempotent per day on \code{name}.
#'
#' @param name Character. Idempotency key + log label.
#' @param root Character. Directory holding the live daily todo file.
#' @param room Character or NULL. Matrix room for the summary.
#' @param sources List of briefing source specs used to build git evidence.
#' @param system_prompt Character. Reconcile system prompt.
#' @param model Character. LLM model id. Required; there is no default.
#' @param provider Character. LLM provider id.
#' @param max_tokens Integer or NULL.
#' @param force Logical. Override once-per-day guard.
#' @param dry_run Logical. Print the plan instead of writing/sending.
#' @param instance_dir Character or NULL.
#' @param date Date. For testing; defaults to today.
#' @return The summary text, invisibly. NULL when skipped.
#' @export
reconcile_todo <- function(name = "reconcile", root, room = NULL,
                           sources = list(), system_prompt, model,
                           provider = "anthropic", max_tokens = NULL,
                           force = FALSE, dry_run = FALSE,
                           instance_dir = NULL, date = Sys.Date()) {
    .require_model(model)
    runner <- function() {
        day_name <- weekdays(date)
        path <- .reconcile_daily_file(root, date)
        if (is.null(path)) {
            msg <- "reconcile: no current daily todo file found; nothing to do."
            message("cerebro: ", msg)
            return(invisible(NULL))
        }
        lines <- readLines(path, warn = FALSE)
        rng <- .reconcile_section_range(lines, day_name)
        if (is.null(rng)) {
            msg <- sprintf(
                           "reconcile: no '## %s' section in %s; nothing to do.",
                           day_name, basename(path))
            message("cerebro: ", msg)
            return(invisible(NULL))
        }

        cand <- .reconcile_candidates(lines, rng)
        if (!nrow(cand)) {
            msg <- sprintf("reconcile: no open items in today's (%s) list.",
                           day_name)
            message("cerebro: ", msg)
            if (!isTRUE(dry_run) && !is.null(room)) {
                matrix_send(msg, room = room, instance_dir = instance_dir)
            }
            return(invisible(msg))
        }

        evidence <- .reconcile_evidence(sources, date, instance_dir)
        ops <- .reconcile_ask(cand, evidence, day_name,
                              system_prompt = system_prompt,
                              model = model, provider = provider,
                              max_tokens = max_tokens)

        if (is.null(ops)) {
            msg <- paste0(
                          "reconcile: could not read a clear answer from the model; ",
                          "left today's list untouched.")
            message("cerebro: ", msg)
            if (!isTRUE(dry_run) && !is.null(room)) {
                matrix_send(msg, room = room, instance_dir = instance_dir)
            }
            return(invisible(msg))
        }

        applied <- .reconcile_apply(lines, rng, cand, ops)
        summary <- .reconcile_summary(applied, cand, ops, day_name)

        if (isTRUE(dry_run)) {
            cat("=== reconcile ", name, " (dry-run) ===\n", summary, "\n",
                sep = "")
            cat("--- would write ", length(applied$done_lines),
                " done, ", length(ops$offlist), " off-list ---\n", sep = "")
            return(invisible(summary))
        }

        if (applied$changed) {
            writeLines(applied$lines, path)
            message(sprintf("cerebro: reconcile wrote %d edits to %s",
                            length(applied$done_lines) + length(ops$offlist),
                            basename(path)))
        }
        if (!is.null(room)) {
            matrix_send(summary, room = room, instance_dir = instance_dir,
                        markdown = TRUE)
        }
        invisible(summary)
    }

    if (isTRUE(dry_run)) {
        invisible(runner())
    } else {
        once_today(paste0("reconcile_", name), runner,
                   force = force, instance_dir = instance_dir)
    }
}

# Capability builder. Room, model, provider, sources and max_tokens come
# from the `briefings:` entry the routine names (default "morning"); the
# routine's own `system_prompt` and `root` set the prompt and the todo
# directory.
.cap_reconcile <- function(r, briefings, cfg, instance_dir) {
    bcfg <- briefings[[r$briefing %||% "morning"]] %||% list()
    sys_prompt <- .resolve_prompt(r$system_prompt, instance_dir)
    root <- r$root %||% file.path(
                                  tryCatch(instance_dir(instance_dir), error = function(e) "."),
                                  "todo", "this_week")
    room <- r$room %||% bcfg$room
    model <- .require_model(bcfg$model,
                            sprintf("briefing '%s' (used by routine %s)",
                                    r$briefing %||% "morning",
                                    r$name %||% "reconcile"))
    provider <- bcfg$provider %||% "anthropic"
    sources <- bcfg$sources %||% list()
    max_tokens <- NULL
    if (!is.null(bcfg$max_tokens)) {
        max_tokens <- as.integer(bcfg$max_tokens)
    }
    nm <- r$name %||% "reconcile"
    idir <- instance_dir
    function() {
        reconcile_todo(name = nm, root = root, room = room,
                       sources = sources, system_prompt = sys_prompt,
                       model = model, provider = provider,
                       max_tokens = max_tokens, instance_dir = idir)
    }
}

# Resolve the current week's daily file (lowercase hacer convention:
# todo_<yymmdd>_daily.md, keyed on the week's Monday). Pick the latest
# whose date is on or before `date`.
.reconcile_daily_file <- function(root, date) {
    if (is.null(root) || !dir.exists(root)) {
        return(NULL)
    }
    files <- list.files(root, pattern = "^todo_[0-9]{6}_daily\\.(md|txt)$",
                        full.names = TRUE, ignore.case = TRUE)
    if (!length(files)) {
        return(NULL)
    }
    starts <- as.Date(sub("^.*todo_([0-9]{6}).*$", "\\1", basename(files)),
                      format = "%y%m%d")
    ok <- !is.na(starts) & starts <= date
    if (!any(ok)) {
        return(NULL)
    }
    files[ok][which.max(starts[ok])]
}

# Line range [start, end] of the body under the `## <day_name>` heading,
# exclusive of the heading line itself. NULL if the section is absent.
.reconcile_section_range <- function(lines, day_name) {
    hdr <- grepl(sprintf("^#+\\s+%s\\s*$", day_name), lines)
    if (!any(hdr)) {
        return(NULL)
    }
    start <- which(hdr)[1L]
    rest <- which(grepl("^#+\\s+\\w", lines))
    after <- rest[rest > start]
    if (length(after)) {
        end <- after[1L] - 1L
    } else {
        end <- length(lines)
    }
    if (end < start + 1L) {
        return(c(start + 1L, start)) # empty section
    }
    c(start + 1L, end)
}

# Open ([ ] / [/]) task lines within the section, as a data.frame with the
# absolute line number, status symbol, and stripped text. Done and blocked
# items are excluded so they are never re-touched.
.reconcile_candidates <- function(lines, rng) {
    out <- data.frame(line = integer(), status = character(),
                      text = character(), stringsAsFactors = FALSE)
    if (rng[2L] < rng[1L]) {
        return(out)
    }
    pat <- "^(\\s*)[-*]\\s+\\[( |/)\\]\\s+(.*)$"
    for (i in seq(rng[1L], rng[2L])) {
        ln <- lines[i]
        if (!grepl(pat, ln)) {
            next
        }
        status <- sub(pat, "\\2", ln)
        text <- sub(pat, "\\3", ln)
        out <- rbind(out, data.frame(line = i, status = status, text = text,
                                     stringsAsFactors = FALSE))
    }
    out
}

# Run the configured briefing sources to assemble a git-evidence block,
# mirroring the briefing's section assembly (RECENT COMMITS, OFF-LIST
# ACTIVITY, ...). Errors in any one source are swallowed.
.reconcile_evidence <- function(sources, date, instance_dir) {
    if (!length(sources)) {
        return("")
    }
    mode <- tryCatch(briefing_mode_default(date), error = function(e) "daily")
    ctx <- new.env(parent = emptyenv())
    ctx$date <- date
    ctx$instance_dir <- tryCatch(instance_dir(instance_dir),
                                 error = function(e) NULL)
    blocks <- character()
    for (spec in sources) {
        out <- tryCatch(.run_source(spec, mode, ctx), error = function(e) {
            message(sprintf("cerebro: reconcile source %s error: %s",
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
            blocks <- c(blocks, sprintf("=== %s ===\n%s", label, txt))
        }
    }
    paste(blocks, collapse = "\n\n")
}

# Ask the model for structured ops. Returns list(done=<int>, unclear=
# list(line, why), offlist=<chr>) or NULL if the response can't be read.
.reconcile_ask <- function(cand, evidence, day_name, system_prompt, model,
                           provider, max_tokens) {
    if (!requireNamespace("llm.api", quietly = TRUE)) {
        stop("llm.api required for reconcile", call. = FALSE)
    }
    items <- paste(sprintf("L%d: [%s] %s", cand$line, cand$status, cand$text),
                   collapse = "\n")
    prompt <- paste(
                    sprintf("Today is %s. Here are today's open todo items, each tagged",
                            day_name),
                    "with its line number (L<n>):",
                    "",
                    items,
                    "",
                    "Evidence of what actually shipped (git history, off-list commits):",
                    "",
        if (nzchar(evidence)) evidence else "(no git evidence available)",
                    "",
                    paste("Return ONLY a JSON object with keys: \"done\" (array of line",
                          "numbers you are confident shipped), \"unclear\" (array of",
                          "{\"line\": n, \"why\": \"...\"} for items you cannot judge),",
                          "and \"offlist\" (array of short strings for shipped work that",
                          "has no matching item above). No prose outside the JSON."),
                    sep = "\n")

    chat_args <- list(prompt = prompt, system = system_prompt,
                      model = model, provider = provider)
    if (!identical(provider, "openai_codex") && !is.null(max_tokens)) {
        chat_args$max_tokens <- as.integer(max_tokens)
    }
    resp <- tryCatch(do.call(llm.api::chat, chat_args),
                     error = function(e) {
        message("cerebro: reconcile chat error: ", conditionMessage(e))
        NULL
    })
    if (is.null(resp)) {
        return(NULL)
    }
    if (is.list(resp) && !is.null(resp$content)) {
        reply <- resp$content
    } else {
        reply <- as.character(resp)[1]
    }

    .reconcile_parse_ops(reply, valid_lines = cand$line)
}

# Pull the first {...} block out of the reply and coerce it into the ops
# list, dropping anything that doesn't reference a real candidate line.
.reconcile_parse_ops <- function(reply, valid_lines) {
    if (is.null(reply) || !nzchar(reply)) {
        return(NULL)
    }
    m <- regmatches(reply, regexpr("\\{(?:[^{}]|\\{[^{}]*\\})*\\}", reply,
                                   perl = TRUE))
    if (!length(m)) {
        return(NULL)
    }
    obj <- tryCatch(jsonlite::fromJSON(m[1L], simplifyVector = FALSE),
                    error = function(e) NULL)
    if (is.null(obj)) {
        return(NULL)
    }

    as_lines <- function(x) {
        v <- suppressWarnings(as.integer(unlist(x, use.names = FALSE)))
        v <- v[!is.na(v)]
        intersect(unique(v), valid_lines)
    }
    done <- as_lines(obj$done)

    unclear <- list()
    for (u in obj$unclear %||% list()) {
        ln <- suppressWarnings(as.integer(u$line))
        if (is.na(ln) || !(ln %in% valid_lines) || ln %in% done) {
            next
        }
        unclear[[length(unclear) + 1L]] <- list(
            line = ln, why = as.character(u$why %||% ""))
    }

    offlist <- as.character(unlist(obj$offlist %||% character(),
                                   use.names = FALSE))
    offlist <- trimws(offlist)
    offlist <- offlist[nzchar(offlist)]
    if (length(offlist) > 10L) {
        offlist <- offlist[seq_len(10L)]
    }

    list(done = done, unclear = unclear, offlist = offlist)
}

# Apply ops to `lines`: flip [ ]/[/] -> [x] on done lines, append off-list
# shipped items at the end of the section. Returns the new lines plus what
# changed. Bracket flips are guarded to open candidate lines only.
.reconcile_apply <- function(lines, rng, cand, ops) {
    done_lines <- intersect(ops$done, cand$line)
    for (i in done_lines) {
        lines[i] <- sub("^(\\s*[-*]\\s+)\\[( |/)\\]", "\\1[x]", lines[i])
    }
    changed <- length(done_lines) > 0L

    if (length(ops$offlist)) {
        add <- sprintf("- [x] %s  <!-- off-list, reconciled -->", ops$offlist)
        at <- rng[2L]
        lines <- append(lines, add, after = at)
        changed <- TRUE
    }
    list(lines = lines, changed = changed, done_lines = done_lines)
}

# Human-readable Matrix summary. Speaks plainly; lists unclear items so the
# user can course-correct.
.reconcile_summary <- function(applied, cand, ops, day_name) {
    txt_for <- function(ln) {
        hit <- cand$text[cand$line == ln]
        if (length(hit)) {
            hit[1L]
        } else {
            paste0("line ", ln)
        }
    }
    parts <- sprintf("**Reconciled today's list (%s):**", day_name)

    if (length(applied$done_lines)) {
        done_txt <- vapply(applied$done_lines, txt_for, character(1))
        parts <- c(parts, "", "Marked done:", paste0("- ", done_txt))
    } else {
        parts <- c(parts, "", "Marked done: none.")
    }

    if (length(ops$offlist)) {
        parts <- c(parts, "", "Off-list shipped (added):",
                   paste0("- ", ops$offlist))
    }

    if (length(ops$unclear)) {
        un <- vapply(ops$unclear, function(u) {
            t <- txt_for(u$line)
            if (nzchar(u$why)) sprintf("- %s (%s)", t, u$why)
            else sprintf("- %s", t)
        }, character(1))
        parts <- c(parts, "", "Unclear, left as-is for you:", un)
    }

    paste(parts, collapse = "\n")
}
