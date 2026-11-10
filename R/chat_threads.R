# Conversation extraction, topical wiki filing, and Matrix thread rooms.

#' Recent user-authored conversation evidence
#'
#' Reads bounded, recent conversations from Matrix archives and the local
#' Claude Code and Codex session stores. Tool calls, tool results, reasoning,
#' developer messages, generated environment context, and assistant-only
#' Matrix notifications are excluded.
#'
#' @param mode Character briefing mode. Present for the briefing-source API.
#' @param ctx Briefing context environment.
#' @param params Named source configuration.
#' @return A named list containing recent chat evidence, or NULL.
#' @export
briefing_source_chat_recent <- function(mode, ctx, params) {
    sessions <- .chat_collect(ctx, params)
    scope <- match.arg(params$scope %||% "all", c("all", "general", "repo"))
    if (identical(scope, "general")) {
        sessions <- Filter(function(x) !nzchar(.thread_session_repo(x)),
                           sessions)
    } else if (identical(scope, "repo")) {
        sessions <- Filter(function(x) nzchar(.thread_session_repo(x)), sessions)
    }
    if (!length(sessions)) {
        return(NULL)
    }
    text <- .chat_sessions_text(
                                sessions,
                                max_turns = as.integer(params$max_turns_per_session %||% 6L),
                                max_turn_chars = as.integer(params$max_chars_per_turn %||% 1000L),
                                max_chars = as.integer(params$max_chars %||% 8000L),
                                include_assistant = if (is.null(params$include_assistant)) {
            TRUE
        } else {
            isTRUE(params$include_assistant)
        }
    )
    if (!nzchar(text)) {
        return(NULL)
    }
    ctx$chat_recent_text <- text
    list(`RECENT CHAT EVIDENCE` = text)
}

.chat_collect <- function(ctx, params) {
    include <- unique(tolower(as.character(unlist(
                    params$include %||% c("matrix", "claude", "codex"),
                    use.names = FALSE
                ))))
    days <- as.integer(params$days %||% 3L)
    max_sessions <- as.integer(params$max_sessions %||% 9L)
    max_per_source <- as.integer(params$max_per_source %||%
                                 max(1L, ceiling(max_sessions / max(1L, length(include)))))
    window <- .local_date_window(ctx$date %||% Sys.Date(), days)
    cutoff <- window$cutoff
    end <- window$end
    roots <- list(
                  matrix = params$matrix_root %||% tryCatch(
            .source_instance_path(ctx, "vault", "raw", "matrix"),
            error = function(e) NULL
        ),
                  claude = params$claude_root %||% "~/.claude/projects",
                  codex = params$codex_root %||% "~/.codex/sessions"
    )
    parsers <- list(matrix = .chat_parse_matrix, claude = .chat_parse_claude,
                    codex = .chat_parse_codex)
    # Rooms the caller never wants routed, by stable room id. matrix_root
    # cannot express this: it selects directories, and every room in an
    # instance shares one. Applied after parsing, since the room id is
    # what the archive carries rather than anything in its filename.
    exclude_rooms <- unique(as.character(unlist(
                params$exclude_matrix_rooms %||% character(),
                use.names = FALSE
            )))
    sessions <- list()
    # Content identities already admitted, across every source and root,
    # so the caps below count distinct conversations.
    seen_fp <- character()
    n_dupes <- 0L
    n_excluded <- 0L
    for (source in intersect(include, names(parsers))) {
        source_roots <- path.expand(as.character(unlist(
                    roots[[source]] %||% character(), use.names = FALSE
                )))
        found <- list()
        for (root in source_roots) {
            if (!nzchar(root) || !dir.exists(root)) {
                next
            }
            # Every candidate, newest-first. Truncating here hid older
            # unique conversations behind a run of newer duplicates,
            # because the cut happened before identity was considered.
            # The budget check below is what stops the walk.
            files <- .chat_recent_files(root, cutoff, end, Inf)
            for (path in files) {
                parsed <- parsers[[source]](path, cutoff, end)
                if (is.null(parsed) || !length(parsed$turns) ||
                    !any(vapply(parsed$turns, `[[`, character(1L), "role") ==
                         "user")) {
                    next
                }
                if (length(exclude_rooms) &&
                    nzchar(parsed$room_id %||% "") &&
                    parsed$room_id %in% exclude_rooms) {
                    n_excluded <- n_excluded + 1L
                    next
                }
                parsed$source <- source
                parsed$path <- normalizePath(path, mustWork = FALSE)
                # Deduplicate while filling the budget, not after. The
                # same conversation is archived repeatedly, and a copy
                # that consumed a slot here left an older distinct
                # conversation undiscovered -- trading duplicate posts
                # for lost material.
                fp <- .thread_session_fingerprint(parsed)
                if (fp %in% seen_fp) {
                    n_dupes <- n_dupes + 1L
                    next
                }
                seen_fp <- c(seen_fp, fp)
                found[[length(found) + 1L]] <- parsed
                if (length(found) >= max_per_source) {
                    break
                }
            }
            if (length(found) >= max_per_source) {
                break
            }
        }
        sessions <- c(sessions, found)
    }
    # Report before the empty return, not after: a run where everything
    # was a duplicate or an excluded room collects nothing, and that is
    # exactly the case where saying why matters most.
    if (n_dupes > 0L) {
        message(sprintf("cerebro: %d duplicate conversation source(s) ignored",
                        n_dupes))
    }
    if (n_excluded > 0L) {
        message(sprintf("cerebro: %d archive(s) from excluded room(s) ignored",
                        n_excluded))
    }
    if (!length(sessions)) {
        return(list())
    }
    times <- vapply(sessions, function(x) as.numeric(x$time), numeric(1L))
    sessions <- sessions[order(times, decreasing = TRUE)]
    utils::head(sessions, max_sessions)
}

.chat_recent_files <- function(root, cutoff, end, limit) {
    files <- list.files(root, pattern = "\\.(md|jsonl)$", recursive = TRUE,
                        full.names = TRUE)
    if (!length(files)) {
        return(character())
    }
    info <- file.info(files)
    keep <- !is.na(info$mtime) & info$mtime >= cutoff & info$mtime < end
    files <- files[keep]
    info <- info[keep,, drop = FALSE]
    if (!length(files)) {
        return(character())
    }
    files[utils::head(order(info$mtime, decreasing = TRUE), limit)]
}

# Is this archived turn one of our own summaries fed back to us?
#
# Thread rooms are archived like any other room, so a summary posted
# into one becomes a source for the next run, which summarizes it, posts
# that, and so on. Left alone the room fills with the bot talking to
# itself and each archive of it produces another post.
#
# The role check is what makes an in-body marker unnecessary. Only the
# bot produces assistant turns, so a human who happens to type this
# phrase lands as a user turn and is preserved. Invisible markers were
# tried and rejected: the markdown converter escapes HTML comments, and
# zero-width padding does not hide literal text.
# Matched against .THREAD_SUMMARY_LEAD, the same literal the emitters
# build from, so a wording change cannot leave the two out of step.
.chat_is_generated_summary <- function(role, text) {
    identical(role, "assistant") &&
    startsWith(trimws(text %||% ""), .THREAD_SUMMARY_LEAD)
}

.chat_parse_matrix <- function(path, cutoff, end) {
    info <- file.info(path)
    if (is.na(info$mtime) || info$mtime < cutoff || info$mtime >= end) {
        return(NULL)
    }
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    idx <- grep("^## (user|assistant)\\s*$", lines)
    if (!length(idx)) {
        return(NULL)
    }
    roles <- sub("^## (user|assistant)\\s*$", "\\1", lines[idx])
    turns <- lapply(seq_along(idx), function(i) {
        start <- idx[[i]] + 1L
        stop <- if (i < length(idx)) idx[[i + 1L]] - 1L else length(lines)
        selected <- if (start <= stop) lines[start:stop] else character()
        list(role = roles[[i]],
             text = trimws(paste(selected, collapse = "\n")),
             time = info$mtime)
    })
    turns <- Filter(function(x) nzchar(x$text), turns)
    turns <- Filter(function(x) !.chat_is_generated_summary(x$role, x$text),
                    turns)

    fm <- .chat_matrix_frontmatter(lines)
    source <- .chat_scalar(fm$source %||% "")
    title <- .chat_scalar(fm$title %||% "")
    if (startsWith(source, "!")) {
        room_id <- source
    } else {
        room_id <- ""
    }
    label <- .chat_matrix_metadata(lines, "Room name at archive time")
    cwd <- .chat_matrix_metadata(lines, "Working directory at archive time")
    if (!nzchar(cwd)) {
        cwd <- .chat_matrix_metadata(lines, "Working directory")
    }
    if (!nzchar(label)) {
        if (nzchar(title) && !startsWith(title, "!")) {
            label <- title
        } else {
            label <- source
        }
    }
    if (!nzchar(label)) {
        # no frontmatter or room metadata at all: keep the old filename fallback
        label <- tools::file_path_sans_ext(basename(path))
    }

    parsed <- .chat_session(label, turns, path)
    if (is.null(parsed)) {
        return(NULL)
    }
    if (nzchar(cwd) && dir.exists(path.expand(cwd))) {
        parsed$cwd <- normalizePath(path.expand(cwd), mustWork = FALSE)
    }
    if (nzchar(room_id)) {
        parsed$room_id <- room_id
    }
    parsed
}

.chat_matrix_frontmatter <- function(lines) {
    if (length(lines) < 3L || !identical(trimws(lines[[1L]]), "---")) {
        return(list())
    }
    end <- which(trimws(lines[-1L]) == "---")
    if (!length(end)) {
        return(list())
    }
    end <- end[[1L]]
    yaml_text <- paste(lines[2L:end], collapse = "\n")
    tryCatch(yaml::yaml.load(yaml_text), error = function(e) list()) %||% list()
}

.chat_scalar <- function(x) {
    if (is.null(x) || !length(x)) {
        return("")
    }
    x <- as.character(x[[1L]])
    if (is.na(x)) {
        return("")
    }
    trimws(gsub("^[\"']|[\"']$", "", x))
}

.chat_matrix_metadata <- function(lines, field) {
    pattern <- paste0("^", field, ":\\s*")
    hit <- grep(pattern, lines, value = TRUE)
    if (!length(hit)) {
        return("")
    }
    .chat_scalar(sub(pattern, "", hit[[1L]]))
}

.chat_parse_claude <- function(path, cutoff, end) {
    rows <- .chat_read_jsonl(path)
    turns <- list()
    label <- NULL
    for (row in rows) {
        if (isTRUE(row$isSidechain) || !row$type %in% c("user", "assistant")) {
            next
        }
        time <- .chat_parse_time(row$timestamp)
        if (is.na(time) || time < cutoff || time >= end) {
            next
        }
        content <- row$message$content %||% NULL
        if (identical(row$type, "user")) {
            if (!is.character(content) || length(content) != 1L) {
                next
            }
            text <- .chat_clean_user_text(content)
        } else if (is.character(content)) {
            text <- paste(content, collapse = "\n")
        } else if (is.list(content)) {
            text <- vapply(Filter(function(x) identical(x$type, "text"),
                                  content),
                           function(x) x$text %||% "", character(1L))
            text <- paste(text[nzchar(text)], collapse = "\n")
        } else {
            next
        }
        if (!nzchar(trimws(text))) {
            next
        }
        label <- label %||% row$cwd
        turns[[length(turns) + 1L]] <- list(
            role = row$type, text = text, time = time
        )
    }
    .chat_session(label, turns, path)
}

.chat_parse_codex <- function(path, cutoff, end) {
    rows <- .chat_read_jsonl(path)
    turns <- list()
    label <- NULL
    for (row in rows) {
        if (identical(row$type, "session_meta")) {
            label <- row$payload$cwd %||% label
            next
        }
        time <- .chat_parse_time(row$timestamp)
        if (is.na(time) || time < cutoff || time >= end) {
            next
        }
        payload <- row$payload %||% list()
        if (identical(row$type, "event_msg") &&
            identical(payload$type, "user_message")) {
            text <- .chat_clean_user_text(payload$message %||% "")
            role <- "user"
        } else if (identical(row$type, "response_item") &&
            identical(payload$type, "message") &&
            identical(payload$role, "assistant")) {
            content <- payload$content %||% list()
            text <- vapply(Filter(function(x) {
                identical(x$type, "output_text")
            }, content), function(x) x$text %||% "", character(1L))
            text <- paste(text[nzchar(text)], collapse = "\n")
            role <- "assistant"
        } else {
            next
        }
        if (!nzchar(trimws(text))) {
            next
        }
        turns[[length(turns) + 1L]] <- list(role = role, text = text,
            time = time)
    }
    .chat_session(label, turns, path)
}

.chat_read_jsonl <- function(path) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    out <- lapply(lines[nzchar(lines)], function(line) {
        tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE),
                 error = function(e) NULL)
    })
    Filter(Negate(is.null), out)
}

# An instant, carrying no zone name. The session stores write UTC
# strings, and the lookback window they are compared against is built
# in the local zone (.local_date_window), because that is what a day
# means to the user. R warns every time two POSIXct values naming
# different zones meet, and a day of sessions compares tens of
# thousands of times. Only the instant matters here, so the parsed
# value names no zone, and the window keeps the one that defines its
# day boundaries.
.chat_parse_time <- function(x) {
    if (is.null(x) || !length(x) || !nzchar(x[[1L]])) {
        return(as.POSIXct(NA))
    }
    at <- as.POSIXct(strptime(x[[1L]], "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))
    attr(at, "tzone") <- NULL
    at
}

.chat_session <- function(label, turns, path) {
    turns <- .chat_dedupe_turns(turns)
    if (!length(turns)) {
        return(NULL)
    }
    times <- vapply(turns, function(x) as.numeric(x$time), numeric(1L))
    cwd <- if (!is.null(label) && dir.exists(path.expand(label))) {
        normalizePath(path.expand(label), mustWork = FALSE)
    } else {
        NULL
    }
    list(
         label = .chat_display_path(label %||%
                                    tools::file_path_sans_ext(basename(path))),
         cwd = cwd,
         time = as.POSIXct(max(times), origin = "1970-01-01", tz = "UTC"),
         turns = turns
    )
}

.chat_dedupe_turns <- function(turns) {
    if (length(turns) < 2L) {
        return(turns)
    }
    keep <- rep(TRUE, length(turns))
    for (i in 2:length(turns)) {
        keep[[i]] <- !identical(turns[[i]]$role, turns[[i - 1L]]$role) ||
        !identical(trimws(turns[[i]]$text), trimws(turns[[i - 1L]]$text))
    }
    turns[keep]
}

.chat_select_turns <- function(turns, max_turns, include_assistant) {
    users <- which(vapply(turns, `[[`, character(1L), "role") == "user")
    selected <- integer()
    for (i in users) {
        selected <- c(selected, i)
        if (isTRUE(include_assistant)) {
            next_user <- users[users > i]
            if (length(next_user)) {
                stop <- next_user[[1L]] - 1L
            } else {
                stop <- length(turns)
            }
            assistants <- which(vapply(turns, `[[`, character(1L), "role") ==
                                "assistant")
            assistants <- assistants[assistants > i & assistants <= stop]
            if (length(assistants)) {
                selected <- c(selected, utils::tail(assistants, 1L))
            }
        }
    }
    selected <- sort(unique(selected))
    turns[utils::tail(selected, max_turns)]
}

.chat_sessions_text <- function(sessions, max_turns, max_turn_chars,
                                max_chars, include_assistant) {
    blocks <- vapply(sessions, function(session) {
        turns <- .chat_select_turns(session$turns, max_turns, include_assistant)
        rendered <- vapply(turns, function(turn) {
            sprintf("%s: %s", toupper(turn$role),
                    .chat_bound_text(turn$text, max_turn_chars))
        }, character(1L))
        sprintf("-- %s | %s | %s | %s --\n%s",
                session$source,
                session$label,
                format(session$time, "%Y-%m-%d %H:%M:%S %Z"),
                .chat_display_path(session$path),
                paste(rendered, collapse = "\n\n"))
    }, character(1L))
    .chat_bound_text(paste(blocks, collapse = "\n\n"), max_chars)
}

.chat_clean_user_text <- function(text) {
    text <- trimws(text %||% "")
    if (grepl("^<environment_context>[\\s\\S]*</environment_context>$", text,
              perl = TRUE) || startsWith(text, "<turn_aborted>")) {
        return("")
    }
    text <- gsub("<system-reminder>[\\s\\S]*?</system-reminder>", "",
                 text, perl = TRUE)
    trimws(text)
}

.chat_bound_text <- function(text, max_chars) {
    text <- trimws(text %||% "")
    if (nchar(text) <= max_chars) {
        return(text)
    }
    head_chars <- max(1L, floor(max_chars * 0.7))
    tail_chars <- max(1L, max_chars - head_chars - 7L)
    paste0(substr(text, 1L, head_chars), "\n[...]\n",
           substr(text, nchar(text) - tail_chars + 1L, nchar(text)))
}

.chat_display_path <- function(path) {
    if (is.null(path) || !length(path)) {
        return("unknown")
    }
    sub(paste0("^", path.expand("~")), "~", path[[1L]])
}
