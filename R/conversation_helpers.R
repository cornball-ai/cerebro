# The repository a working directory belongs to, following a linked
# worktree back to it.
#
# `rev-parse --show-toplevel` inside a worktree returns the WORKTREE, and
# filing by that path gives a permanent room to a directory that exists
# to be deleted -- `~/glinty/.claude/worktrees/gallery-loop` was one.
# The common git dir points at the main repository, so a worktree session
# files with the repo it belongs to. Following it rather than skipping it
# means the conversation still gets threaded, which dropping it would
# have cost.
#
# In the main worktree the common dir IS that repo's .git, so this
# returns the toplevel unchanged and the ordinary case is untouched.
.thread_git_root <- function(cwd) {
    run <- function(args) {
        out <- tryCatch(
                        suppressWarnings(system2("git", c("-C", shQuote(cwd), args),
                    stdout = TRUE, stderr = FALSE)),
                        error = function(e) character())
        if (length(out) && nzchar(out[[1L]])) {
            out[[1L]]
        } else {
            ""
        }
    }
    top <- run(c("rev-parse", "--show-toplevel"))
    if (!nzchar(top) || !dir.exists(top)) {
        return("")
    }
    common <- run(c("rev-parse", "--path-format=absolute", "--git-common-dir"))
    if (nzchar(common) && basename(common) == ".git") {
        main <- dirname(common)
        if (dir.exists(main)) {
            return(normalizePath(main, mustWork = FALSE))
        }
    }
    normalizePath(top, mustWork = FALSE)
}

.thread_session_repo <- function(session) {
    cwd <- session$cwd %||% ""
    if (nzchar(cwd) && dir.exists(cwd)) {
        root <- .thread_git_root(cwd)
        if (nzchar(root)) {
            return(root)
        }
    }
    if (identical(session$source, "matrix") &&
        grepl(" \u00b7 ", session$label, fixed = TRUE)) {
        parts <- strsplit(session$label, " \u00b7 ", fixed = TRUE)[[1L]]
        repo <- utils::tail(parts, 1L)
        candidates <- file.path("~", repo)
        projects <- tryCatch(saber::projects(), error = function(e) NULL)
        if (is.data.frame(projects) && nrow(projects)) {
            matched <- projects$path[
                basename(projects$path) == repo | projects$package == repo
            ]
            candidates <- c(candidates, matched)
        }
        candidates <- path.expand(candidates)
        hit <- candidates[dir.exists(file.path(candidates, ".git"))]
        if (length(hit)) {
            return(normalizePath(hit[[1L]], mustWork = FALSE))
        }
    }
    ""
}

.thread_prepare_records <- function(sessions, vault, source_state,
                                    force = FALSE, dry_run = FALSE) {
    records <- list()
    for (session in sessions) {
        turns <- .chat_select_turns(session$turns, 12L, TRUE)
        body <- .thread_session_markdown(session, turns)
        # Change detection compares the conversation, not the rendered
        # body. The body carries "Session path:", so hashing it made the
        # same conversation look changed the moment it was archived to a
        # new path -- which is every restart, and defeats keying on
        # content identity.
        hash <- .thread_session_fingerprint(session)
        # Key on what the conversation IS, not where this copy of it
        # happens to live. A path key meant that once the archive we had
        # summarized aged out of the lookback, an identical copy at
        # another path read as new and was summarized again -- so
        # in-memory deduplication alone could not survive a run.
        key <- .thread_session_fingerprint(session)
        previous <- source_state[[key]] %||% list()
        if (!length(previous)) {
            # Migrate pre-0.1.1.15 state. It was keyed on (source, path)
            # and stored a SHA-1 of the rendered body, so simply reading
            # the old entry proves nothing: its hash can never equal the
            # new fingerprint. Compare it the way it was written, and
            # only then carry the entry forward under the new identity.
            # Without this, every previously summarized conversation
            # reads as new on upgrade and is summarized and posted again.
            legacy_key <- digest::digest(paste(session$source, session$path,
                    sep = "::"), algo = "sha1")
            legacy <- source_state[[legacy_key]] %||% list()
            if (length(legacy) &&
                identical(legacy$summary_hash,
                          digest::digest(body, algo = "sha1"))) {
                legacy$summary_hash <- hash
                legacy$raw_hash <- hash
                source_state[[key]] <- legacy
                # Carry it forward as this session's previous state and
                # fall through. Returning here skipped the force check
                # below, so an explicitly forced run silently produced
                # nothing for anything still on legacy state.
                previous <- legacy
            }
        }
        if (!isTRUE(force) && identical(previous$summary_hash, hash)) {
            next
        }

        raw_path <- previous$raw_path %||% ""
        raw_exists <- nzchar(raw_path) &&
        file.exists(file.path(vault, raw_path))
        if (!identical(previous$raw_hash, hash) || !raw_exists) {
            if (isTRUE(dry_run)) {
                raw_path <- paste0("raw/chats/pending-", key, ".md")
            } else {
                written <- pensar::ingest(
                    content = body,
                    type = "chats",
                    source = paste0(session$source, ":", session$path),
                    title = paste(.thread_source_label(session$source),
                                  session$label),
                    tags = c("conversation", session$source),
                    vault = vault
                )
                raw_path <- .thread_relative(written, vault)
                previous$raw_hash <- hash
                previous$raw_path <- raw_path
                source_state[[key]] <- previous
            }
        }
        records[[length(records) + 1L]] <- list(
            key = key, hash = hash, raw_path = raw_path,
            link = sub("\\.md$", "", gsub("\\\\", "/", raw_path)),
            session = session, body = body
        )
    }
    list(records = records, state = source_state)
}

.thread_mark_summarized <- function(source_state, records) {
    for (record in records) {
        previous <- source_state[[record$key]] %||% list()
        previous$summary_hash <- record$hash
        previous$raw_hash <- record$hash
        previous$raw_path <- record$raw_path
        source_state[[record$key]] <- previous
    }
    source_state
}

.thread_session_markdown <- function(session, turns) {
    rendered <- vapply(turns, function(turn) {
        paste0("## ", turn$role, "\n\n", .chat_bound_text(turn$text, 2400L))
    }, character(1L))
    paste(c(
            paste0("# ", session$label),
            paste0("Source: ", session$source),
            paste0("Session path: ", .chat_display_path(session$path)),
            if (nzchar(session$room_id %||% "")) {
                paste0("Matrix room id: ", session$room_id)
            } else character(),
            if (nzchar(session$cwd %||% "")) {
                paste0("Working directory: ", .chat_display_path(session$cwd))
            } else character(),
            rendered
        ), collapse = "\n\n")
}

.thread_records_prompt <- function(records) {
    paste(vapply(records, function(record) {
        paste0("=== [[", record$link, "]] ===\n", record$body)
    }, character(1L)), collapse = "\n\n")
}

.thread_require_citations <- function(text, records) {
    missing <- vapply(records, function(record) {
        !grepl(paste0("[[", record$link, "]]"), text, fixed = TRUE)
    }, logical(1L))
    if (any(missing)) {
        stop("conversation synthesis omitted source citation(s): ",
             paste(vapply(records[missing], `[[`, character(1L), "link"),
                   collapse = ", "), call. = FALSE)
    }
    invisible(TRUE)
}

.thread_generate <- function(prompt, system, model, provider, max_tokens,
                             generate_fn = NULL) {
    if (!is.null(generate_fn)) {
        out <- generate_fn(prompt = prompt, system = system, model = model,
                           provider = provider, max_tokens = max_tokens)
    } else {
        args <- list(prompt = prompt, system = system,
                     model = model, provider = provider)
        if (!identical(provider, "openai_codex") && !is.null(max_tokens)) {
            args$max_tokens <- as.integer(max_tokens)
        }
        out <- do.call(llm.api::chat, args)
    }
    if (is.list(out) && !is.null(out$content)) {
        text <- out$content
    } else {
        text <- as.character(out)[[1L]]
    }

    text <- trimws(text %||% "")
    text <- sub("^```(?:markdown|md)?\\s*", "", text,
                ignore.case = TRUE, perl = TRUE)
    text <- sub("\\s*```$", "", text, perl = TRUE)
    if (!nzchar(text)) stop("conversation synthesis returned empty text",
                            call. = FALSE)
    text
}

.thread_repo_page <- function(vault, repo) {
    reg <- pensar::vault_registry(vault, refresh = TRUE)
    wiki <- reg[startsWith(reg$path, "wiki/") & !reg$system_file,, drop = FALSE]
    exact <- which(tolower(wiki$node_id) == tolower(repo) |
                   tolower(wiki$title) == tolower(repo))
    if (length(exact)) {
        return(file.path(vault, wiki$path[[exact[[1L]]]]))
    }
    file.path(vault, "wiki", paste0(.thread_slug(repo), ".md"))
}

.thread_file_body <- function(path, max_chars = NULL) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    if (length(lines) && identical(trimws(lines[[1L]]), "---")) {
        end <- which(trimws(lines[-1L]) == "---")
        if (length(end)) {
            lines <- lines[-seq_len(end[[1L]] + 1L)]
        }
    }
    text <- trimws(paste(lines, collapse = "\n"))
    if (!is.null(max_chars)) {
        .chat_bound_text(text, max_chars)
    } else {
        text
    }
}

.thread_append_page <- function(path, title, tags, body, marker, vault, date) {
    start <- paste0("<!-- cerebro:conversation:", marker, ":start -->")
    end <- paste0("<!-- cerebro:conversation:", marker, ":end -->")
    block <- c(start,
               paste0("## Conversation update \u2014 ", as.character(as.Date(date))),
               "", body, end)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(path)) {
        lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
        if (start %in% lines) {
            return(invisible(path))
        }
        lines <- c(lines, "", block)
    } else {
        fm <- yaml::as.yaml(list(title = title, type = "summary",
                                 source = "conversation threads",
                                 date = as.character(as.Date(date)),
                                 tags = unique(tags)))
        lines <- c("---", sub("\n$", "", fm), "---", "", block)
    }
    writeLines(lines, path, useBytes = TRUE)
    pensar::update_index(vault)
    pensar::log_entry(paste("Updated conversation thread:", basename(path)),
                      operation = "conversation_thread", vault = vault)
    invisible(path)
}

# The router's candidates: every wiki page, and every joined room with a
# name. A NULL client means no transport is reachable, which leaves the
# pages as the only candidates rather than failing the routing.
.thread_topic_candidates <- function(vault, client = NULL) {
    reg <- pensar::vault_registry(vault, refresh = TRUE)
    wiki <- reg[startsWith(reg$path, "wiki/") & !reg$system_file,, drop = FALSE]
    pages <- lapply(seq_len(nrow(wiki)), function(i) {
        path <- file.path(vault, wiki$path[[i]])
        list(path = wiki$path[[i]], title = wiki$title[[i]],
             tags = wiki$tags[[i]], excerpt = .thread_file_body(path, 900L))
    })
    rooms <- list()
    if (!is.null(client)) {
        ids <- tryCatch(chat.api::chat_channels(client),
                        error = function(e) character())
        rooms <- lapply(ids, function(id) {
            room_id <- as.character(id[[1L]])
            info <- .channel_info(client, room_id)
            list(id = room_id, room_id = room_id,
                 name = .matrix_room_name_key(info$name),
                 topic = info$topic)
        })
        rooms <- Filter(function(x) nzchar(x$name %||% ""), rooms)
    }
    list(pages = pages, rooms = rooms)
}

# The topic the router named, or "" when it did not name one.
#
# The router had no structural way to say "there is no durable topic
# here", so it said it in prose, in the topic field: "No durable topical
# thread" came back with confidence above the create threshold and became
# a room. A confidence gate measures how sure the model is, not what it
# was sure of, so it cannot catch this. An empty topic -- which also
# happened, and produced a room named for the prefix and nothing else --
# is the same answer with the words left out.
#
# The system prompt now asks for an empty topic in that case, which is
# the fix. This is the guard behind it, because a prompt is a request.
.thread_durable_topic <- function(topic) {
    t <- .matrix_room_name_key(topic)
    if (!nzchar(t)) {
        return("")
    }
    # A topic is a label. Sentence length means the model is explaining
    # itself in the field meant for the answer.
    if (nchar(t) > 60L) {
        return("")
    }
    # "durable" is the system prompt's own word, not something a theme is
    # ever called. Its presence is the model quoting the question back.
    if (grepl("durable", t, ignore.case = TRUE)) {
        return("")
    }
    if (grepl("\\bno (clear|specific|single|obvious) ", t, ignore.case = TRUE)) {
        return("")
    }
    if (t %in% c("none", "None", "NONE", "n/a", "N/A", "na", "NA",
                 "unknown", "Unknown", "unclear", "Unclear")) {
        return("")
    }
    t
}

# How much of a conversation the human actually wrote. turns counts
# user turns and chars the characters across them; assistant text is
# excluded, because the persona can be arbitrarily verbose about a
# two-word ping.
.thread_session_substance <- function(session) {
    turns <- Filter(function(t) {
        identical(as.character(t$role %||% ""), "user")
    }, session$turns %||% list())
    chars <- vapply(turns, function(t) {
        sum(nchar(as.character(t$text %||% ""), type = "chars"))
    }, integer(1L))
    list(turns = length(turns), chars = as.integer(sum(chars)))
}

.thread_route_general <- function(records, candidates, model, provider,
                                  max_tokens, generate_fn) {
    candidate_json <- jsonlite::toJSON(candidates, auto_unbox = TRUE,
                                       null = "null", pretty = TRUE)
    evidence <- .thread_records_prompt(records)
    system <- paste(
                    "Route one personal/general conversation into a durable topical thread.",
                    "Prefer an existing wiki page and Matrix room when semantically relevant.",
                    "Examples of durable topics include Home Improvement, Health, Travel,",
                    "Family, Finance, and specific long-running interests. Do not route",
                    "repository/software work here; that belongs in a repository thread. Create a new",
                    "theme only when the conversation has enough durable substance to recur.",
                    "When it does not, return an EMPTY topic. Do not describe the absence of",
                    "a theme in the topic field: an empty string is how you say there is no",
                    "durable topic, and any words there are taken as a theme to file under",
                    "and to name a room after.",
                    "Return JSON only with keys: topic, slug, existing_path, room_id,",
                    "room_name, confidence, summary. existing_path and room_id must exactly",
                    "match a candidate or be empty strings. Use room_id, not room_name, to",
                    "select an existing Matrix room; room_name is display metadata for a",
                    "new room only. confidence is 0..1. summary is Markdown and must cite",
                    "every supplied [[raw/...]] source. It must capture durable",
                    "observations or decisions, not todos, diagnostics, or a transcript recap.",
                    "The summary must end with a 'Source conversations' section containing",
                    "every supplied source wikilink exactly once."
    )
    prompt <- paste("Existing topical candidates:", candidate_json,
                    "Conversation evidence:", evidence, sep = "\n\n")
    raw <- .thread_generate(prompt, system, model, provider, max_tokens,
                            generate_fn)
    json <- sub("^```json\\s*", "", raw, ignore.case = TRUE, perl = TRUE)
    json <- sub("\\s*```$", "", json, perl = TRUE)
    route <- tryCatch(jsonlite::fromJSON(json, simplifyVector = TRUE),
                      error = function(e) NULL)
    if (is.null(route) || !is.list(route)) {
        stop("general conversation router returned invalid JSON", call. = FALSE)
    }
    page_paths <- vapply(candidates$pages, function(x) x$path, character(1L))
    room_ids <- vapply(candidates$rooms, function(x) {
        x$room_id %||% x$id %||% ""
    }, character(1L))
    room_names <- vapply(candidates$rooms, function(x) x$name, character(1L))
    existing <- as.character(route$existing_path %||% "")[[1L]]
    room_id <- as.character(route$room_id %||% route$id %||% "")[[1L]]
    room <- .matrix_room_name_key(route$room_name %||% "")
    if (nzchar(existing) && !existing %in% page_paths) {
        existing <- ""
    }
    if (nzchar(room_id) && !room_id %in% room_ids) {
        room_id <- ""
    }
    if (!nzchar(room_id) && nzchar(room) && room %in% room_names) {
        room_id <- room_ids[[match(room, room_names)]]
    }
    if (nzchar(room_id)) {
        idx <- match(room_id, room_ids)
        if (!is.na(idx) && nzchar(room_names[[idx]])) {
            room <- room_names[[idx]]
        }
    } else if (nzchar(room) && !room %in% room_names) {
        room <- ""
    }
    list(
         # No "General" fallback: a missing topic is the router declining
         # to name one, and inventing a label here would file the
         # conversation under a theme nothing chose and open a room for it.
         topic = .thread_durable_topic(route$topic %||% ""),
         slug = as.character(route$slug %||% route$topic %||% "general")[[1L]],
         existing_path = existing,
         room_id = room_id,
         room_name = room,
         confidence = suppressWarnings(as.numeric(route$confidence %||% 0)),
         summary = as.character(route$summary %||% "")[[1L]]
    )
}

.thread_slug <- function(x) {
    x <- iconv(as.character(x)[[1L]], to = "ASCII//TRANSLIT")
    x <- tolower(gsub("[^A-Za-z0-9]+", "-", x))
    x <- gsub("(^-+|-+$)", "", x)
    if (!nzchar(x)) {
        "conversation-topic"
    } else {
        x
    }
}

.thread_relative <- function(path, vault) {
    path <- .resolve_path(path)
    vault <- normalizePath(vault, winslash = "/", mustWork = TRUE)
    prefix <- paste0(vault, .Platform$file.sep)
    if (!startsWith(path, prefix)) {
        stop("path is outside the target vault: ", path, call. = FALSE)
    }
    substring(path, nchar(prefix) + 1L)
}

.thread_source_label <- function(source) {
    switch(source, claude = "Claude Code", codex = "Codex",
           matrix = "Matrix", source)
}
.thread_instance_dir <- function(path = NULL) {
    if (is.null(path) || !length(path) || !nzchar(path[[1L]])) {
        path <- Sys.getenv("CEREBRO_INSTANCE_DIR", unset = "")
    }
    if (!nzchar(path)) {
        path <- getwd()
    }
    normalizePath(path.expand(path), mustWork = TRUE)
}

.thread_vault <- function(vault = NULL, instance_dir) {
    path <- vault %||% file.path(instance_dir, "vault")
    normalizePath(path.expand(path), mustWork = TRUE)
}

.thread_room_prefix <- function(prefix = NULL, instance_dir) {
    if (!is.null(prefix)) {
        if (!is.character(prefix) || length(prefix) != 1L ||
            is.na(prefix) || !nzchar(trimws(prefix))) {
            stop("room_prefix must be a non-empty character scalar",
                 call. = FALSE)
        }
        return(trimws(prefix))
    }
    cfg <- tryCatch(read_instance_config(instance_dir),
                    error = function(e) list())
    name <- cfg$instance %||% basename(instance_dir)
    if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
        name <- basename(instance_dir)
    }
    name
}
