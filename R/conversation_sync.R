# Durable conversation routing for repository and general topical threads.

# The opening of every summary this package posts. Emitters build the
# message through .thread_summary_message() and the archive parser
# recognizes it through .chat_is_generated_summary(), both from this one
# literal -- a wording edit that only touched one of them would silently
# restart the feedback loop the filter exists to break.
.THREAD_SUMMARY_LEAD <- "Conversation summary filed in **"

.thread_summary_message <- function(topic, summary) {
    paste0(.THREAD_SUMMARY_LEAD, topic, "**\n\n", summary)
}

# Content identity for a collected conversation: the ordered sequence of
# its turns, exactly as parsed. Metadata that varies between copies of
# the same conversation -- file path, archive date, modification time --
# is excluded on purpose, because that variation is exactly what made
# copies look distinct.
#
# Text is compared verbatim. An earlier version trimmed it, which made
# the identity not quite the full content it claimed to be; whitespace
# carries meaning in Markdown.
#
# Scoped to a conversation namespace where one exists, so two separate
# rooms holding the same short exchange stay separate. Matrix archives
# carry a room id; other sources fall back to an explicit legacy
# namespace rather than silently sharing one.
.thread_session_fingerprint <- function(session) {
    ns <- session$room_id %||% ""
    if (!nzchar(ns)) {
        # Claude and Codex sessions have no room, so they take the
        # context they ran in: the git root when there is one, otherwise
        # the working directory. Without it, the same exchange in two
        # repositories collapses into one and the second is never
        # summarized.
        repo <- tryCatch(.thread_session_repo(session), error = function(e) "")
        if (!nzchar(repo %||% "")) {
            repo <- session$cwd %||% ""
        }
        ns <- paste0("legacy:", session$source %||% "", ":", repo, ":",
                     session$label %||% "")
    }
    digest::digest(list(namespace = ns,
                        turns = lapply(session$turns %||% list(), function(t) {
        list(role = as.character(t$role %||% ""),
             text = as.character(t$text %||% ""))
    })), algo = "sha256")
}

# Collapse conversations that are byte-identical in content.
#
# The same room gets archived repeatedly -- historically on every
# restart, and still once per archive pass -- so one conversation
# arrives as many sources. Each was summarized separately, at the cost
# of an LLM call apiece, and each posted its own message to the same
# destination. One live room reached 98 archives of a single
# conversation.
#
# Identity is full content rather than an event id: archives predating
# the Matrix event ledger carry no ids, and inventing an identity they
# cannot support would be worse than comparing what they do have.
#
# The first occurrence wins. Collection sorts newest-first, so that is
# the most recent copy -- the one whose archive is least likely to age
# out of the lookback next run.
.thread_dedupe_sessions <- function(sessions) {
    if (length(sessions) < 2L) {
        return(sessions)
    }
    seen <- character()
    keep <- logical(length(sessions))
    for (i in seq_along(sessions)) {
        fp <- .thread_session_fingerprint(sessions[[i]])
        keep[[i]] <- !(fp %in% seen)
        seen <- c(seen, fp)
    }
    dropped <- sum(!keep)
    if (dropped > 0L) {
        message(sprintf("cerebro: %d duplicate conversation source(s) ignored",
                        dropped))
    }
    sessions[keep]
}

#' File coding conversations into repository threads
#'
#' Finds recent Claude Code, Codex, and archived Matrix conversations that can
#' be tied to a git repository. Each changed conversation is preserved as raw
#' evidence and summarized into the matching repository wiki page. A linked
#' notification for conversations originating elsewhere is posted to
#' one persistent Matrix room per repository. Rooms are titled with the
#' shell-style repository path (\code{~/repo}, or the absolute path outside
#' the home directory); rooms created under the older
#' \code{"<prefix> · <repo>"} title are found and renamed automatically.
#' The room topic uses corteza's \code{"<repo path> | <description>"}
#' convention, so tools in the room are scoped to that repository.
#'
#' @param vault Vault root, or NULL to use \code{<instance_dir>/vault}.
#' @param instance_dir Instance directory. NULL uses \code{CEREBRO_INSTANCE_DIR}
#'   and then the current working directory.
#' @param days Recent-session lookback.
#' @param max_sessions Maximum source sessions to inspect.
#' @param model LLM model identifier. Required unless \code{generate_fn}
#'   is supplied; there is no default.
#' @param provider LLM provider identifier.
#' @param max_tokens Maximum generated tokens when supported by the provider.
#' @param matrix_config Matrix JSON config, or NULL for corteza resolution.
#' @param invite Matrix users invited to newly created rooms, or to
#'   \code{space} once when one is named.
#' @param room_prefix Display-name prefix identifying rooms created before
#'   the path-style titles, so they can be renamed. NULL uses the configured
#'   instance name or the instance-directory basename.
#' @param space Character or NULL. Name of a Matrix space to file new repo
#'   rooms under. One room per repository is a volume generator -- 45 had
#'   accumulated before anyone counted them -- and a space collapses that to
#'   a single sidebar entry without merging any histories.
#' @param force Reprocess unchanged conversations.
#' @param dry_run Generate and report without writes or Matrix sends.
#' @param date Date used to bound session discovery.
#' @param generate_fn Optional test/custom generation function.
#' @param client A chat.api client, or NULL to build one from
#'   \code{matrix_config} the first time a room is needed.
#' @return Invisibly, a list describing updated repo threads.
#' @export
sync_repo_conversation_threads <- function(vault = NULL, instance_dir = NULL,
    days = 3L, max_sessions = 24L,
    model = NULL, provider = "auto",
    max_tokens = 1400L,
    matrix_config = NULL,
    invite = character(),
    room_prefix = NULL, space = NULL,
    force = FALSE, dry_run = FALSE,
    date = Sys.Date(),
    generate_fn = NULL, client = NULL) {
    if (is.null(generate_fn)) {
        .require_model(model)
    }
    instance_dir <- .thread_instance_dir(instance_dir)
    vault <- .thread_vault(vault, instance_dir)
    room_prefix <- .thread_room_prefix(room_prefix, instance_dir)
    chat <- .lazy_client(client, matrix_config)
    # Resolved on first use, as in the general sweep: a run that files
    # nothing should not leave an empty space behind.
    space_id <- NULL
    space_done <- FALSE
    resolve_space <- function() {
        if (!space_done) {
            space_done <<- TRUE
            if (is.character(space) && length(space) == 1L && nzchar(space)) {
                space_id <<- ensure_matrix_space(space, invite = invite,
                    instance_dir = instance_dir, dry_run = dry_run,
                    client = chat())
            }
        }
        space_id
    }
    ctx <- new.env(parent = emptyenv())
    ctx$date <- as.Date(date)
    ctx$instance_dir <- instance_dir
    sessions <- .chat_collect(ctx, list(
                                        include = c("claude", "codex", "matrix"),
                                        matrix_root = file.path(vault, "raw", "matrix"),
                                        days = days,
                                        max_sessions = max_sessions,
                                        max_per_source = max_sessions
        ))
    if (!length(sessions)) {
        return(invisible(list()))
    }

    repo_roots <- vapply(sessions, .thread_session_repo, character(1L))
    keep <- nzchar(repo_roots)
    sessions <- sessions[keep]
    repo_roots <- repo_roots[keep]
    if (!length(sessions)) {
        return(invisible(list()))
    }

    state <- read_instance_state(instance_dir)
    source_state <- state$conversation_threads$repo_sources %||% list()
    groups <- split(seq_along(sessions), repo_roots)
    updated <- list()
    for (repo_root in names(groups)) {
        prepared <- .thread_prepare_records(sessions[groups[[repo_root]]],
            vault, source_state, force = force, dry_run = dry_run)
        source_state <- prepared$state
        if (!isTRUE(dry_run)) {
            state$conversation_threads$repo_sources <- source_state
            .thread_write_conversation_state(state, instance_dir, "repo_sources")
        }
        records <- prepared$records
        if (!length(records)) {
            next
        }

        repo <- basename(repo_root)
        page <- .thread_repo_page(vault, repo)
        existing <- if (file.exists(page)) {
            .thread_file_body(page, 7000L)
        } else {
            ""
        }
        evidence <- .thread_records_prompt(records)
        system <- paste(
                        "Write one concise durable update for a repository knowledge page.",
                        "Return Markdown body only. Capture decisions, design principles,",
                        "changed assumptions, and non-obvious lessons. Do not write a task",
                        "list, status recap, transcript recap, or diagnostics. Distinguish",
                        "the user's decisions from assistant suggestions. Every substantive",
                        "claim must cite the exact supplied [[raw/...]] source wikilink.",
                        "End with a 'Source conversations' section containing every",
                        "supplied source wikilink exactly once."
        )
        prompt <- paste(
                        "Repository:", repo,
                        "Repository path:", .chat_display_path(repo_root),
                        "Existing wiki page excerpt:", existing,
                        "New conversation evidence:", evidence,
                        sep = "\n\n"
        )
        summary <- .thread_generate(prompt, system, model, provider,
                                    max_tokens, generate_fn)
        .thread_require_citations(summary, records)
        marker <- digest::digest(vapply(records, `[[`, character(1L), "hash"),
                                 algo = "sha1")
        title <- repo
        if (isTRUE(dry_run)) {
            message("cerebro: would update ", page)
        } else {
            .thread_append_page(page, title, c("repo", repo), summary,
                                marker, vault, date)
        }

        room_name <- .chat_display_path(repo_root)
        legacy_room <- paste(room_prefix, repo, sep = " \u00b7 ")
        room_topic <- paste0(.chat_display_path(repo_root),
                             " | Persistent repo thread; wiki: ",
                             .thread_relative(page, vault))
        rid <- ensure_matrix_room(
                                  room_name, topic = room_topic, invite = invite,
                                  instance_dir = instance_dir,
                                  dry_run = dry_run, legacy_name = legacy_room,
                                  space = resolve_space(), client = chat()
        )
        .thread_send_update(
                            repo, records, page, vault, room = rid,
                            dry_run = dry_run, instance_dir = instance_dir,
                            client = chat(), summary = summary
        )
        if (!isTRUE(dry_run)) {
            source_state <- .thread_mark_summarized(source_state, records)
            state$conversation_threads$repo_sources <- source_state
            .thread_write_conversation_state(state, instance_dir, "repo_sources")
        }
        updated[[repo]] <- list(page = page, room = rid,
                                sources = length(records))
    }
    invisible(updated)
}

#' File general conversations into topical threads
#'
#' Reads recent archived Matrix conversations plus any
#' local Claude Code or Codex sessions not rooted in a git repository. An LLM
#' routes each changed conversation to an existing personal-wiki topic and
#' Matrix room when possible. A durable new topic creates one wiki page and one
#' private topical room only when the routing confidence clears
#' \code{create_threshold}. A linked notification resurfaces the room only for
#' records originating elsewhere. Conversations already in that room, including
#' its threads, still update the wiki and are marked processed without a post.
#'
#' @param vault Personal pensar vault, or NULL to use \code{<instance_dir>/vault}.
#' @param instance_dir Instance directory. NULL uses \code{CEREBRO_INSTANCE_DIR}
#'   and then the current working directory.
#' @param matrix_roots Matrix raw archives to inspect.
#' @param exclude_matrix_rooms Character vector of Matrix room ids whose
#'   archives are never routed. \code{matrix_roots} cannot express this:
#'   it selects directories, and every room in an instance shares one.
#'   Matching is on the room id recorded in the archive, so a room can be
#'   suppressed without its name or its files changing.
#' @param days Recent-session lookback.
#' @param max_sessions Maximum source sessions to inspect.
#' @param reuse_threshold Minimum confidence for reusing an existing topic.
#' @param create_threshold Minimum confidence for a new topic and room.
#' @param create_min_turns Substance floor for a new topic and room: the
#'   conversation needs at least this many user turns, or it needs to
#'   clear \code{create_min_chars}. Routing into an existing topic is
#'   never gated by the floor.
#' @param create_min_chars The other arm of the substance floor: at
#'   least this many characters of user-authored text opens a new topic
#'   regardless of turn count.
#' @param model LLM model identifier. Required unless \code{generate_fn}
#'   is supplied; there is no default.
#' @param provider LLM provider identifier.
#' @param max_tokens Maximum generated tokens when supported by the provider.
#' @param matrix_config Matrix JSON config, or NULL for corteza resolution.
#' @param invite Matrix users invited to newly created rooms, or to
#'   \code{space} once when one is named.
#' @param room_prefix Legacy display-name prefix. Rooms are named for their
#'   topic alone; this is the older \code{"<prefix> · <topic>"} form,
#'   still used to find and rename rooms created under it, and to word the
#'   room topic. NULL uses the configured instance name or the
#'   instance-directory basename.
#' @param space Character or NULL. Name of a Matrix space to file new topic
#'   rooms under. With one, \code{invite} goes to the space once instead of
#'   to every room: eleven new topic rooms in a morning is eleven invites
#'   without it, and one with.
#' @param force Reprocess unchanged conversations.
#' @param dry_run Generate and report without writes or Matrix sends.
#' @param date Date used to bound session discovery.
#' @param generate_fn Optional test/custom generation function.
#' @param client A chat.api client, or NULL to build one from
#'   \code{matrix_config} the first time a room is needed.
#' @return Invisibly, a list describing updated topical threads.
#' @export
sync_general_conversation_threads <- function(vault = NULL,
    instance_dir = NULL,
    matrix_roots = NULL, days = 7L,
    max_sessions = 18L,
    reuse_threshold = 0.55,
    create_threshold = 0.72,
    create_min_turns = 3L,
    create_min_chars = 400L,
    model = NULL,
    provider = "auto",
    max_tokens = 1600L,
    matrix_config = NULL,
    invite = character(),
    room_prefix = NULL,
    space = NULL, force = FALSE,
    dry_run = FALSE,
    date = Sys.Date(),
    generate_fn = NULL,
    exclude_matrix_rooms = character(),
    client = NULL) {
    if (is.null(generate_fn)) {
        .require_model(model)
    }
    instance_dir <- .thread_instance_dir(instance_dir)
    vault <- .thread_vault(vault, instance_dir)
    room_prefix <- .thread_room_prefix(room_prefix, instance_dir)
    chat <- .lazy_client(client, matrix_config)
    # Resolved on first use rather than up front: a sweep that files
    # nothing should not leave an empty space and an invite behind.
    space_id <- NULL
    space_done <- FALSE
    resolve_space <- function() {
        if (!space_done) {
            space_done <<- TRUE
            if (is.character(space) && length(space) == 1L && nzchar(space)) {
                space_id <<- ensure_matrix_space(space, invite = invite,
                    instance_dir = instance_dir, dry_run = dry_run,
                    client = chat())
            }
        }
        space_id
    }
    matrix_roots <- matrix_roots %||% file.path(vault, "raw", "matrix")
    ctx <- new.env(parent = emptyenv())
    ctx$date <- as.Date(date)
    ctx$instance_dir <- instance_dir
    sessions <- .chat_collect(ctx, list(
                                        include = c("matrix", "claude", "codex"),
                                        matrix_root = matrix_roots,
                                        exclude_matrix_rooms = exclude_matrix_rooms,
                                        days = days,
                                        max_sessions = max_sessions,
                                        max_per_source = max_sessions
        ))
    if (!length(sessions)) {
        return(invisible(list()))
    }
    sessions <- Filter(function(x) !nzchar(.thread_session_repo(x)), sessions)
    if (!length(sessions)) {
        return(invisible(list()))
    }
    # Before routing, and so before any LLM call: identical copies of one
    # conversation must not each be summarized and posted.
    sessions <- .thread_dedupe_sessions(sessions)

    state <- read_instance_state(instance_dir)
    source_state <- state$conversation_threads$general_sources %||% list()
    # Rooms are candidates only when the transport is reachable; the
    # pages alone still route.
    candidates <- .thread_topic_candidates(vault, tryCatch(chat(),
            error = function(e) NULL))
    updated <- list()

    # Phase 1: route every conversation. Raw evidence and its state are
    # written here, but no destination page is touched and nothing is
    # sent. Sending from inside this loop is what produced one message
    # per source: several conversations routed to one topic each posted
    # separately into the same room.
    pending <- list()
    destinations <- character()
    for (session in sessions) {
        prepared <- .thread_prepare_records(list(session), vault,
            source_state, force = force, dry_run = dry_run)
        source_state <- prepared$state
        if (!isTRUE(dry_run)) {
            # Raw evidence is already on disk at this point, so its state
            # is persisted here. Summarized state is not: that waits for
            # a successful write in phase 2.
            state$conversation_threads$general_sources <- source_state
            .thread_write_conversation_state(state, instance_dir, "general_sources")
        }
        records <- prepared$records
        if (!length(records)) {
            next
        }

        route <- .thread_route_general(records, candidates, model, provider,
                                       max_tokens, generate_fn)
        if (nzchar(route$existing_path)) {
            threshold <- reuse_threshold
        } else {
            threshold <- create_threshold
        }

        if (is.na(route$confidence) || route$confidence < threshold) {
            message("cerebro: general conversation did not clear routing ",
                    "threshold; leaving it as raw evidence")
            next
        }
        # Checked separately from confidence, which cannot see this: the
        # router can be entirely confident that there is no durable topic,
        # and that answer used to arrive as a topic name and become a room.
        if (!nzchar(route$topic)) {
            message("cerebro: general conversation has no durable topic; ",
                    "leaving it as raw evidence")
            next
        }
        # A new topic also clears a substance floor, which confidence and
        # topic cannot see: a two-line test ping can carry a confident,
        # perfectly durable-sounding theme, and it used to mint a room.
        # Reuse stays ungated -- filing a small exchange into a room that
        # exists is the cheap case; opening a room for it is the noise.
        if (!nzchar(route$existing_path)) {
            substance <- .thread_session_substance(session)
            if (substance$turns < create_min_turns &&
                substance$chars < create_min_chars) {
                message("cerebro: general conversation is too slight to ",
                        "open a topic room (", substance$turns,
                        " user turn(s), ", substance$chars,
                        " character(s)); leaving it as raw evidence")
                next
            }
        }
        page <- if (nzchar(route$existing_path)) {
            file.path(vault, route$existing_path)
        } else {
            file.path(vault, "wiki", paste0(.thread_slug(route$slug %||%
                        route$topic), ".md"))
        }
        .thread_require_citations(route$summary, records)

        # The wiki page is the destination identity. It does not imply
        # the room: the router chooses page and room independently, so
        # phase 2 has to reconcile the group's room decisions separately.
        key <- normalizePath(page, mustWork = FALSE)
        if (!(key %in% destinations)) {
            destinations <- c(destinations, key)
        }
        pending[[key]] <- c(pending[[key]] %||% list(),
                            list(list(route = route, records = records, page = page)))
    }

    # Phase 2: one page update, one room, one message per destination.
    for (key in destinations) {
        group <- pending[[key]]
        route <- group[[1L]]$route
        page <- group[[1L]]$page
        records <- unlist(lapply(group, `[[`, "records"), recursive = FALSE)
        body <- .thread_group_body(group)
        marker <- .thread_group_marker(records)

        # The router picks existing_path and room_id independently, so a
        # shared page does NOT imply a shared room. Take the group's one
        # known room if it has one, create only when none does, and
        # refuse to guess between two -- guessing would post a merged
        # update into whichever route happened to be first.
        known <- .thread_group_room_ids(group)
        if (length(known) > 1L) {
            message("cerebro: conversations routed to ", basename(page),
                    " disagree on the destination room; leaving them as raw ",
                    "evidence rather than choosing one")
            next
        }

        if (isTRUE(dry_run)) {
            message(sprintf("cerebro: would update %s from %d conversation(s)",
                            page, length(group)))
        } else {
            .thread_append_page(page, route$topic,
                                c("conversation", "personal-context"),
                                body, marker, vault, date)
        }

        if (length(known) == 1L) {
            rid <- known
        } else {
            # The topic names the room. The persona prefix used to lead
            # every one of these ("mybot · Travel"), which says the same
            # thing in every room in a sidebar that is already the
            # persona's. It survives as legacy_name so the rooms that
            # carry it are renamed and reused rather than duplicated
            # alongside fresh unprefixed ones.
            if (nzchar(route$room_name)) {
                room_name <- route$room_name
            } else {
                room_name <- route$topic
            }
            legacy_room <- paste(room_prefix, route$topic, sep = " \u00b7 ")

            rid <- ensure_matrix_room(
                                      room_name,
                                      topic = paste0(room_prefix, " topical thread; wiki: ",
                    .thread_relative(page, vault)),
                                      invite = invite,
                                      instance_dir = instance_dir, dry_run = dry_run,
                                      legacy_name = legacy_room,
                                      space = resolve_space(), client = chat()
            )
        }
        # A general topical thread is also a fold destination. Unstamped,
        # .topic_rooms() cannot see it, so the fold sweep classifies
        # against a near-empty menu and opens a second room for a
        # subject that already has a thread. Stamping here rather than
        # only at creation also repairs the rooms that predate the fold
        # subsystem, on the sweep that next touches them. Repo threads
        # stay unstamped on purpose: a personal segment does not belong
        # in a repository's room. A transport that cannot be reached
        # skips the stamp; the next sweep retries.
        if (!isTRUE(dry_run)) {
            .stamp_topic_room(tryCatch(chat(), error = function(e) NULL),
                              rid, route$topic)
        }
        .thread_send_update(
                            route$topic, records, page, vault, room = rid,
                            dry_run = dry_run, instance_dir = instance_dir,
                            client = chat(),
                            summary = if (length(group) == 1L) route$summary
        )
        if (!isTRUE(dry_run)) {
            # Every contributing record advances together, and only now:
            # a failed write above leaves them all pending for retry
            # rather than marking some of them done.
            source_state <- .thread_mark_summarized(source_state, records)
            state$conversation_threads$general_sources <- source_state
            .thread_write_conversation_state(state, instance_dir, "general_sources")
        }
        # No candidate refresh here. The old per-session loop rebuilt
        # them so the next session's routing saw pages and rooms this
        # one had just created; all routing now happens in phase 1, so
        # nothing would read the new value. It was a vault read and a
        # Matrix room enumeration per destination that could fail a run
        # after its state had already advanced.
        #
        # Routing all sessions against one snapshot has a known limit.
        # A route that creates a new page derives it from the slug the
        # router returned, and the router supplies that independently
        # per conversation. Two routes can agree on a topic and still
        # return different slugs, landing on separate pages, because
        # phase 1 cannot see destinations earlier routes are pending.
        #
        # Bounded, and unlike the fan-out this replaced it does not
        # compound: each page is still written once.
        #
        # The two ways out are not equivalent. Deriving new-page
        # identity from the topic instead of the returned slug ENFORCES
        # the invariant. Showing later phase-1 routes the destinations
        # earlier ones are pending only MITIGATES it: the router can see
        # the pending candidate and still return a new slug. See #34.
        #
        # Keyed by destination, not topic: two pages can carry the same
        # topic string, and keying by topic let one silently replace the
        # other in the result.
        updated[[key]] <- list(page = page, room = rid, topic = route$topic,
                               sources = length(group))
    }
    invisible(updated)
}

# A readable heading for one contributing conversation: the room name as
# it read when archived, plus the date. The label alone repeats when the
# same room contributes twice, which is the common case. Presentation
# only -- never used for grouping, markers, or state.
.thread_group_heading <- function(g) {
    label <- g$records[[1L]]$session$label %||% g$route$topic %||% "source"
    when <- tryCatch(as.character(as.Date(g$records[[1L]]$session$time)),
                     error = function(e) "")
    if (nzchar(when %||% "") && !is.na(when)) {
        paste0(label, " \u2014 ", when)
    } else {
        label
    }
}

# The distinct rooms a destination's routes already know about.
#
# The router chooses existing_path and room_id independently, so routes
# sharing a page can disagree about the room. Zero means none is known
# and one must be created; one is the answer; more than one is a
# disagreement the caller must refuse rather than resolve by picking
# whichever route happened to sort first.
.thread_group_room_ids <- function(group) {
    unique(Filter(nzchar, vapply(group, function(g) {
        as.character(g$route$room_id %||% "")
    }, character(1L))))
}

# Identity of a destination's complete contributing source set, in
# canonical order.
#
# The page block is delimited by this marker, so an append with a marker
# already present is a no-op. Using only the first record's hash meant a
# retry whose group gained or lost sources -- while keeping the same
# first source -- silently wrote nothing, and those sources could then
# be marked summarized without ever reaching the wiki.
.thread_group_marker <- function(records) {
    hashes <- sort(vapply(records, function(r) as.character(r$hash %||% ""),
                          character(1L)))
    digest::digest(paste(hashes, collapse = "|"), algo = "sha1")
}

# One body for a destination, preserving each contributing conversation
# as its own section with its raw evidence linked. A single source keeps
# its summary verbatim, so the common case reads as it did before.
.thread_group_body <- function(group) {
    if (length(group) <= 1L) {
        return(group[[1L]]$route$summary)
    }
    # No citation is appended here. The router is asked to end each
    # summary with a "Source conversations" section listing every
    # supplied wikilink, and .thread_require_citations() checks that the
    # links are present -- not that they appear once, or where. Adding
    # one on top would duplicate links the summary already carries.
    parts <- vapply(group, function(g) {
        paste0("### ", .thread_group_heading(g), "\n\n", g$route$summary)
    }, character(1L))
    paste(parts, collapse = "\n\n")
}

# Both filers notify only about records from elsewhere. Suppression
# affects the notification, not the page or its processed-source state.
# Missing origin metadata stays eligible, including CLI conversations.
.thread_send_update <- function(topic, records, page, vault, room,
                                dry_run = FALSE, instance_dir = NULL,
                                client = NULL, summary = NULL) {
    incoming <- Filter(function(record) {
        !isTRUE(record$session$room_id == room)
    }, records)
    if (!length(incoming)) {
        return(invisible(NULL))
    }
    text <- if (length(incoming) == length(records) && !is.null(summary)) {
        .thread_summary_message(topic, summary)
    } else {
        .thread_update_message(topic, incoming, page, vault)
    }
    matrix_send(text, room = room, dry_run = dry_run,
                instance_dir = instance_dir, client = client)
}

# A linked handoff built from eligible records only. In particular, a
# repo summary can mix origins in one paragraph and cannot be filtered
# by deleting citations. When any source is suppressed, keep that complete
# summary on the wiki page; unfiltered single-source and repo posts retain it.
.thread_update_message <- function(topic, records, page, vault) {
    lines <- vapply(records, function(record) {
        heading <- .thread_group_heading(list(records = list(record),
                route = list(topic = topic)))
        paste0("- ", heading, " [[", record$link, "]]")
    }, character(1L))
    .thread_summary_message(topic, paste(c(
                sprintf("%d conversation%s filed.", length(records),
                    if (length(records) == 1L) "" else "s"),
                "",
                lines,
                "",
                paste0("Full notes: [[", .thread_relative(page, vault), "]]")
            ), collapse = "\n"))
}

.thread_write_conversation_state <- function(state, instance_dir, source_key) {
    current <- read_instance_state(instance_dir)
    current$conversation_threads <- current$conversation_threads %||% list()
    current$conversation_threads[[source_key]] <-
    (state$conversation_threads %||% list())[[source_key]] %||% list()
    write_instance_state(current, instance_dir)
}
