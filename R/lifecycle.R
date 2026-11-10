# The lifecycle schema's event types. cerebro writes them here, corteza
# reads them at /clear and on a thread reply, and a client that groups
# rooms by state reads them too; the names are the contract.
.LIFECYCLE_TYPE <- "ai.cornball.lifecycle"
.FOLD_TYPE <- "ai.cornball.fold"

#' Fold stale conversation segments into topic rooms
#'
#' The fold half of the conversation lifecycle. Segment rooms (opened by
#' \code{\link{segment_open}} when corteza clears a conversation, marked
#' by an \code{ai.cornball.lifecycle} state event with
#' \code{state: "segment"}) sink by recency and are
#' eventually folded: a root message carrying the segment's name and
#' archived-transcript pointer is posted to a topic room as an
#' \code{m.notice}, the segment's lifecycle event flips to
#' \code{state: "folded"} with a \code{folded_into} pointer, and the
#' segment room is tombstoned toward the topic room so stock clients
#' render the handoff.
#'
#' Each fold also writes an \code{ai.cornball.fold} state event into the
#' topic room, keyed by the thread root and holding the segment room and
#' its archive pointer. That index is what lets a reply in the thread be
#' answered with the conversation it continues: one state read, rather
#' than a scan of the topic room's history for a root that sinks further
#' back with every later fold.
#'
#' A segment folds when either trigger fires, both keyed to the last
#' \emph{human} message in the segment room (bot traffic keeps nothing
#' alive; a segment no one ever spoke in ages from its creation):
#' \itemize{
#'   \item idle longer than \code{fold_after_days}, or
#'   \item more than \code{max_segments} segments exist (sidebar
#'     pressure) — the oldest fold first, but never one idle less than
#'     \code{fold_min_days}.
#' }
#'
#' Topic assignment never asks a question. Each fold is classified
#' against the existing topic rooms with a self-rated confidence: at or
#' above \code{topic_confidence} it files silently, a proposed new
#' topic that itself clears the bar is created and announced as a
#' notice (veto or rename at leisure), and everything else — low
#' confidence, unreadable replies, a provider outage — lands in
#' \code{unsorted_topic}, silently. The confidence is the model's own
#' estimate, ordinal rather than calibrated: the knob to turn when
#' misfiles annoy, not a probability.
#'
#' Each segment folds independently; one failure logs and leaves the
#' rest of the sweep running. Segments whose activity cannot be dated
#' (no messages readable and no \code{since} on the state event) are
#' skipped with a message rather than folded on a guess.
#'
#' @param fold_after_days Integer. Idle days before a segment folds.
#' @param max_segments Integer. Segments allowed before pressure folds
#'   the oldest.
#' @param fold_min_days Integer. Pressure never folds a segment idle
#'   fewer days than this.
#' @param topic_confidence Numeric. Minimum classification confidence
#'   to file into (or create) a topic; below it, folds go to
#'   \code{unsorted_topic}.
#' @param unsorted_topic Character. Topic room for unclassified folds.
#' @param model Character. The classifier model id, as in
#'   \code{llm.api::chat}. Required; there is no default.
#' @param provider Character or NULL. The classifier provider. NULL uses
#'   \code{"anthropic"}.
#' @param room Character or NULL. Room for the sweep's summary and
#'   new-topic notices (sent via \code{\link{matrix_send}}, so
#'   \code{m.notice}); NULL sends none.
#' @param matrix_config Character or NULL. Explicit corteza Matrix JSON
#'   config, as in \code{\link{matrix_send}}.
#' @param instance_dir Character or NULL. State cache for topic-room
#'   resolution.
#' @param space Character or NULL. Name of a Matrix space to file new topic
#'   rooms under, as in
#'   \code{\link{sync_general_conversation_threads}}. Without one a topic
#'   room created here has no members but the persona, so it is not
#'   reachable at all; with one it appears in the space.
#' @param invite Character vector of Matrix user IDs, invited to
#'   \code{space} once when it is created.
#' @param dry_run Logical. Report the plan without network writes.
#' @param client A chat.api client, or NULL to build one from
#'   \code{matrix_config}. The transport needs durable channel state and
#'   history (\code{chat_capabilities()$set_state} and \code{$history});
#'   one without them errors on first use.
#' @return Invisibly, a list with \code{folded} (list of
#'   \code{list(id, topic)}) and \code{skipped} (character ids).
#' @export
sweep_segments <- function(fold_after_days = 7L, max_segments = 12L,
                           fold_min_days = 2L, topic_confidence = 0.7,
                           unsorted_topic = "Unsorted", model,
                           provider = NULL, room = NULL,
                           matrix_config = NULL, instance_dir = NULL,
                           space = NULL, invite = character(),
                           dry_run = FALSE, client = NULL) {
    .require_model(model)
    client <- client %||% .chat_client(matrix_config)

    segs <- .segment_records(client)
    undated <- vapply(segs, function(s) is.na(s$last_active), logical(1))
    for (s in segs[undated]) {
        message("cerebro: segment ", s$id, " has no datable activity; skipping")
    }
    skipped <- vapply(segs[undated], function(s) s$id, character(1))
    segs <- segs[!undated]

    ids <- .fold_due(segs, now = Sys.time(),
                     fold_after_days = fold_after_days,
                     max_segments = max_segments,
                     fold_min_days = fold_min_days)
    if (!length(ids)) {
        message("cerebro: no segments due to fold (", length(segs), " active)")
        return(invisible(list(folded = list(), skipped = skipped)))
    }

    to_fold <- segs[vapply(segs, function(s) s$id %in% ids, logical(1))]

    # Every fold gets a destination before anything is written, so a
    # classifier outage mid-sweep cannot leave half the folds filed
    # differently from how the same sweep would have filed them all.
    topics <- .topic_rooms(client)
    decisions <- lapply(to_fold, function(s) {
        cls <- .classify_segment(.room_label(client, s$id),
                                 .segment_excerpt(s), topics,
                                 model = model,
                                 provider = provider %||% "anthropic")
        .topic_decision(cls, topics, topic_confidence, unsorted_topic)
    })

    # Resolve each distinct destination once. Existing topic rooms are
    # already known by id; new ones (Unsorted included, its first time)
    # are created, stamped, and — when genuinely new — announced.
    topic_rids <- list()
    # Resolved on first new topic room, not before: a sweep whose folds
    # all land in rooms that already exist should not create a space.
    space_id <- NULL
    space_done <- FALSE
    resolve_space <- function() {
        if (!space_done) {
            space_done <<- TRUE
            if (is.character(space) && length(space) == 1L && nzchar(space)) {
                space_id <<- ensure_matrix_space(space, invite = invite,
                    instance_dir = instance_dir,
                    dry_run = dry_run, client = client)
            }
        }
        space_id
    }
    for (d in decisions) {
        t <- d$topic
        if (!is.null(topic_rids[[t]])) {
            next
        }
        if (t %in% names(topics)) {
            topic_rids[[t]] <- topics[[t]]
            next
        }
        rid <- ensure_matrix_room(t, instance_dir = instance_dir,
                                  dry_run = dry_run, invite = invite,
                                  space = resolve_space(), client = client)
        if (!isTRUE(dry_run)) {
            # Tolerant: ensure_matrix_room() resolves by name, so this
            # rid can be a room the user created and the bot cannot
            # send state into. Erroring here would abandon a sweep
            # whose destinations were all resolved up front precisely
            # so that one failure could not scatter the folds.
            .stamp_topic_room(client, rid, t)
        }
        topic_rids[[t]] <- rid
        if (isTRUE(d$create)) {
            note <- paste0("New topic room: ", t)
            if (is.null(room)) {
                message("cerebro: ", note)
            } else {
                matrix_send(note, room = room, dry_run = dry_run,
                            instance_dir = instance_dir, client = client)
            }
        }
    }

    folded <- list()
    for (i in seq_along(to_fold)) {
        s <- to_fold[[i]]
        t <- decisions[[i]]$topic
        if (isTRUE(dry_run)) {
            message("cerebro: would fold ", s$id, " (idle since ",
                    format(s$last_active), ") into ", t)
            folded[[length(folded) + 1L]] <- list(id = s$id, topic = t)
            next
        }
        ok <- tryCatch({
            .fold_one(client, s, topic_rids[[t]], t)
            TRUE
        }, error = function(e) {
            message("cerebro: fold of ", s$id, " failed: ", conditionMessage(e))
            FALSE
        })
        if (ok) {
            folded[[length(folded) + 1L]] <- list(id = s$id, topic = t)
        }
    }

    if (length(folded) && !is.null(room)) {
        dests <- unique(vapply(folded, function(f) f$topic, character(1)))
        matrix_send(sprintf("Folded %d conversation%s into %s.",
                            length(folded),
                if (length(folded) == 1L) "" else "s",
                            paste(dests, collapse = ", ")),
                    room = room, dry_run = dry_run,
                    instance_dir = instance_dir, client = client)
    }
    invisible(list(folded = folded, skipped = skipped))
}

# Capability builder. Every knob is a field on the routine entry, with
# the sweep's defaults; the numeric ones are validated at registration.
.cap_lifecycle_sweep <- function(r, briefings, cfg, instance_dir) {
    fold_after_days <- .config_integer(r$fold_after_days, 7L,
                                       "fold_after_days", min = 1L)
    max_segments <- .config_integer(r$max_segments, 12L,
                                    "max_segments", min = 1L)
    fold_min_days <- .config_integer(r$fold_min_days, 2L,
                                     "fold_min_days", min = 0L)
    topic_confidence <- .config_number(r$topic_confidence, 0.7,
                                       "topic_confidence", min = 0, max = 1)
    unsorted_topic <- r$unsorted_topic %||% "Unsorted"
    model <- .require_model(r$model,
                            sprintf("routine %s",
                                    r$name %||% "lifecycle_sweep"))
    provider <- r$provider %||% NULL
    room <- r$room %||% NULL
    space <- r$space %||% NULL
    invite <- as.character(r$invite %||% character())
    dry_run <- isTRUE(r$dry_run)
    idir <- instance_dir
    function() {
        sweep_segments(fold_after_days = fold_after_days,
                       max_segments = max_segments,
                       fold_min_days = fold_min_days,
                       topic_confidence = topic_confidence,
                       unsorted_topic = unsorted_topic,
                       model = model, provider = provider,
                       room = room, space = space, invite = invite,
                       dry_run = dry_run, instance_dir = idir)
    }
}

# A room's lifecycle state event, or NULL when it has none or cannot be
# read. Absence is the ordinary answer -- most rooms are plain -- so a
# read error is folded into it rather than stopping an enumeration.
.lifecycle_state <- function(client, room_id) {
    tryCatch(chat.api::chat_get_state(client, room_id, .LIFECYCLE_TYPE),
             error = function(e) NULL)
}

# Every joined room carrying an ai.cornball.lifecycle event in state
# "segment", with the timestamp folding decisions run on.
.segment_records <- function(client) {
    rooms <- chat.api::chat_channels(client)
    out <- list()
    for (rid in rooms) {
        st <- .lifecycle_state(client, rid)
        if (!identical(st$state, "segment")) {
            next
        }
        out[[length(out) + 1L]] <- list(
                                        id = rid,
                                        state = st,
                                        last_active = .last_human_ts(client, rid, fallback = st$since)
        )
    }
    out
}

# When the human in a segment room last said anything: the newest
# message from a sender other than the bot. Bot notices must not keep a
# segment alive, so the bot's own messages are ignored, and the walk
# pages backwards -- a single window of recent messages could sit
# entirely on bot traffic and misdate the room. A room the human never
# spoke in ages from its lifecycle event's `since`. NA when neither
# source can date it -- the caller refuses to fold on NA rather than
# treating it as old.
#
# The walk is capped. When the cap is hit with history remaining, the
# oldest message scanned is returned instead: every scanned message is
# newer than any human message, so that timestamp is an upper bound --
# the segment can only look fresher than it is, which delays a fold and
# never causes a premature one.
.last_human_ts <- function(client, room_id, fallback = NULL, max_pages = 8L) {
    cursor <- NULL
    oldest <- NULL
    reached_start <- FALSE
    for (i in seq_len(max_pages)) {
        page <- tryCatch(
                         chat.api::chat_history(client, room_id, limit = 100L,
                cursor = cursor),
                         error = function(e) NULL
        )
        if (is.null(page)) {
            reached_start <- TRUE
            break
        }
        # A page runs oldest-first; the newest message is the one the
        # question is about, so each page is read from its end.
        for (m in rev(page$messages)) {
            at <- m$ts
            if (!inherits(at, "POSIXct") || length(at) != 1L || is.na(at)) {
                next
            }
            if (is.null(oldest) || at < oldest) {
                oldest <- at
            }
            if (!isTRUE(m$self)) {
                return(at)
            }
        }
        cursor <- page$cursor
        if (is.null(cursor) || !nzchar(cursor)) {
            reached_start <- TRUE
            break
        }
    }
    if (!reached_start && !is.null(oldest)) {
        return(oldest)
    }
    if (is.character(fallback) && length(fallback) == 1L && nzchar(fallback)) {
        parsed <- as.POSIXct(fallback, format = "%Y-%m-%dT%H:%M:%SZ",
                             tz = "UTC")
        if (!is.na(parsed)) {
            return(parsed)
        }
    }
    as.POSIXct(NA)
}

# Which segments fold, from timestamps alone. Pure so the policy is
# testable without a homeserver. `segs` is a list of list(id,
# last_active); returns the ids to fold, oldest first.
.fold_due <- function(segs, now, fold_after_days, max_segments, fold_min_days) {
    if (!length(segs)) {
        return(character())
    }
    ids <- vapply(segs, function(s) s$id, character(1))
    idle <- vapply(segs, function(s) {
        as.numeric(difftime(now, s$last_active, units = "days"))
    }, numeric(1))

    due <- idle > fold_after_days
    # Sidebar pressure: what survives the age fold still has to fit.
    # Oldest first, and never anything the human touched more recently
    # than fold_min_days -- pressure is a nudge, not a purge.
    keep <- which(!due)
    overflow <- length(keep) - max_segments
    if (overflow > 0L) {
        eligible <- keep[idle[keep] >= fold_min_days]
        eligible <- eligible[order(idle[eligible], decreasing = TRUE)]
        due[utils::head(eligible, overflow)] <- TRUE
    }
    ids[due][order(idle[due], decreasing = TRUE)]
}

# A room's display name, falling back to its id when the name cannot
# be read. What the classifier sees as the title and the fold posts as
# the root's first line.
.room_label <- function(client, room_id) {
    name <- .channel_info(client, room_id)$name
    if (is.null(name) || !length(name) || !nzchar(name[[1L]])) {
        room_id
    } else {
        as.character(name[[1L]])
    }
}

# Fold one segment: thread root in the topic room, tombstone, then the
# folded state event. Errors propagate; the sweep isolates them per
# segment.
#
# The state flip comes LAST because it is what stops the next sweep
# from selecting this room again -- it is the fold's commit point. A
# failure anywhere earlier leaves the room in state "segment" and the
# whole fold reruns, so every earlier step must tolerate a rerun: the
# tombstone is a state PUT (rewriting it is a no-op), and the root is
# deduplicated by .existing_root(), which finds the previous attempt's
# message by the segment room id its body carries.
.fold_one <- function(client, seg, topic_rid, topic_name) {
    root <- .existing_root(client, topic_rid, seg$id)
    if (is.null(root)) {
        body <- paste0(.room_label(client, seg$id),
            if (!is.null(seg$state$vault)) {
                paste0("\nArchived transcript: ", seg$state$vault)
            },
                       # The segment room id makes the root findable: retry dedup
                       # here, and the thread -> segment mapping rehydration needs.
                       "\nSegment: ", seg$id)
        ids <- chat.api::chat_send(client, topic_rid, body, kind = "notice")
        root <- as.character(ids)[[length(ids)]]
    }

    # The fold index, in the topic room, keyed by the thread root. It is
    # what lets a reply in this thread be answered with the conversation
    # it continues: the bot reads one state event rather than scanning
    # the topic room's history for a root that sinks further back with
    # every fold. Written here, before the commit point, and idempotent
    # like the tombstone -- a rerun rewrites the same content.
    index <- list(segment = seg$id)
    if (!is.null(seg$state$vault)) {
        index$vault <- seg$state$vault
    }
    chat.api::chat_set_state(client, topic_rid, .FOLD_TYPE, index,
                             state_key = root)

    chat.api::chat_set_state(client, seg$id, "m.room.tombstone",
                             list(body = paste0("Folded into ", topic_name),
                                  replacement_room = topic_rid))

    # segment_of, vault, and since survive the flip: since stays the
    # segment's creation time, and the fold gets its own timestamp.
    content <- seg$state
    content$state <- "folded"
    content$folded_into <- list(room = topic_rid, thread = root)
    content$folded_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    chat.api::chat_set_state(client, seg$id, .LIFECYCLE_TYPE, content)
    invisible(root)
}

# The thread root a previous fold attempt already posted for this
# segment, or NULL. Scans the topic room's recent messages for the
# segment room id that .fold_one() writes into every root. Bounded:
# a root older than the scan window is not found and the retry posts a
# second one -- rare (the retry follows the failure by one sweep) and
# preferred over an unbounded walk of a long-lived topic room.
.existing_root <- function(client, topic_rid, seg_id, max_pages = 3L) {
    needle <- paste0("\nSegment: ", seg_id)
    cursor <- NULL
    for (i in seq_len(max_pages)) {
        page <- tryCatch(
                         chat.api::chat_history(client, topic_rid, limit = 100L,
                cursor = cursor),
                         error = function(e) NULL
        )
        if (is.null(page)) {
            return(NULL)
        }
        for (m in rev(page$messages)) {
            body <- m$body
            if (is.character(body) && length(body) == 1L &&
                grepl(needle, body, fixed = TRUE) &&
                is.character(m$id) && nzchar(m$id)) {
                return(m$id)
            }
        }
        cursor <- page$cursor
        if (is.null(cursor) || !nzchar(cursor)) {
            return(NULL)
        }
    }
    NULL
}

# Stamp a topic room as such, once. The marker is what lets a later
# classification pass enumerate the existing topics, and writing it
# only when absent keeps a nightly sweep from appending an identical
# state event every run.
#
# Any other lifecycle state already on the room is a veto and is left
# alone. Renaming a room corrects a topic's label, but nothing could
# say "this room is not a fold destination": a room stamped `excluded`
# used to be re-stamped on the next sweep that touched its page. The
# case that forced it is a curated profile page, which is a fact sheet
# rather than a conversation bucket, and whose label is the most
# attractive entry on the menu for any personal conversation.
.ensure_topic_state <- function(client, room_id, topic_name) {
    st <- .lifecycle_state(client, room_id)
    if (identical(st$state, "topic")) {
        return(invisible(FALSE))
    }
    if (is.character(st$state) && length(st$state) == 1L && nzchar(st$state)) {
        return(invisible(FALSE))
    }
    chat.api::chat_set_state(client, room_id, .LIFECYCLE_TYPE,
                             list(state = "topic", topic = topic_name))
    invisible(TRUE)
}

# Stamp a room as a fold destination. The label is the room's current
# display name where it can be read, matching what .topic_rooms() will
# enumerate and leaving `label` as the fallback.
#
# A Matrix failure here is reported and swallowed. A missed stamp costs
# the classifier one candidate on the next sweep, which is not worth
# failing an already-filed thread over, and the next sweep retries. The
# live case is a room the user created and the bot joined as a plain
# member: state events default to power level 50, so the stamp is
# refused with M_FORBIDDEN while the room is otherwise a perfectly good
# destination that name lookup will keep reusing.
.stamp_topic_room <- function(client, room_id, label = "") {
    if (is.null(client) || !is.character(room_id) ||
        length(room_id) != 1L || !nzchar(room_id)) {
        return(invisible(FALSE))
    }
    nm <- .channel_info(client, room_id)$name
    if (!is.character(nm) || length(nm) != 1L || !nzchar(nm)) {
        nm <- label
    }
    if (!nzchar(nm)) {
        return(invisible(FALSE))
    }
    tryCatch(.ensure_topic_state(client, room_id, nm),
             error = function(e) {
        message("cerebro: could not stamp ", room_id, " as a topic room: ",
                conditionMessage(e))
        invisible(FALSE)
    })
}
