library(tinytest)

# The fold sweep: policy (.fold_due) is pure and tested exhaustively;
# the chat I/O around it is tested through one end-to-end sweep against
# chat.api's Matrix adapter with its homeserver calls replaced by seams
# (helper_matrix.R).

if (!requireNamespace("mx.client", quietly = TRUE)) {
    exit_file("mx.client not installed")
}
source("helper_matrix.R")

seg <- function(id, idle_days, now) {
    list(id = id, last_active = now - idle_days * 86400)
}

# ---- Policy ----
local({
    now <- as.POSIXct("2026-08-19 12:00:00", tz = "UTC")
    fd <- function(segs, ...) cerebro:::.fold_due(segs, now = now, ...)

    expect_identical(fd(list(), fold_after_days = 7, max_segments = 12,
                        fold_min_days = 2), character())

    # Age trigger: strictly older than fold_after_days.
    segs <- list(seg("a", 8, now), seg("b", 7, now), seg("c", 1, now))
    expect_identical(fd(segs, fold_after_days = 7, max_segments = 12,
                        fold_min_days = 2), "a")

    # Pressure: nothing is due by age, but only max_segments fit. The
    # oldest fold first.
    segs <- list(seg("a", 3, now), seg("b", 6, now), seg("c", 4, now),
                 seg("d", 5, now), seg("e", 1, now))
    expect_identical(fd(segs, fold_after_days = 7, max_segments = 3,
                        fold_min_days = 2), c("b", "d"))

    # The min-days guard holds even under pressure: five fresh segments
    # over a cap of two fold nothing.
    segs <- list(seg("a", 1, now), seg("b", 0.5, now), seg("c", 1.5, now),
                 seg("d", 0.2, now), seg("e", 1.2, now))
    expect_identical(fd(segs, fold_after_days = 7, max_segments = 2,
                        fold_min_days = 2), character())

    # Age folds free up room, so pressure only covers the remainder --
    # and the result comes back oldest first across both triggers.
    segs <- list(seg("a", 10, now), seg("b", 6, now), seg("c", 5, now),
                 seg("d", 3, now))
    expect_identical(fd(segs, fold_after_days = 7, max_segments = 2,
                        fold_min_days = 2), c("a", "b"))
})

# ---- One mocked sweep ----
# Three lifecycle rooms on the homeserver: seg1 is old (only bot
# messages, dated by its state event), seg2 has a fresh human message,
# seg3 cannot be dated at all. One sweep folds seg1 into a created
# Unsorted room, leaves seg2 alone, and refuses seg3.
local({
    now <- Sys.time()
    iso <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

    states <- list(
        `!seg1:ex` = list(state = "segment", segment_of = "!home:ex",
                          vault = "raw/matrix/seg1.md",
                          since = iso(now - 18 * 86400)),
        `!seg2:ex` = list(state = "segment", segment_of = "!home:ex",
                          since = iso(now - 18 * 86400)),
        `!seg3:ex` = list(state = "segment", segment_of = "!home:ex")
    )
    messages <- list(
        `!seg1:ex` = list(chunk = list(
            matrix_event("@bot:ex", as.numeric(now) * 1000, "bot notice",
                         msgtype = "m.notice")
        )),
        `!seg2:ex` = list(chunk = list(
            matrix_event("@owner:ex", as.numeric(now - 86400) * 1000, "hi")
        )),
        `!seg3:ex` = list(chunk = list())
    )
    names_by_id <- c(`!seg1:ex` = "Fix the fold formula (2026-08-01)",
                     `!seg2:ex` = "Fresh topic (2026-08-18)",
                     `!seg3:ex` = "Undated", `!home:ex` = "mybot")
    sent <- list()
    set_states <- list()

    cl <- matrix_client(
        .channels = function(sess) {
            c("!seg1:ex", "!seg2:ex", "!seg3:ex", "!home:ex")
        },
        .info = list(name = function(sess, rid) names_by_id[[rid]],
                     topic = function(sess, rid) NULL),
        .get_state = function(sess, rid, type, state_key = "") {
            if (identical(type, "ai.cornball.lifecycle")) {
                return(states[[rid]])
            }
            NULL
        },
        .history = function(sess, rid, dir = "b", limit = 50L, from = NULL) {
            messages[[rid]] %||% list(chunk = list())
        },
        .send = function(mx, text, room = NULL, msgtype = "m.text",
                         markdown = FALSE, ...) {
            sent[[length(sent) + 1L]] <<- list(room = room, body = text,
                                               msgtype = msgtype)
            "$root1"
        },
        .state = function(sess, rid, type, content, state_key = "") {
            set_states[[length(set_states) + 1L]] <<- list(
                room = rid, type = type, content = content,
                state_key = state_key)
            "$st"
        },
        .create = function(sess, name, ...) "!topic:ex")

    # Classification is test_topics.R's subject; here it fails closed,
    # which is the Unsorted path this test pins.
    orig_cls <- cerebro:::.classify_segment
    assignInNamespace(".classify_segment", function(...) NULL,
                      ns = "cerebro")
    on.exit(assignInNamespace(".classify_segment", orig_cls,
                              ns = "cerebro"), add = TRUE)

    res <- cerebro::sweep_segments(fold_after_days = 7, max_segments = 12,
                                   fold_min_days = 2, model = "m",
                                   client = cl)

    # seg1 folded, seg2 untouched, seg3 refused rather than guessed.
    expect_identical(vapply(res$folded, function(f) f$id, character(1)),
                     "!seg1:ex")
    expect_identical(res$skipped, "!seg3:ex")

    # The thread root went to the created topic room as a notice,
    # carrying the segment's name and vault pointer.
    root <- sent[[1L]]
    expect_identical(root$room, "!topic:ex")
    expect_identical(root$msgtype, "m.notice")
    expect_true(grepl("Fix the fold formula", root$body, fixed = TRUE))
    expect_true(grepl("raw/matrix/seg1.md", root$body, fixed = TRUE))
    # The root names its segment: retry dedup and the thread -> segment
    # mapping both hang off this line.
    expect_true(grepl("\nSegment: !seg1:ex", root$body, fixed = TRUE))

    types <- vapply(set_states, function(s) paste(s$room, s$type),
                    character(1))
    # The topic room is stamped once as a topic.
    i <- match("!topic:ex ai.cornball.lifecycle", types)
    expect_identical(set_states[[i]]$content$state, "topic")
    # The segment's lifecycle event flips to folded and points at the
    # thread root, keeping its provenance fields.
    i <- match("!seg1:ex ai.cornball.lifecycle", types)
    st <- set_states[[i]]$content
    expect_identical(st$state, "folded")
    expect_identical(st$folded_into, list(room = "!topic:ex",
                                          thread = "$root1"))
    expect_identical(st$segment_of, "!home:ex")
    expect_identical(st$vault, "raw/matrix/seg1.md")
    # since stays the segment's own timestamp; the fold gets its own.
    expect_identical(st$since, states[["!seg1:ex"]]$since)
    expect_true(is.character(st$folded_at) && nzchar(st$folded_at))
    # And the room is tombstoned toward the topic room.
    i <- match("!seg1:ex m.room.tombstone", types)
    expect_identical(set_states[[i]]$content$replacement_room, "!topic:ex")

    # The fold index lands in the TOPIC room, keyed by the thread root:
    # a reply in that thread is answered with the conversation it
    # continues, and finding it must not mean scanning the topic room's
    # history for a root that sinks with every later fold.
    idx <- Filter(function(s) identical(s$type, "ai.cornball.fold"),
                  set_states)
    expect_identical(length(idx), 1L)
    expect_identical(idx[[1L]]$room, "!topic:ex")
    expect_identical(idx[[1L]]$state_key, "$root1")
    expect_identical(idx[[1L]]$content$segment, "!seg1:ex")
    expect_identical(idx[[1L]]$content$vault, "raw/matrix/seg1.md")
    # The lifecycle events themselves stay on the empty state key, so
    # the index cannot shadow them.
    life <- Filter(function(s) identical(s$type, "ai.cornball.lifecycle"),
                   set_states)
    expect_true(all(vapply(life, function(s) identical(s$state_key, ""),
                           logical(1))))

    # No writes touched seg2.
    expect_false(any(grepl("!seg2:ex", types, fixed = TRUE)))
})

# ---- Activity lookup pages past bot traffic ----
# A window of recent messages sitting entirely on bot traffic must not
# date the room; the walk pages back until it finds the human.
local({
    bot <- function(ts) matrix_event("@bot:ex", ts, "b", msgtype = "m.notice")
    human <- function(ts) matrix_event("@owner:ex", ts, "h")
    pages <- list(
        `start` = list(chunk = list(bot(9000), bot(8000)), end = "p2"),
        `p2` = list(chunk = list(bot(7000), human(5000)), end = "p3")
    )
    cl <- matrix_client(
        .history = function(sess, rid, dir = "b", limit = 50L, from = NULL) {
            pages[[from %||% "start"]]
        })

    ts <- cerebro:::.last_human_ts(cl, "!r:ex")
    expect_identical(as.numeric(ts) * 1000, 5000)

    # Cap hit with history remaining: the oldest scanned message is an
    # upper bound on human activity, so the room looks fresher, never
    # staler -- a premature fold is the failure this rules out.
    pages <- list(
        `start` = list(chunk = list(bot(9000), bot(8000)), end = "p2"),
        `p2` = list(chunk = list(bot(7000), bot(6000)), end = "p3")
    )
    ts <- cerebro:::.last_human_ts(cl, "!r:ex",
                                   fallback = "2020-01-01T00:00:00Z",
                                   max_pages = 2L)
    expect_identical(as.numeric(ts) * 1000, 6000)

    # Beginning reached with no human message: the state event's since
    # dates the room instead.
    pages <- list(`start` = list(chunk = list(bot(9000))))
    ts <- cerebro:::.last_human_ts(cl, "!r:ex",
                                   fallback = "2026-08-01T00:00:00Z")
    expect_identical(format(ts, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                     "2026-08-01T00:00:00Z")
})

# ---- A retried fold reuses its root ----
# The state flip is the commit point, so a fold that failed after
# posting its root reruns whole; the rerun must find the earlier root
# instead of posting a duplicate.
local({
    writes <- list()
    cl <- matrix_client(
        .history = function(sess, rid, dir = "b", limit = 50L, from = NULL) {
            list(chunk = list(matrix_event(
                "@bot:ex", 1000, "old (2026-08-01)\nSegment: !seg1:ex",
                event_id = "$earlier", msgtype = "m.notice")))
        },
        .send = function(...) {
            stop("a retried fold must not post a second root")
        },
        .state = function(sess, rid, type, content, state_key = "") {
            writes[[length(writes) + 1L]] <<- list(type = type,
                                                   content = content,
                                                   state_key = state_key)
            "$st"
        })

    seg <- list(id = "!seg1:ex",
                state = list(state = "segment", segment_of = "!home:ex",
                             since = "2026-08-01T00:00:00Z"))
    root <- cerebro:::.fold_one(cl, seg, "!topic:ex", "Unsorted")
    expect_identical(root, "$earlier")
    st <- writes[[match("ai.cornball.lifecycle",
                        vapply(writes, `[[`, character(1), "type"))]]
    expect_identical(st$content$folded_into$thread, "$earlier")
    # The index is rewritten on the retry too -- a state PUT, so a
    # rerun costs nothing -- and keyed to the root it adopted.
    idx <- writes[[match("ai.cornball.fold",
                         vapply(writes, `[[`, character(1), "type"))]]
    expect_identical(idx$state_key, "$earlier")
    expect_identical(idx$content$segment, "!seg1:ex")
    # This segment archived nothing, so the index carries no pointer
    # rather than one naming an archive that was never written.
    expect_false("vault" %in% names(idx$content))
})

# ---- Dry run ----
# The plan is reported and nothing is written.
local({
    now <- Sys.time()
    cl <- matrix_client(
        .channels = function(sess) "!seg1:ex",
        .info = list(name = function(sess, rid) "old",
                     topic = function(sess, rid) NULL),
        .get_state = function(sess, rid, type, state_key = "") {
            list(state = "segment", segment_of = "!home:ex",
                 since = format(now - 30 * 86400, "%Y-%m-%dT%H:%M:%SZ",
                                tz = "UTC"))
        },
        .history = function(sess, rid, dir = "b", limit = 50L, from = NULL) {
            list(chunk = list())
        },
        .send = function(...) stop("must not send"),
        .state = function(...) stop("must not write"))
    orig_cls <- cerebro:::.classify_segment
    assignInNamespace(".classify_segment", function(...) NULL,
                      ns = "cerebro")
    on.exit(assignInNamespace(".classify_segment", orig_cls,
                              ns = "cerebro"), add = TRUE)

    expect_message(
        res <- cerebro::sweep_segments(model = "m", client = cl,
                                       dry_run = TRUE),
        "would fold")
    expect_identical(res$folded[[1L]]$id, "!seg1:ex")
})
