library(tinytest)

# The open half of the lifecycle: the segment room a cleared conversation
# becomes, and the archive a folded thread stands for. Both are the hooks
# run() hands corteza. The loopback adapter is the transport double: it
# supports channel_create and set_state, which is exactly the pair the
# segment is gated on.

if (!requireNamespace("chat.api", quietly = TRUE)) {
    exit_file("chat.api not installed")
}

cfg <- list(segment_rooms = "home")

# ---- The segment room ----
local({
    lo <- chat.api::chat_loopback()
    seg <- cerebro::segment_open(lo, cfg, "home", "Fold formula (2026-08-19)",
                                 "raw/matrix/seg.md")
    expect_identical(seg$id, "Fold formula (2026-08-19)")
    expect_identical(seg$name, "Fold formula (2026-08-19)")
    # The summary went to the new room as a notice carrying the pointer.
    msg <- chat.api::chat_poll(lo)$messages[[1L]]
    expect_identical(msg$channel, seg$id)
    expect_identical(msg$kind, "notice")
    expect_true(grepl("raw/matrix/seg.md", msg$body, fixed = TRUE))
    # And the lifecycle state event marks it a segment of the home room:
    # the shape sweep_segments() folds by.
    st <- chat.api::chat_get_state(lo, seg$id, "ai.cornball.lifecycle")
    expect_identical(st$state, "segment")
    expect_identical(st$segment_of, "home")
    expect_identical(st$vault, "raw/matrix/seg.md")
    expect_true(is.character(st$since) && nzchar(st$since))
    expect_true(seg$id %in% chat.api::chat_channels(lo))
})

# No archive pointer: the state event and summary simply omit it.
local({
    lo <- chat.api::chat_loopback()
    seg <- cerebro::segment_open(lo, cfg, "home", "Untitled (x)", NULL)
    st <- chat.api::chat_get_state(lo, seg$id, "ai.cornball.lifecycle")
    expect_false("vault" %in% names(st))
    expect_false(grepl("Archived transcript",
                       chat.api::chat_poll(lo)$messages[[1L]]$body,
                       fixed = TRUE))
})

# ---- The policy gate ----
# Only rooms the bot config lists segment. A room not listed, or no
# list at all, gets nothing and creates nothing: the clear is simply
# acknowledged.
local({
    lo <- chat.api::chat_loopback()
    before <- chat.api::chat_channels(lo)
    expect_null(cerebro::segment_open(lo, cfg, "other", "T (d)", NULL))
    expect_null(cerebro::segment_open(lo, list(), "home", "T (d)", NULL))
    expect_null(cerebro::segment_open(lo, list(segment_rooms = NULL), "home",
                                      "T (d)", NULL))
    expect_identical(chat.api::chat_channels(lo), before)
    expect_identical(length(chat.api::chat_poll(lo)$messages), 0L)
})

# A transport that cannot write state skips with a message rather than
# erroring: archive-and-reset is corteza's critical path, not the segment.
local({
    irc <- structure(list(env = new.env(parent = emptyenv()), nick = "bot"),
                     class = c("chat_irc", "chat_client"))
    expect_false(isTRUE(chat.api::chat_capabilities(irc)$set_state))
    expect_message(res <- cerebro::segment_open(irc, list(segment_rooms = "#lab"),
                                                "#lab", "t", NULL),
                   "no segment")
    expect_null(res)
})

# ---- Finding the archive a thread stands for ----
local({
    lo <- chat.api::chat_loopback()
    # A thread nobody folded into is not a fold, and answers NULL
    # rather than erroring.
    expect_null(cerebro::fold_lookup(lo, "topic", "$root"))
    expect_null(cerebro::fold_lookup(lo, "topic", NULL))
    expect_null(cerebro::fold_lookup(lo, "topic", ""))
    chat.api::chat_set_state(lo, "topic", "ai.cornball.fold",
                             list(segment = "!seg:ex",
                                  vault = "raw/matrix/seg.md"),
                             state_key = "$root")
    got <- cerebro::fold_lookup(lo, "topic", "$root")
    expect_identical(got, list(segment = "!seg:ex", vault = "raw/matrix/seg.md"))
    # Keyed by the root: a different thread in the same room is a
    # different fold, and an unindexed one is not a fold at all.
    expect_null(cerebro::fold_lookup(lo, "topic", "$other"))
    # An index without a usable pointer is no archive.
    chat.api::chat_set_state(lo, "topic", "ai.cornball.fold",
                             list(segment = "!seg:ex"), state_key = "$bare")
    expect_null(cerebro::fold_lookup(lo, "topic", "$bare"))
})

# A transport without durable state has no index to read: NULL, so a
# bot on such a transport keeps answering unseeded.
local({
    irc <- structure(list(env = new.env(parent = emptyenv()), nick = "bot"),
                     class = c("chat_irc", "chat_client"))
    expect_null(cerebro::fold_lookup(irc, "#lab", "$root"))
})

# ---- The fold index is what the sweep writes ----
# The lookup reads back exactly what .fold_one() recorded, which is the
# contract between the two halves: root event id as the key, the segment
# id and the archive pointer as the content.
if (requireNamespace("mx.client", quietly = TRUE)) local({
    source("helper_matrix.R")
    states <- list()
    sent <- list()
    cl <- matrix_client(
        .send = function(mx, text, room = NULL, msgtype = "m.text",
                         markdown = FALSE, ...) {
            sent[[length(sent) + 1L]] <<- list(room = room, body = text)
            "$root1"
        },
        .state = function(sess, rid, type, content, state_key = "") {
            states[[paste(rid, type, state_key, sep = "\r")]] <<- content
            "$st"
        },
        .get_state = function(sess, rid, type, state_key = "") {
            states[[paste(rid, type, state_key, sep = "\r")]]
        })
    seg <- list(id = "!seg:ex", name = "Fold formula (2026-08-19)",
                state = list(state = "segment", segment_of = "!home:ex",
                             vault = "raw/matrix/seg.md",
                             since = "2026-08-19T10:00:00Z"))
    root <- cerebro:::.fold_one(cl, seg, "!topic:ex", "Cooking")
    expect_identical(root, "$root1")
    expect_identical(cerebro::fold_lookup(cl, "!topic:ex", root),
                     list(segment = "!seg:ex", vault = "raw/matrix/seg.md"))
})
