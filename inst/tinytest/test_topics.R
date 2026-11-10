library(tinytest)

# Topic assignment for the fold sweep: the pure decision rule, the
# classifier's reply parsing, topic-room enumeration, and one sweep
# where segments fan out across an existing topic, a created one, and
# the Unsorted catch-all. Nothing on this path may ever ask a question:
# every failure mode must land somewhere silently.

if (!requireNamespace("mx.client", quietly = TRUE)) {
    exit_file("mx.client not installed")
}
source("helper_matrix.R")

# ---- The decision rule ----
local({
    topics <- c(Cooking = "!cook:ex", Reading = "!read:ex")
    dec <- function(cls) {
        cerebro:::.topic_decision(cls, topics, threshold = 0.7,
                                  unsorted = "Unsorted")
    }
    unsorted <- list(topic = "Unsorted", create = FALSE)

    # Every failure mode falls through to the catch-all, silently.
    expect_identical(dec(NULL), unsorted)
    expect_identical(dec(list(topic = "Cooking", new = FALSE,
                              confidence = 0.5)), unsorted)
    # NA refuses, never passes.
    expect_identical(dec(list(topic = "Cooking", new = FALSE,
                              confidence = NA_real_)), unsorted)
    expect_identical(dec(list(topic = "", new = FALSE,
                              confidence = 0.9)), unsorted)
    expect_identical(dec(list(topic = strrep("long ", 20L), new = TRUE,
                              confidence = 0.9)), unsorted)
    # A model electing the catch-all by name is the catch-all.
    expect_identical(dec(list(topic = "Unsorted", new = TRUE,
                              confidence = 0.9)), unsorted)

    # Confident match on an existing topic files there.
    expect_identical(dec(list(topic = "Cooking", new = FALSE,
                              confidence = 0.9)),
                     list(topic = "Cooking", create = FALSE))
    # Membership decides existing vs new, not the model's `new` flag:
    # a claimed-existing name that is not there is a proposal.
    expect_identical(dec(list(topic = "Gardening", new = FALSE,
                              confidence = 0.9)),
                     list(topic = "Gardening", create = TRUE))
    expect_identical(dec(list(topic = "Gardening", new = TRUE,
                              confidence = 0.9)),
                     list(topic = "Gardening", create = TRUE))
})

# ---- Reply parsing ----
local({
    if (!requireNamespace("llm.api", quietly = TRUE)) {
        exit_file("llm.api not installed")
    }
    orig <- llm.api::chat
    on.exit(assignInNamespace("chat", orig, ns = "llm.api"), add = TRUE)
    seen_prompt <- NULL
    reply <- NULL
    assignInNamespace("chat", function(prompt, ...) {
        seen_prompt <<- prompt
        reply
    }, ns = "llm.api")

    # JSON survives being wrapped in prose, and the prompt carries the
    # existing topics and the title the model needs.
    reply <- list(content = paste0("Sure, here you go: {\"topic\": ",
                                   "\"Cooking\", \"new\": false, ",
                                   "\"confidence\": 0.85} Hope that helps!"))
    cls <- cerebro:::.classify_segment("Sourdough starter (2026-08-01)",
                                       "excerpt text",
                                       c(Cooking = "!cook:ex"),
                                       model = "m", provider = "p")
    expect_identical(cls$topic, "Cooking")
    expect_false(cls$new)
    expect_identical(cls$confidence, 0.85)
    expect_true(grepl("- Cooking", seen_prompt, fixed = TRUE))
    expect_true(grepl("Sourdough starter", seen_prompt, fixed = TRUE))

    # No JSON in the reply is NULL, not an error.
    reply <- list(content = "I cannot decide.")
    expect_null(cerebro:::.classify_segment("t", "", character(),
                                            model = "m", provider = "p"))

    # Malformed replies -- content missing, empty, or not a string --
    # are garbage, and garbage falls through rather than erroring.
    for (bad in list(list(), list(content = character(0)),
                     list(content = NULL), list(content = list("x")),
                     list(content = NA_character_), character(0))) {
        reply <- bad
        expect_null(cerebro:::.classify_segment("t", "", character(),
                                                model = "m",
                                                provider = "p"))
    }

    # A provider error is a message and NULL, never a condition the
    # sweep would trip over.
    assignInNamespace("chat", function(...) stop("api down"),
                      ns = "llm.api")
    expect_message(
        res <- cerebro:::.classify_segment("t", "", character(),
                                           model = "m", provider = "p"),
        "classify error")
    expect_null(res)
})

# ---- Topic enumeration ----
# The stamp marks a room as a topic; the label is the room's CURRENT
# display name. Renaming the room is the veto, so a stamped Gardening
# renamed to Outdoor must enumerate as Outdoor -- the stale stamp name
# is only the fallback when the name cannot be read.
local({
    states <- list(
        `!cook:ex` = list(state = "topic", topic = "Cooking"),
        `!garden:ex` = list(state = "topic", topic = "Gardening"),
        `!noname:ex` = list(state = "topic", topic = "Stamped"),
        `!seg:ex` = list(state = "segment", segment_of = "!home:ex")
    )
    names_by_id <- c(`!cook:ex` = "Cooking", `!garden:ex` = "Outdoor",
                     `!seg:ex` = "old thread")
    cl <- matrix_client(
        .channels = function(sess) {
            c("!cook:ex", "!garden:ex", "!noname:ex", "!seg:ex", "!plain:ex")
        },
        .get_state = function(sess, rid, type, state_key = "") states[[rid]],
        .info = list(name = function(sess, rid) {
            nm <- names_by_id[rid]
            if (is.na(nm)) stop("M_NOT_FOUND") else nm[[1L]]
        }, topic = function(sess, rid) NULL))
    topics <- cerebro:::.topic_rooms(cl)
    expect_identical(topics[["Cooking"]], "!cook:ex")
    # The rename won: the classifier sees Outdoor, not Gardening.
    expect_identical(topics[["Outdoor"]], "!garden:ex")
    expect_false("Gardening" %in% names(topics))
    # Unreadable name falls back to the stamp, keeping the room
    # enumerable.
    expect_identical(topics[["Stamped"]], "!noname:ex")
})

# ---- One sweep fans out across topics ----
# Three old segments: one classifies into the existing Cooking room,
# one proposes Gardening confidently (created and announced), one is
# low-confidence and files into Unsorted silently.
local({
    now <- Sys.time()
    iso <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    states <- list(
        `!segA:ex` = list(state = "segment", segment_of = "!home:ex",
                          since = iso(now - 30 * 86400)),
        `!segB:ex` = list(state = "segment", segment_of = "!home:ex",
                          since = iso(now - 30 * 86400)),
        `!segC:ex` = list(state = "segment", segment_of = "!home:ex",
                          since = iso(now - 30 * 86400)),
        `!cook:ex` = list(state = "topic", topic = "Cooking")
    )
    names_by_id <- c(`!segA:ex` = "Trip plans (2026-07-01)",
                     `!segB:ex` = "Garden beds (2026-07-02)",
                     `!segC:ex` = "Misc chat (2026-07-03)",
                     `!cook:ex` = "Cooking")
    sent <- list()
    set_states <- list()

    cl <- matrix_client(
        .channels = function(sess) names(states),
        .info = list(name = function(sess, rid) names_by_id[[rid]],
                     topic = function(sess, rid) NULL),
        .get_state = function(sess, rid, type, state_key = "") {
            if (identical(type, "ai.cornball.lifecycle")) {
                return(states[[rid]])
            }
            NULL
        },
        .history = function(sess, rid, dir = "b", limit = 50L, from = NULL) {
            list(chunk = list())
        },
        .send = function(mx, text, room = NULL, msgtype = "m.text",
                         markdown = FALSE, ...) {
            sent[[length(sent) + 1L]] <<- list(room = room, body = text)
            paste0("$root", length(sent))
        },
        .state = function(sess, rid, type, content, state_key = "") {
            set_states[[length(set_states) + 1L]] <<- list(
                room = rid, type = type, content = content)
            "$st"
        },
        .create = function(sess, name, ...) paste0("!", tolower(name), ":ex"))

    orig_cls <- cerebro:::.classify_segment
    on.exit(assignInNamespace(".classify_segment", orig_cls,
                              ns = "cerebro"), add = TRUE)
    assignInNamespace(".classify_segment",
                      function(title, excerpt, topics, model, provider) {
        if (grepl("Trip plans", title, fixed = TRUE)) {
            return(list(topic = "Cooking", new = FALSE, confidence = 0.9))
        }
        if (grepl("Garden beds", title, fixed = TRUE)) {
            return(list(topic = "Gardening", new = TRUE,
                        confidence = 0.95))
        }
        list(topic = "Whatever", new = FALSE, confidence = 0.2)
    }, ns = "cerebro")

    # room = NULL routes the new-topic announcement to message().
    expect_message(
        res <- cerebro::sweep_segments(model = "m", client = cl),
        "New topic room: Gardening")

    filed <- vapply(res$folded, function(f) f$topic, character(1))
    names(filed) <- vapply(res$folded, function(f) f$id, character(1))
    expect_identical(filed[["!segA:ex"]], "Cooking")
    expect_identical(filed[["!segB:ex"]], "Gardening")
    expect_identical(filed[["!segC:ex"]], "Unsorted")

    # Roots reached the right rooms: the existing Cooking room by its
    # known id, the created ones by theirs.
    roots <- vapply(sent, function(s) s$room, character(1))
    expect_true("!cook:ex" %in% roots)
    expect_true("!gardening:ex" %in% roots)
    expect_true("!unsorted:ex" %in% roots)

    # Created topic rooms are stamped; the existing one is not
    # re-stamped.
    stamps <- vapply(set_states, function(s) {
        if (identical(s$type, "ai.cornball.lifecycle") &&
            identical(s$content$state, "topic")) s$room else ""
    }, character(1))
    expect_true("!gardening:ex" %in% stamps)
    expect_true("!unsorted:ex" %in% stamps)
    expect_false("!cook:ex" %in% stamps)
})

# ---- A topical thread is a fold destination ----
# The general thread sync used to create its per-page rooms without the
# lifecycle stamp, so .topic_rooms() could not see them: the classifier
# chose from a near-empty menu and opened a second room for a subject
# that already had a thread. .stamp_topic_room closes that gap, and
# labels the room with its CURRENT display name so the stamp agrees
# with what enumeration reports.
local({
    states <- list(`!books:ex` = list(state = "topic", topic = "Books"))
    names_by_id <- c(`!books:ex` = "Books", `!fin:ex` = "Finance",
                     `!noname:ex` = "")
    sets <- list()
    refuse <- FALSE
    cl <- matrix_client(
        .channels = function(sess) c("!books:ex", "!fin:ex"),
        .get_state = function(sess, rid, type, state_key = "") states[[rid]],
        .info = list(name = function(sess, rid) {
            nm <- names_by_id[rid]
            if (is.na(nm) || !nzchar(nm)) stop("M_NOT_FOUND") else nm[[1L]]
        }, topic = function(sess, rid) NULL),
        .state = function(sess, rid, type, content, state_key = "") {
            if (refuse) {
                stop("M_FORBIDDEN")
            }
            sets[[length(sets) + 1L]] <<- list(room = rid, type = type,
                                               content = content)
            states[[rid]] <<- content
            "$st"
        })

    # Before: the unstamped Finance thread room is invisible.
    expect_identical(names(cerebro:::.topic_rooms(cl)), "Books")

    expect_true(cerebro:::.stamp_topic_room(cl, "!fin:ex", "Finance"))
    expect_identical(length(sets), 1L)
    expect_identical(sets[[1L]]$room, "!fin:ex")
    expect_identical(sets[[1L]]$type, "ai.cornball.lifecycle")
    expect_identical(sets[[1L]]$content$state, "topic")
    # The display name, not the caller's label, when both are readable.
    expect_identical(sets[[1L]]$content$topic, "Finance")

    # After: the same sweep's classifier now has two candidates.
    expect_true(all(c("Books", "Finance") %in%
                    names(cerebro:::.topic_rooms(cl))))

    # Idempotent: a nightly sweep re-touching the room writes nothing.
    expect_false(cerebro:::.stamp_topic_room(cl, "!fin:ex", "Finance"))
    expect_identical(length(sets), 1L)

    # An unreadable name falls back to the caller's label.
    expect_true(cerebro:::.stamp_topic_room(cl, "!noname:ex", "Travel"))
    expect_identical(sets[[2L]]$content$topic, "Travel")

    # Nothing to stamp, nothing written.
    expect_false(cerebro:::.stamp_topic_room(NULL, "!fin:ex", "Finance"))
    expect_false(cerebro:::.stamp_topic_room(cl, "", "Finance"))
    expect_false(cerebro:::.stamp_topic_room(cl, "!noname:ex", ""))
    expect_identical(length(sets), 2L)

    # A Matrix failure is reported and swallowed: the thread is already
    # filed, and the next sweep retries the stamp.
    refuse <- TRUE
    expect_message(
        res <- cerebro:::.stamp_topic_room(cl, "!other:ex", "Cooking"),
        "could not stamp")
    expect_false(res)
})

# ---- An explicit non-topic state vetoes the stamp ----
# Renaming a room corrects a topic's label; nothing could say "never a
# fold destination". A curated profile room is the case: a fact sheet,
# not a conversation bucket, and the most attractive label on the menu
# for any personal conversation. Marking it `excluded` must survive the
# next sweep that touches its page.
local({
    states <- list(
        `!profile:ex` = list(state = "excluded", topic = "Owner profile"),
        `!seg:ex` = list(state = "segment", segment_of = "!home:ex"),
        `!blank:ex` = list(topic = "Stale"),
        `!empty:ex` = list(state = "", topic = "Stale")
    )
    sets <- list()
    cl <- matrix_client(
        .channels = function(sess) names(states),
        .get_state = function(sess, rid, type, state_key = "") states[[rid]],
        .info = list(name = function(sess, rid) rid,
                     topic = function(sess, rid) NULL),
        .state = function(sess, rid, type, content, state_key = "") {
            sets[[length(sets) + 1L]] <<- list(room = rid, content = content)
            states[[rid]] <<- content
            "$st"
        })

    # Vetoed: excluded stays excluded, a segment room stays a segment.
    expect_false(cerebro:::.stamp_topic_room(cl, "!profile:ex", "Owner profile"))
    expect_false(cerebro:::.stamp_topic_room(cl, "!seg:ex", "Old thread"))
    expect_identical(length(sets), 0L)
    expect_identical(states[["!profile:ex"]]$state, "excluded")

    # An excluded room is not a candidate.
    expect_false("Owner profile" %in% names(cerebro:::.topic_rooms(cl)))
    expect_false("!profile:ex" %in% unname(cerebro:::.topic_rooms(cl)))

    # A missing or empty state is not a veto: those rooms still stamp.
    expect_true(cerebro:::.stamp_topic_room(cl, "!blank:ex", "Blank"))
    expect_true(cerebro:::.stamp_topic_room(cl, "!empty:ex", "Empty"))
    expect_identical(length(sets), 2L)
    expect_identical(sets[[1L]]$content$state, "topic")
})
