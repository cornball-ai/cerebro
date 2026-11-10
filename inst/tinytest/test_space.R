library(tinytest)

if (!requireNamespace("mx.client", quietly = TRUE)) {
    exit_file("mx.client not installed")
}
source("helper_matrix.R")

# Spaces, and what filing a room under one actually writes.
#
# The point of the space is that ONE invite replaces one invite per room:
# eleven topic rooms in a morning was eleven invites. So the assertions
# that matter are about what is NOT sent -- no invite on a filed room --
# and about the third state event, the one that makes the space more than
# a sidebar decoration.

root <- file.path(tempdir(), paste0("cerebro-space-", Sys.getpid(), "-",
                                    sample.int(1e6, 1L)))
dir.create(root, recursive = TRUE)

fake <- new.env(parent = emptyenv())
fake$rooms <- list()
fake$created <- list()
fake$state <- list()
fake$invited <- list()
fake$fail_join_rules <- FALSE

cl <- matrix_client(
    user_id = "@bot:example.invalid",
    .channels = function(sess) names(fake$rooms),
    .info = list(name = function(sess, rid) fake$rooms[[rid]],
                 topic = function(sess, rid) NULL),
    .invite = function(sess, rid, user_id) {
        fake$invited[[length(fake$invited) + 1L]] <-
        list(room = rid, user = user_id)
        "$ok:test"
    },
    .state = function(sess, rid, type, content, state_key = "") {
        if (identical(type, "m.room.join_rules") &&
            isTRUE(fake$fail_join_rules)) {
            stop("unsupported room version", call. = FALSE)
        }
        fake$state[[length(fake$state) + 1L]] <-
        list(room = rid, type = type, key = state_key, content = content)
        "$event:test"
    },
    .create = function(sess, name = NULL, topic = NULL,
                       visibility = "private", preset = NULL,
                       invite = character(), creation_content = NULL) {
        # NO ":server" SUFFIX. From room version 12 a room id is opaque
        # and colon-free, and this fixture used to carry one -- which is
        # the only reason the via assertion below passed while
        # production filed 54 rooms nothing would render.
        rid <- paste0("!made", length(fake$created) + 1L,
                      "_AbCdEfGhIjKlMnOpQrSt")
        fake$created[[length(fake$created) + 1L]] <-
        list(name = name, invite = invite,
             creation_content = creation_content)
        fake$rooms[[rid]] <- name
        rid
    })

read_state <- getFromNamespace("read_instance_state", "cerebro")
via <- getFromNamespace(".mx_via", "cerebro")

# ---- the via server ----------------------------------------------------
# Taken from the BOT'S user id, because a room id no longer carries a
# server. From room version 12 they are opaque and colon-free, so
# splitting one on ":" hands back the whole id -- and a child event whose
# via is a room id is stored happily and rendered by nothing. That is a
# space that fills up and reads empty, which is exactly what happened to
# 54 rooms before this was found.
me <- "@bot:example.invalid"
expect_equal(via(me, "!opaqueRoomIdNoColon"), "example.invalid")
# A room id that DOES carry a server contributes it, and the bot's own
# server comes second -- both are true answers.
expect_equal(via(me, "!abc:other.example"), c("other.example",
                                              "example.invalid"))
# No duplicate when they agree.
expect_equal(via(me, "!abc:example.invalid"), "example.invalid")
# Nothing to go on is empty, and .file_in_space refuses rather than
# writing an unrenderable child.
expect_equal(length(via("nonsense", "!x")), 0L)
expect_equal(length(via("", "!x")), 0L)

# ---- a space is created as a space -------------------------------------
sid <- suppressMessages(
    cerebro::ensure_matrix_space("mybot", invite = "@owner:example.invalid",
                                 client = cl, instance_dir = root)
)
expect_equal(length(fake$created), 1L)
# THE FIELD THAT MAKES IT A SPACE. Without it the server returns a
# perfectly ordinary room and everything downstream still "works" --
# rooms get filed into it, nothing errors, and it renders as a chat with
# strange events in it.
expect_equal(fake$created[[1L]]$creation_content, list(type = "m.space"))
# The one invite. This is the entire feature.
expect_equal(fake$created[[1L]]$invite, "@owner:example.invalid")

# Cached apart from matrix_rooms: a space and a room may share a display
# name, and one cache hands back whichever was written last.
expect_equal(read_state(root)$matrix_spaces[["mybot"]], sid)
expect_null(read_state(root)$matrix_rooms[["mybot"]])

# Reused, not recreated.
sid2 <- suppressMessages(
    cerebro::ensure_matrix_space("mybot", client = cl, instance_dir = root)
)
expect_equal(sid2, sid)
expect_equal(length(fake$created), 1L)

# FOUND ON THE SERVER, NOT ONLY IN THE CACHE. Personas share one space:
# whichever creates it caches the id, and every other one starts with an
# empty cache. A cache-only lookup has each of them create its own space
# of the same name -- which looks exactly like it worked, until two
# sidebars disagree about which "dev" is the real one. Asserted from a
# different instance_dir, which is what a second persona actually is.
local({
    other <- file.path(root, "second-persona")
    dir.create(other, recursive = TRUE, showWarnings = FALSE)
    again <- suppressMessages(
        cerebro::ensure_matrix_space("mybot", client = cl, instance_dir = other)
    )
    expect_equal(again, sid)
    expect_equal(length(fake$created), 1L)
    # And it caches what it found, so the second persona stops looking.
    expect_equal(read_state(other)$matrix_spaces[["mybot"]], sid)
})

# ---- a room filed under a space is NOT invited to ----------------------
fake$state <- list()
rid <- suppressMessages(
    cerebro::ensure_matrix_room("Travel", client = cl, instance_dir = root,
                                invite = "@owner:example.invalid",
                                space = sid)
)
# The invite was dropped on the floor deliberately. Passing both would
# put back exactly the notification the space exists to remove, and it
# would look like it was working.
expect_equal(length(fake$created[[2L]]$invite), 0L)
expect_equal(length(fake$invited), 0L)

types <- vapply(fake$state, function(s) s$type, character(1L))
expect_true("m.space.child" %in% types)
expect_true("m.space.parent" %in% types)

child <- fake$state[[match("m.space.child", types)]]
expect_equal(child$room, sid)          # child event lives on the SPACE
expect_equal(child$key, rid)           # keyed by the room it points at
# A RESOLVABLE SERVER, and asserted as one rather than as "not empty".
# The via that broke this was non-empty: it was the room id. Every
# structural check passed on it -- the event existed, it had a via, the
# via was a one-element list -- and none of those is the property that
# matters, which is that a client can route to it.
expect_equal(child$content$via, list("example.invalid"))
expect_false(grepl("^!", child$content$via[[1L]]))

parent <- fake$state[[match("m.space.parent", types)]]
expect_equal(parent$room, rid)         # parent event lives on the ROOM
expect_equal(parent$key, sid)
expect_true(isTRUE(parent$content$canonical))

# THE ONE THAT IS EASY TO OMIT. child + parent put the room in the
# sidebar; only restricted join rules let a space member actually open
# it. Without this the room is listed and refuses the click, which reads
# as a client bug rather than a missing state event.
expect_true("m.room.join_rules" %in% types)
rules <- fake$state[[match("m.room.join_rules", types)]]
expect_equal(rules$room, rid)
expect_equal(rules$content$join_rule, "restricted")
expect_equal(rules$content$allow[[1L]]$room_id, sid)
expect_equal(rules$content$allow[[1L]]$type, "m.room_membership")

# ---- join rules fail: fall back to inviting ----------------------------
# Restricted join rules need room version 9+. On an older server the
# filing fails and a room nobody can enter is worse than the invite this
# was meant to avoid, so the invite comes back, through chat.api's own
# verb for it.
fake$fail_join_rules <- TRUE
fake$state <- list()
fake$invited <- list()
suppressMessages(
    cerebro::ensure_matrix_room("Statistics", client = cl,
                                instance_dir = root,
                                invite = "@owner:example.invalid",
                                space = sid)
)
expect_equal(length(fake$invited), 1L)
expect_equal(fake$invited[[1L]]$user, "@owner:example.invalid")
fake$fail_join_rules <- FALSE

# ---- no space: the old behaviour, unchanged ----------------------------
# Every existing caller passes no space and must still get its invite at
# creation. A space that silently swallowed invites for everyone would be
# a much quieter bug than eleven of them.
before <- length(fake$created)
suppressMessages(
    cerebro::ensure_matrix_room("Cooking", client = cl, instance_dir = root,
                                invite = "@owner:example.invalid")
)
expect_equal(fake$created[[before + 1L]]$invite, "@owner:example.invalid")
expect_null(fake$created[[before + 1L]]$creation_content)

# ---- dry run writes nothing -------------------------------------------
n <- length(fake$created)
expect_message(
    cerebro::ensure_matrix_space("scratch", client = cl, dry_run = TRUE),
    pattern = "would create Matrix space"
)
expect_equal(length(fake$created), n)

unlink(root, recursive = TRUE)
