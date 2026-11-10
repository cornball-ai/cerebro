library(tinytest)

if (!requireNamespace("mx.client", quietly = TRUE)) {
    exit_file("mx.client not installed")
}
source("helper_matrix.R")

root <- file.path(tempdir(), paste0("cerebro-transport-", Sys.getpid(), "-",
                                    sample.int(1e6, 1L)))
dir.create(root, recursive = TRUE)

legacy <- paste("opsbot","pensar", sep = " · ")

# In-memory Matrix server double: rooms maps rid -> display name.
fake <- new.env(parent = emptyenv())
fake$rooms <- list("!old:test" = legacy)
fake$renames <- list()
fake$topics <- list()
fake$created <- list()

cl <- matrix_client(
    user_id = "@bot:example.invalid",
    .channels = function(sess) names(fake$rooms),
    .info = list(name = function(sess, rid) fake$rooms[[rid]],
                 topic = function(sess, rid) fake$topics[[rid]]),
    .state = function(sess, rid, type, content, state_key = "") {
        if (identical(type, "m.room.name")) {
            fake$rooms[[rid]] <- content$name
            fake$renames[[length(fake$renames) + 1L]] <-
            list(room_id = rid, name = content$name)
        }
        if (identical(type, "m.room.topic")) {
            fake$topics[[rid]] <- content$topic
        }
        "$event:test"
    },
    .create = function(sess, name = NULL, topic = NULL, ...) {
        rid <- paste0("!created", length(fake$created) + 1L, ":test")
        fake$created[[length(fake$created) + 1L]] <- name
        fake$rooms[[rid]] <- name
        rid
    })

# Pre-seed the instance room cache under the legacy key so the rename
# re-keys it.
write_state <- getFromNamespace("write_instance_state", "cerebro")
read_state <- getFromNamespace("read_instance_state", "cerebro")
state <- read_state(root)
state$matrix_rooms <- stats::setNames(list("!old:test"), legacy)
write_state(state, root)

# Dry run reports the rename without touching the server.
expect_message(
               cerebro::ensure_matrix_room("~/pensar", client = cl,
        dry_run = TRUE, legacy_name = legacy),
               pattern = "would rename Matrix room"
)
expect_equal(length(fake$renames), 0L)

# Live path: legacy hit is renamed in place, cache is re-keyed.
rid <- suppressMessages(
    cerebro::ensure_matrix_room("~/pensar", topic = "~/pensar | thread",
        client = cl, instance_dir = root,
        legacy_name = legacy)
)
expect_equal(rid, "!old:test")
expect_equal(length(fake$renames), 1L)
expect_equal(fake$rooms[["!old:test"]], "~/pensar")
expect_equal(fake$topics[["!old:test"]], "~/pensar | thread")
expect_equal(length(fake$created), 0L)
rooms_cache <- read_state(root)$matrix_rooms
expect_equal(rooms_cache[["~/pensar"]], "!old:test")
expect_false(legacy %in% names(rooms_cache))

# Idempotent reuse: the new name now matches directly, no second rename,
# no duplicate room, no duplicate cache key.
rid_again <- suppressMessages(
    cerebro::ensure_matrix_room("~/pensar", topic = "~/pensar | thread",
        client = cl, instance_dir = root,
        legacy_name = legacy)
)
expect_equal(rid_again, "!old:test")
expect_equal(length(fake$renames), 1L)
expect_equal(length(fake$created), 0L)
expect_equal(sum(names(read_state(root)$matrix_rooms) == "~/pensar"), 1L)

# Display-name lookup normalizes insignificant whitespace. Matrix rooms
# can carry a trailing space from client-side edits; that must not create
# a duplicate room when the requested name is clean.
fake$rooms[["!trail:test"]] <- "Cooking "
rid_trailing <- suppressMessages(
    cerebro::ensure_matrix_room("Cooking", client = cl, instance_dir = root)
)
expect_equal(rid_trailing, "!trail:test")
expect_equal(length(fake$created), 0L)
expect_equal(read_state(root)$matrix_rooms[["Cooking"]], "!trail:test")
expect_equal(cerebro::resolve_room("Cooking ", instance_dir = root,
                                  client = cl), "!trail:test")

# The cache is authoritative once populated; otherwise an existing duplicate
# with the same display name can win future exact-name lookups.
state <- read_state(root)
state$matrix_rooms$Cached <- "!cached:test"
write_state(state, root)
rid_cached <- suppressMessages(
    cerebro::ensure_matrix_room("Cached", client = cl, instance_dir = root)
)
expect_equal(rid_cached, "!cached:test")
expect_equal(length(fake$created), 0L)

# Create path: no match under either name creates the room with the new
# title.
rid_new <- suppressMessages(
    cerebro::ensure_matrix_room("~/newrepo", client = cl, instance_dir = root,
        legacy_name = paste("opsbot","newrepo", sep = " · "))
)
expect_equal(length(fake$created), 1L)
expect_equal(fake$created[[1L]], "~/newrepo")
expect_equal(read_state(root)$matrix_rooms[["~/newrepo"]], rid_new)

# A literal id needs no transport and no config at all.
expect_equal(cerebro::resolve_room("!literal:test"), "!literal:test")

unlink(root, recursive = TRUE)
