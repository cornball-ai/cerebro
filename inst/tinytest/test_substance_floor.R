library(tinytest)

substance <- cerebro:::.thread_session_substance

# A one-line ping is one user turn, few characters. Assistant text does
# not count toward either measure.
ping <- list(turns = list(
    list(role = "user", text = "Can you see the image?"),
    list(role = "assistant", text = "I can. It renders fine.")
))
got <- substance(ping)
expect_equal(got$turns, 1L)
expect_equal(got$chars, nchar("Can you see the image?"))

# Assistant verbosity cannot buy a room.
verbose <- list(turns = list(
    list(role = "user", text = "hi"),
    list(role = "assistant", text = strrep("a very long reply. ", 100L))
))
expect_equal(substance(verbose)$turns, 1L)
expect_equal(substance(verbose)$chars, 2L)

# Several short user turns accumulate both measures.
chatty <- list(turns = list(
    list(role = "user", text = "one"),
    list(role = "assistant", text = "yes"),
    list(role = "user", text = "two"),
    list(role = "user", text = "three")
))
expect_equal(substance(chatty)$turns, 3L)
expect_equal(substance(chatty)$chars, 11L)

# Absent pieces degrade to zero rather than erroring.
expect_equal(substance(list())$turns, 0L)
expect_equal(substance(list())$chars, 0L)
expect_equal(substance(list(turns = list()))$turns, 0L)
expect_equal(substance(list(turns = list(list(role = "user"))))$chars, 0L)
expect_equal(substance(list(turns = list(list(text = "x"))))$turns, 0L)

# End to end: a confident, clearly-themed ping is not a room. The stub
# router proposes a brand-new topic at 0.99, which cleared every gate
# before the floor existed.
local({
    v <- tempfile("floor-vault-")
    pensar::init_vault(v)
    op <- options(pensar.vault = v)
    inst <- tempfile("floor-inst-")
    dir.create(file.path(inst, "vault", "raw", "matrix"), recursive = TRUE)
    on.exit({
        options(op)
        unlink(c(v, inst), recursive = TRUE)
    }, add = TRUE)
    # A dry run still resolves the destination room, which builds a
    # real Matrix client from this machine's config. Replace the lookup
    # with a fixed room so the test needs no config.
    ensure_orig <- get("ensure_matrix_room", envir = asNamespace("cerebro"))
    assignInNamespace("ensure_matrix_room", function(...) "!stub:ex",
                      "cerebro")
    on.exit(assignInNamespace("ensure_matrix_room", ensure_orig, "cerebro"),
            add = TRUE)

    writeLines(c("---", "source: '!ping:ex'", "title: '!ping:ex'", "---",
                 "# !ping:ex", "",
                 "## user", "", "Can you see the image?", "",
                 "## assistant", "", "I can. It renders."),
               file.path(inst, "vault", "raw", "matrix",
                         "2026-08-24-ping.md"))

    stub <- function(...) {
        args <- list(...)
        ev <- paste(unlist(args), collapse = " ")
        link <- regmatches(ev, regexpr("raw/chats/[^ \")]+", ev))
        cite <- if (length(link)) {
            sprintf("[[%s]]", sub("\\.md$", "", link[[1L]]))
        } else {
            ""
        }
        sprintf(paste0('{"topic":"Image tests","slug":"image-tests",',
                       '"confidence":0.99,"existing_path":"","room_id":"",',
                       '"room_name":"","summary":"Notes. %s"}'), cite)
    }

    run <- function(...) {
        msgs <- character()
        invisible(utils::capture.output(
            got <- withCallingHandlers(
                cerebro::sync_general_conversation_threads(
                    vault = v, instance_dir = inst,
                    matrix_roots = file.path(inst, "vault", "raw", "matrix"),
                    days = 30L, dry_run = TRUE, generate_fn = stub, ...),
                message = function(m) {
                    msgs <<- c(msgs, conditionMessage(m))
                    invokeRestart("muffleMessage")
                })))
        list(got = got, msgs = msgs)
    }

    floored <- run()
    expect_true(any(grepl("too slight", floored$msgs, fixed = TRUE)))
    expect_equal(length(floored$got), 0L)

    # The same ping with the floor lowered files normally, so the floor
    # is the only thing that stood between it and a room.
    open <- run(create_min_turns = 1L, create_min_chars = 0L)
    expect_false(any(grepl("too slight", open$msgs, fixed = TRUE)))
    expect_equal(length(open$got), 1L)

    # One long, substantive message clears through the character arm
    # even at a single turn.
    writeLines(c("---", "source: '!essay:ex'", "title: '!essay:ex'", "---",
                 "# !essay:ex", "",
                 "## user", "",
                 paste(rep("A real plan with real detail about the trip.",
                           12L), collapse = " "), "",
                 "## assistant", "", "Filed."),
               file.path(inst, "vault", "raw", "matrix",
                         "2026-08-24-essay.md"))
    essay <- run(exclude_matrix_rooms = "!ping:ex")
    expect_false(any(grepl("too slight", essay$msgs, fixed = TRUE)))
    expect_equal(length(essay$got), 1L)
})
