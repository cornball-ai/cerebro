library(tinytest)

# cerebro drives corteza's chat loop through four entry points. They were
# named matrix_* when Matrix was the only transport corteza could speak;
# corteza reaches every transport through chat.api now and renamed them
# bot_*, keeping the old names as wrappers that warn and forward.
#
# cerebro is the only caller of those wrappers, so this file is what says
# it stopped using them. The wrappers stay in corteza until its 1.0.0
# cleanup -- migrating the consumer first is the point of having them.

if (!requireNamespace("corteza", quietly = TRUE)) {
    exit_file("corteza not installed")
}

# ---- The names cerebro calls exist ----
# Not a tautology: these are the four that moved. If corteza 1.0.0 drops
# a wrapper and cerebro were still calling it, this is the line that goes
# red instead of a bot dying on its next restart.
for (fn in c("bot_run_init", "bot_run_step", "bot_request_flush")) {
    expect_true(fn %in% getNamespaceExports("corteza"), info = fn)
}

# And the source calls those, not the deprecated aliases. A wrapper still
# works, so nothing fails at runtime to tell us otherwise -- the only
# symptom is a warning nobody sees, because cerebro::run()'s loop never
# returns and R holds warnings until the top-level call finishes.
local({
    src <- unlist(lapply(ls(asNamespace("cerebro"), all.names = TRUE),
                         function(n) {
        obj <- get(n, envir = asNamespace("cerebro"))
        if (is.function(obj)) deparse(body(obj)) else character()
    }))
    for (old in c("matrix_run_init", "matrix_run_step",
                  "matrix_request_flush")) {
        expect_false(any(grepl(paste0("corteza::", old), src, fixed = TRUE)),
                     info = old)
    }
    # Sends go through chat.api, not through corteza at all: cerebro
    # exports its own matrix_send(), and corteza's send wrappers are not
    # on its path.
    expect_false(any(grepl('getExportedValue("corteza", "matrix_send")', src,
                           fixed = TRUE)))
    expect_false(any(grepl("corteza::matrix_send", src, fixed = TRUE)))
    expect_false(any(grepl("corteza::bot_send", src, fixed = TRUE)))
})

# The Depends bound covers them. bot_* arrived in corteza 0.7.1.19, and
# without the bound an install could resolve an older corteza and fail on
# the first poll rather than at install time.
local({
    dcf <- read.dcf(system.file("DESCRIPTION", package = "cerebro"),
                    fields = "Depends")[[1L]]
    m <- regmatches(dcf, regexpr("corteza \\(>= [0-9.]+\\)", dcf))
    expect_true(length(m) == 1L && nzchar(m))
    declared <- gsub("^.*>= |\\)$", "", m)
    expect_true(package_version(declared) >= package_version("0.7.1.19"))
    # And the installed copy satisfies it, so the bound is not vacuous.
    expect_true(utils::packageVersion("corteza") >= package_version(declared))
})

# ---- Startup ----
# .start_matrix_loop() reports what it got rather than throwing: a
# homeserver that is down at boot must leave the scheduler running, since
# routines are the half of cerebro that does not need Matrix.
local({
    orig <- corteza::bot_run_init
    assignInNamespace("bot_run_init", function(...) stop("homeserver down"),
                      ns = "corteza")
    on.exit(assignInNamespace("bot_run_init", orig, ns = "corteza"),
            add = TRUE)
    expect_message(state <- cerebro:::.start_matrix_loop(list()),
                   "matrix init failed")
    expect_null(state)
})
local({
    orig <- corteza::bot_run_init
    assignInNamespace("bot_run_init", function(system = NULL, model = NULL,
                                               provider = NULL,
                                               tools_filter = NULL,
                                               hooks = NULL) {
        list(sessions = "registry", opts = list(model = model, hooks = hooks))
    }, ns = "corteza")
    on.exit(assignInNamespace("bot_run_init", orig, ns = "corteza"),
            add = TRUE)
    expect_message(state <- cerebro:::.start_matrix_loop(
        list(model = "claude-opus-5")), "folded into the scheduler loop")
    # The config's overrides reach corteza rather than being dropped.
    expect_equal(state$opts$model, "claude-opus-5")
    # And the lifecycle's open half rides the loop as corteza's hooks:
    # without these, a /clear files nothing and a fold thread answers
    # unseeded.
    expect_identical(state$opts$hooks$on_clear, cerebro::segment_open)
    expect_identical(state$opts$hooks$thread_context, cerebro::fold_lookup)
})

# corteza's init accepts the hooks cerebro passes. The stub above fixes
# the signature cerebro codes against; this is the installed corteza
# saying the same.
expect_true("hooks" %in% names(formals(corteza::bot_run_init)))

# ---- Flush ----
# The matrix_archive routine is a signal-file poke at a separately
# running bot, so it has to reach corteza's current name.
local({
    called <- 0L
    orig <- corteza::bot_request_flush
    assignInNamespace("bot_request_flush", function() {
        called <<- called + 1L
        invisible(TRUE)
    }, ns = "corteza")
    on.exit(assignInNamespace("bot_request_flush", orig, ns = "corteza"),
            add = TRUE)
    fn <- cerebro:::.build_routine_fn(
        list(name = "archive", capability = "matrix_archive"),
        briefings = list(), cfg = list(), instance_dir = NULL)
    expect_true(is.function(fn))
    fn()
    expect_equal(called, 1L)
})

# ---- Send ----
# markdown and msgtype map onto chat.api's markup and kind, and the send
# goes to the resolved id, not the name it was given. The loopback
# transport records what arrives.
local({
    root <- file.path(tempdir(), paste0("cerebro-send-", Sys.getpid(), "-",
                                        sample.int(1e6, 1L)))
    dir.create(root, recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    read_state <- getFromNamespace("read_instance_state", "cerebro")
    write_state <- getFromNamespace("write_instance_state", "cerebro")
    state <- read_state(root)
    state$matrix_rooms <- list(lab = "!resolved:example")
    write_state(state, root)

    cl <- chat.api::chat_loopback()
    id <- cerebro::matrix_send("hello", room = "lab", instance_dir = root,
                               client = cl)
    sent <- chat.api::chat_history(cl, "!resolved:example")$messages
    expect_equal(length(sent), 1L)
    expect_equal(sent[[1L]]$id, id)
    expect_equal(sent[[1L]]$body, "hello")
    expect_equal(sent[[1L]]$markup, "markdown")
    # Everything through this wrapper is proactive persona traffic, so the
    # default is a notice: formatted in stock clients, no notification ping.
    expect_equal(sent[[1L]]$kind, "notice")

    cerebro::matrix_send("plain", room = "!resolved:example",
                         markdown = FALSE, client = cl)
    sent <- chat.api::chat_history(cl, "!resolved:example")$messages
    expect_equal(sent[[2L]]$markup, "plain")
    # A caller that wants a ping can still ask for one.
    cerebro::matrix_send("urgent", room = "!resolved:example",
                         msgtype = "m.text", client = cl)
    sent <- chat.api::chat_history(cl, "!resolved:example")$messages
    expect_equal(sent[[3L]]$kind, "message")
})

# A dry run neither resolves nor sends.
local({
    cl <- chat.api::chat_loopback()
    out <- capture.output(res <- cerebro::matrix_send("hi", dry_run = TRUE,
                                                      client = cl))
    expect_null(res)
    expect_true(any(grepl("dry-run", out, fixed = TRUE)))
    expect_length(chat.api::chat_channels(cl), 0L)
})
