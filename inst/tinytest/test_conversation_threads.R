library(tinytest)

window <- cerebro:::.local_date_window(as.Date("2026-06-24"), 1L,
                                       "America/Chicago")
expect_equal(
             format(window$end, tz = "America/Chicago", format = "%Y-%m-%d %H:%M:%S"),
             "2026-06-25 00:00:00"
)
expect_equal(as.numeric(window$end),
             as.numeric(as.POSIXct("2026-06-25 05:00:00", tz = "UTC")))

# A parsed session timestamp compares against the local-zone window
# without R complaining about inconsistent zone names: it names none,
# and it is still the right instant.
local({
    at <- cerebro:::.chat_parse_time("2026-06-25T04:59:59Z")
    expect_null(attr(at, "tzone"))
    expect_equal(as.numeric(at), as.numeric(window$end) - 1)
    expect_silent(at < window$end)
    expect_true(at < window$end && at >= window$cutoff)
    expect_true(is.na(cerebro:::.chat_parse_time("")))
})

root <- file.path(tempdir(), paste0("cerebro-chat-", Sys.getpid(), "-",
                                    sample.int(1e6, 1L)))
dir.create(root, recursive = TRUE)

matrix <- file.path(root, "matrix")
claude <- file.path(root, "claude")
codex <- file.path(root, "codex")
repo <- file.path(root, "repo")
dir.create(matrix)
dir.create(claude)
dir.create(codex)
dir.create(repo)
system2("git", c("-C", repo, "init", "-q"))

writeLines(c("---", "title: '!home:test'", "source: '!home:test'", "---", "",
             "# !home:test", "", "Room name at archive time: Home", "",
             "## user", "",
             "The media room should use acoustic panels.", "",
             "## assistant", "", "That also makes cable routing easier."), file.path(matrix, "home.md"))
writeLines(c("---", "title: To Do", "source: To Do", "---", "", "# To Do",
             "", "## assistant", "", "Vault check found 47 findings."), file.path(matrix, "notification.md"))
writeLines(c("---", "title: '!repo:test'", "source: '!repo:test'", "---", "",
             "# !repo:test", "", "Room name at archive time: ~/repo",
             paste0("Working directory at archive time: ", repo), "",
             "## user", "",
             "Matrix repo notes should stay with the repo."), file.path(matrix, "repo-matrix.md"))

now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
claude_rows <- list(
                    list(type = "user", timestamp = now, cwd = repo, isSidechain = FALSE,
                         message = list(content = "Keep this parser package-owned.")),
                    list(type = "assistant", timestamp = now, cwd = repo,
                         isSidechain = FALSE,
                         message = list(content = list(
                list(type = "text", text = "The repo remains the source of truth."),
                list(type = "tool_use", name = "ignored")
            ))),
                    list(type = "user", timestamp = now, cwd = repo, isSidechain = FALSE,
                         message = list(content = list(
                list(type = "tool_result", content = "must not appear")
            )))
)
writeLines(vapply(claude_rows, jsonlite::toJSON, character(1L),
                  auto_unbox = TRUE), file.path(claude, "repo.jsonl"))

codex_rows <- list(
                   list(type = "session_meta", timestamp = now, payload = list(cwd = root)),
                   list(type = "event_msg", timestamp = now,
                        payload = list(type = "user_message",
                                       message = "<environment_context>generated</environment_context>")),
                   list(type = "event_msg", timestamp = now,
                        payload = list(type = "user_message",
                                       message = "Vacation notes should become a travel thread.")),
                   list(type = "response_item", timestamp = now,
                        payload = list(type = "message", role = "assistant",
                                       content = list(list(type = "output_text",
                    text = "Keep the itinerary durable."))))
)
writeLines(vapply(codex_rows, jsonlite::toJSON, character(1L),
                  auto_unbox = TRUE), file.path(codex, "general.jsonl"))

ctx <- new.env(parent = emptyenv())
ctx$date <- Sys.Date()
ctx$instance_dir <- root
params <- list(include = c("matrix", "claude", "codex"), matrix_root = matrix,
               claude_root = claude, codex_root = codex, days = 1L,
               max_sessions = 9L, max_per_source = 4L, max_chars = 12000L)
all <- cerebro::briefing_source_chat_recent("daily", ctx, params)
text <- all[["RECENT CHAT EVIDENCE"]]
expect_true(grepl("acoustic panels", text, fixed = TRUE))
expect_true(grepl("parser package-owned", text, fixed = TRUE))
expect_true(grepl("Vacation notes", text, fixed = TRUE))
expect_false(grepl("Vault check found", text, fixed = TRUE))
expect_false(grepl("must not appear", text, fixed = TRUE))
expect_false(grepl("generated", text, fixed = TRUE))
parse_matrix <- getFromNamespace(".chat_parse_matrix", "cerebro")
home_parsed <- parse_matrix(file.path(matrix, "home.md"),
                            Sys.time() - 3600, Sys.time() + 3600)
expect_equal(home_parsed$label, "Home")
expect_equal(home_parsed$room_id, "!home:test")

general_ctx <- new.env(parent = emptyenv())
general_ctx$date <- Sys.Date()
general_ctx$instance_dir <- root
general <- cerebro::briefing_source_chat_recent("daily", general_ctx,
    c(params, list(scope = "general")))[["RECENT CHAT EVIDENCE"]]
expect_true(grepl("acoustic panels", general, fixed = TRUE))
expect_true(grepl("Vacation notes", general, fixed = TRUE))
expect_false(grepl("parser package-owned", general, fixed = TRUE))
expect_false(grepl("Matrix repo notes", general, fixed = TRUE))

repo_ctx <- new.env(parent = emptyenv())
repo_ctx$date <- Sys.Date()
repo_ctx$instance_dir <- root
repo_only <- cerebro::briefing_source_chat_recent("daily", repo_ctx,
    c(params, list(scope = "repo")))[["RECENT CHAT EVIDENCE"]]
expect_true(grepl("parser package-owned", repo_only, fixed = TRUE))
expect_true(grepl("Matrix repo notes", repo_only, fixed = TRUE))
expect_false(grepl("acoustic panels", repo_only, fixed = TRUE))

route_fn <- getFromNamespace(".thread_route_general", "cerebro")
records <- list(list(link = "raw/chats/home", body = "home evidence"))
candidates <- list(
                   pages = list(list(path = "wiki/home-improvement.md",
                                     title = "Home Improvement", tags = "house",
                                     excerpt = "Media room and backyard decisions.")),
                   rooms = list(list(id = "!home:test", name = "mybot · Home Improvement",
                                     topic = "Mybot topical thread"))
)
mock <- function(...) jsonlite::toJSON(list(
        topic = "Home Improvement", slug = "home-improvement",
        existing_path = "wiki/home-improvement.md",
        room_id = "!home:test", room_name = "mybot · Home Improvement", confidence = 0.93,
        summary = paste("Acoustic treatment belongs in the existing overview.",
                        "\n\n## Source conversations\n[[raw/chats/home]]")
    ), auto_unbox = TRUE)
route <- route_fn(records, candidates, "test", "test", 100L, mock)
expect_equal(route$existing_path, "wiki/home-improvement.md")
expect_equal(route$room_id, "!home:test")
expect_equal(route$room_name, "mybot · Home Improvement")
expect_equal(route$confidence, 0.93)

vault <- file.path(root, "vault")
pensar::init_vault(vault, rproj = FALSE, agent_instructions = FALSE)
append_fn <- getFromNamespace(".thread_append_page", "cerebro")
page <- file.path(vault, "wiki", "home-improvement.md")
append_fn(page, "Home Improvement", c("conversation"),
          "A durable observation. [[raw/chats/home]]", "abc", vault,
          as.Date("2026-06-24"))
write("\nUser-owned tail.", page, append = TRUE)
append_fn(page, "Home Improvement", c("conversation"),
          "A duplicate. [[raw/chats/home]]", "abc", vault,
          as.Date("2026-06-24"))
page_text <- paste(readLines(page, warn = FALSE), collapse = "\n")
matches <- gregexpr("cerebro:conversation:abc:start", page_text, fixed = TRUE)[[1L]]
expect_equal(sum(matches > 0L), 1L)
expect_true(grepl("User-owned tail", page_text, fixed = TRUE))

cfg <- file.path(root, "matrix.json")
writeLines(jsonlite::toJSON(list(server = "https://example.invalid",
                                 token = "secret",
                                 user_id = "@mybot:example.invalid",
                                 device_id = "device",
                                 room_id = "!default:example.invalid"), auto_unbox = TRUE, pretty = TRUE), cfg)
old <- Sys.getenv("CORTEZA_MATRIX_CONFIG", unset = NA_character_)
Sys.setenv(CORTEZA_MATRIX_CONFIG = cfg)
if (requireNamespace("mx.client", quietly = TRUE)) {
    # The environment names the config when no path is given, exactly
    # as corteza resolves its own.
    read_cfg <- getFromNamespace(".chat_config", "cerebro")
    expect_equal(read_cfg()$user_id, "@mybot:example.invalid")
    expect_equal(read_cfg(cfg)$room_id, "!default:example.invalid")
}

if (is.na(old)) {
    Sys.unsetenv("CORTEZA_MATRIX_CONFIG")
} else {
    Sys.setenv(CORTEZA_MATRIX_CONFIG = old)
}
repo_defaults <- formals(cerebro::sync_repo_conversation_threads)
general_defaults <- formals(cerebro::sync_general_conversation_threads)
for (defaults in list(repo_defaults, general_defaults)) {
    expect_null(defaults$vault)
    expect_null(defaults$instance_dir)
    expect_null(defaults$model)
    expect_null(defaults$matrix_config)
    expect_null(defaults$room_prefix)
    expect_equal(as.character(defaults$provider), "auto")
    expect_length(eval(defaults$invite), 0L)
    text <- tolower(paste(deparse(defaults), collapse = " "))
    expect_false(grepl("troy|tiny|cornelius", text))
}

state_writer <- getFromNamespace(".thread_write_conversation_state", "cerebro")
read_state <- getFromNamespace("read_instance_state", "cerebro")
write_state <- getFromNamespace("write_instance_state", "cerebro")
state <- read_state(root)
state$matrix_rooms <- list(Cooking = "!room:test")
write_state(state, root)
stale <- list(conversation_threads = list(
        general_sources = list(abc = list(raw_hash = "raw"))
    ))
state_writer(stale, root, "general_sources")
merged <- read_state(root)
expect_equal(merged$matrix_rooms$Cooking, "!room:test")
expect_equal(merged$conversation_threads$general_sources$abc$raw_hash, "raw")

repo_sync <- try(cerebro::sync_repo_conversation_threads(vault = vault,
        instance_dir = root, days = 0L, max_sessions = 0L, dry_run = TRUE,
        model = "m"), silent = TRUE)
expect_false(inherits(repo_sync, "try-error"))

# No model and no generate_fn stops before any work: cerebro picks no
# default model.
for (sync_fn in list(cerebro::sync_repo_conversation_threads,
                     cerebro::sync_general_conversation_threads)) {
    expect_error(sync_fn(vault = vault, instance_dir = root, days = 0L,
                         max_sessions = 0L, dry_run = TRUE),
                 "`model` is required")
}

# Repo rooms are titled with the shell-style repo path, not the prefixed
# "<instance> · <repo>" form (dry-run wiring through the real sync).
collect_orig <- getFromNamespace(".chat_collect", "cerebro")
fake_session <- list(source = "claude", path = "fixture.jsonl",
                     label = "fixture", cwd = repo,
                     turns = list(
                                  list(role = "user", text = "Keep this durable."),
                                  list(role = "assistant", text = "Recorded.")
                     ))
assignInNamespace(".chat_collect", function(ctx, params) list(fake_session),
                  ns = "cerebro")
gen <- function(prompt, system, model, provider, max_tokens) {
    links <- regmatches(prompt,
                        gregexpr("\\[\\[raw/[^]]+\\]\\]", prompt))[[1L]]
    paste("A durable update.", paste(links, collapse = " "))
}
# A loopback transport with no rooms: every lookup misses, so the dry
# run reports the room it would create.
msgs <- capture.output(
    cerebro::sync_repo_conversation_threads(vault = vault, instance_dir = root,
        dry_run = TRUE, client = chat.api::chat_loopback(),
        generate_fn = gen),
    type = "message"
)
line <- msgs[grepl("would create Matrix room", msgs, fixed = TRUE)]
expect_equal(length(line), 1L)
expect_true(grepl(basename(repo), line, fixed = TRUE))
expect_false(grepl(" · ", line, fixed = TRUE))
assignInNamespace(".chat_collect", collect_orig, ns = "cerebro")

unlink(root, recursive = TRUE)

# Archives with no frontmatter or room metadata keep the filename label.
bare_dir <- file.path(tempdir(), paste0("cerebro-bare-", Sys.getpid()))
dir.create(bare_dir, showWarnings = FALSE)
writeLines(c("## user", "", "Just a bare note.", "",
             "## assistant", "", "Noted."),
           file.path(bare_dir, "bare-note.md"))
bare_parsed <- getFromNamespace(".chat_parse_matrix", "cerebro")(
    file.path(bare_dir, "bare-note.md"),
    Sys.time() - 3600, Sys.time() + 3600)
expect_equal(bare_parsed$label, "bare-note")
expect_null(bare_parsed$room_id)
unlink(bare_dir, recursive = TRUE)

# Generated summaries are dropped before a matrix archive becomes a
# conversation source. Thread rooms are archived like any other room, so
# without this a summary posted into one becomes input for the next run,
# which summarizes it and posts again.
local({
    is_gen <- cerebro:::.chat_is_generated_summary

    # Our own summaries, as they appear in archives from both send sites.
    expect_true(is_gen("assistant",
                       "Conversation summary filed in **Coffee brewing**\n\nbody"))
    expect_true(is_gen("assistant",
                       "Conversation summary filed in **cerebro**\n\n## notes"))
    # Leading whitespace from the archive's turn splitting still matches.
    expect_true(is_gen("assistant",
                       "\n  Conversation summary filed in **X**\n\nbody"))

    # Role is what prevents false positives: a human quoting the phrase
    # is a user turn and must survive.
    expect_false(is_gen("user",
                        "Conversation summary filed in **Coffee brewing**"))
    # Ordinary assistant replies survive.
    expect_false(is_gen("assistant", "Yes, the core issue is heat loss."))
    # The phrase mid-message is not the marker; only the opening is.
    expect_false(is_gen("assistant",
                        "I saw Conversation summary filed in **X** earlier"))
    expect_false(is_gen("assistant", ""))
    expect_false(is_gen("assistant", NULL))
})

# End to end: an archive of a thread room whose only assistant turns are
# summaries yields no usable source, because what remains has no user
# turn for the collector to keep.
local({
    d <- tempfile("archive-")
    dir.create(d, recursive = TRUE)
    on.exit(unlink(d, recursive = TRUE), add = TRUE)

    self_talk <- file.path(d, "2026-07-27-thread-room.md")
    writeLines(c("---", "source: '!thread:ex'", "title: '!thread:ex'", "---",
                 "# !thread:ex", "",
                 "## assistant", "",
                 "Conversation summary filed in **Coffee brewing**", "",
                 "## assistant", "",
                 "Conversation summary filed in **Coffee brewing**", ""),
               self_talk)
    parsed <- cerebro:::.chat_parse_matrix(self_talk, Sys.time() - 86400,
                                           Sys.time() + 86400)
    expect_true(is.null(parsed) || length(parsed$turns) == 0L)

    # A real exchange in the same room still parses, and the summary
    # mixed into it is dropped rather than taking the whole file with it.
    mixed <- file.path(d, "2026-07-27-real-talk.md")
    writeLines(c("---", "source: '!thread:ex'", "title: '!thread:ex'", "---",
                 "# !thread:ex", "",
                 "## assistant", "",
                 "Conversation summary filed in **Coffee brewing**", "",
                 "## user", "",
                 "what about a gooseneck kettle", "",
                 "## assistant", "",
                 "It gives you flow control the drip assist cannot.", ""),
               mixed)
    got <- cerebro:::.chat_parse_matrix(mixed, Sys.time() - 86400,
                                        Sys.time() + 86400)
    expect_equal(length(got$turns), 2L)
    roles <- vapply(got$turns, `[[`, character(1L), "role")
    expect_equal(roles, c("user", "assistant"))
    expect_false(any(grepl("Conversation summary filed",
                           vapply(got$turns, `[[`, character(1L), "text"),
                           fixed = TRUE)))
})

# Round trip: a message built by the emitter is recognized by the
# parser. Both derive from one literal, so a wording edit that missed
# either side fails here rather than silently restarting the loop.
local({
    msg <- cerebro:::.thread_summary_message("Coffee brewing", "## notes\n\nbody")
    expect_true(cerebro:::.chat_is_generated_summary("assistant", msg))
    # The same text from a person is still a real turn.
    expect_false(cerebro:::.chat_is_generated_summary("user", msg))
    # And it survives the archive's turn splitting, which leaves the
    # leading newline the parser trims.
    expect_true(cerebro:::.chat_is_generated_summary("assistant",
                                                     paste0("\n", msg)))
})

# Duplicate conversation sources collapse before routing, so one
# conversation is summarized once and posted once regardless of how many
# times it was archived. Against the live vault this collapses 117 of
# 284 usable sources.
local({
    mk <- function(path, ..., label = "room") {
        list(label = label, path = path,
             turns = lapply(list(...), function(x) {
                 list(role = x[[1L]], text = x[[2L]])
             }))
    }
    a <- mk("/v/2026-07-01-room.md",
            list("user", "how do I brew this"),
            list("assistant", "slowly"))
    # Same conversation, archived again on a later day: different path,
    # different date, identical content.
    b <- mk("/v/2026-07-27-room.md",
            list("user", "how do I brew this"),
            list("assistant", "slowly"))
    c <- mk("/v/2026-07-27-other.md",
            list("user", "different question"),
            list("assistant", "different answer"))

    # Path and date are excluded from identity on purpose: that
    # variation is what made copies look distinct.
    expect_identical(cerebro:::.thread_session_fingerprint(a),
                     cerebro:::.thread_session_fingerprint(b))
    expect_false(identical(cerebro:::.thread_session_fingerprint(a),
                           cerebro:::.thread_session_fingerprint(c)))

    got <- suppressMessages(cerebro:::.thread_dedupe_sessions(list(a, b, c)))
    expect_equal(length(got), 2L)
    # First occurrence wins, so the earliest copy is the one cited.
    expect_equal(got[[1L]]$path, "/v/2026-07-01-room.md")
    expect_equal(got[[2L]]$path, "/v/2026-07-27-other.md")

    # It reports what it ignored rather than dropping silently.
    expect_message(cerebro:::.thread_dedupe_sessions(list(a, b)),
                   pattern = "1 duplicate")

    # Order within a conversation is part of its identity.
    swapped <- mk("/v/x.md",
                  list("assistant", "slowly"),
                  list("user", "how do I brew this"))
    expect_false(identical(cerebro:::.thread_session_fingerprint(a),
                           cerebro:::.thread_session_fingerprint(swapped)))

    # Degenerate inputs pass straight through.
    expect_equal(length(cerebro:::.thread_dedupe_sessions(list())), 0L)
    expect_equal(length(cerebro:::.thread_dedupe_sessions(list(a))), 1L)
    # A conversation with no turns is still a valid, stable identity.
    empty <- list(label = "e", path = "/v/e.md", turns = list())
    expect_true(nzchar(cerebro:::.thread_session_fingerprint(empty)))
})

# Regressions from review of the deduplication change.
local({
    mk <- function(path, ..., room = NULL, source = "matrix") {
        s <- list(label = "room", path = path, source = source,
                  turns = lapply(list(...), function(x) {
                      list(role = x[[1L]], text = x[[2L]])
                  }))
        if (!is.null(room)) s$room_id <- room
        s
    }

    # Whitespace is content. An earlier version trimmed, so the identity
    # was not the full content it claimed to be.
    a <- mk("/v/a.md", list("user", "one\n\ntwo"))
    b <- mk("/v/b.md", list("user", "one\ntwo"))
    expect_false(identical(cerebro:::.thread_session_fingerprint(a),
                           cerebro:::.thread_session_fingerprint(b)))
    lead <- mk("/v/c.md", list("user", "  padded"))
    expect_false(identical(cerebro:::.thread_session_fingerprint(lead),
                           cerebro:::.thread_session_fingerprint(
                               mk("/v/d.md", list("user", "padded")))))

    # Identical short exchanges in different rooms are different
    # conversations, not duplicates.
    r1 <- mk("/v/r1.md", list("user", "ok"), room = "!one:ex")
    r2 <- mk("/v/r2.md", list("user", "ok"), room = "!two:ex")
    expect_false(identical(cerebro:::.thread_session_fingerprint(r1),
                           cerebro:::.thread_session_fingerprint(r2)))
    # The same room, archived twice, is one conversation.
    r1b <- mk("/v/r1-later.md", list("user", "ok"), room = "!one:ex")
    expect_identical(cerebro:::.thread_session_fingerprint(r1),
                     cerebro:::.thread_session_fingerprint(r1b))

    # Sources without a room namespace fall back explicitly rather than
    # silently sharing one.
    cl <- mk("/v/x.jsonl", list("user", "ok"), source = "claude")
    cx <- mk("/v/y.jsonl", list("user", "ok"), source = "codex")
    expect_false(identical(cerebro:::.thread_session_fingerprint(cl),
                           cerebro:::.thread_session_fingerprint(cx)))
})

# The collection budget counts distinct conversations: duplicates must
# not consume slots and leave older material undiscovered.
local({
    d <- tempfile("collect-")
    dir.create(file.path(d, "matrix"), recursive = TRUE)
    on.exit(unlink(d, recursive = TRUE), add = TRUE)

    write_archive <- function(name, room, body) {
        writeLines(c("---", paste0("source: '", room, "'"),
                     paste0("title: '", room, "'"), "---",
                     paste0("# ", room), "",
                     "## user", "", body, ""),
                   file.path(d, "matrix", name))
    }
    # Five archives of one room, then three distinct rooms.
    for (i in 1:5) {
        write_archive(sprintf("2026-07-%02d-dup.md", i), "!dup:ex", "same talk")
    }
    for (i in 1:3) {
        write_archive(sprintf("2026-07-1%d-uniq.md", i),
                      sprintf("!u%d:ex", i), sprintf("distinct talk %d", i))
    }

    ctx <- new.env(parent = emptyenv())
    ctx$date <- Sys.Date()
    ctx$instance_dir <- d
    got <- cerebro:::.chat_collect(ctx, list(
        include = "matrix", matrix_root = file.path(d, "matrix"),
        days = 3650L, max_sessions = 4L, max_per_source = 4L))

    # Four slots, and the five duplicates must occupy exactly one of
    # them, leaving three for the distinct rooms.
    expect_equal(length(got), 4L)
    rooms <- vapply(got, function(s) s$room_id %||% "", character(1L))
    expect_equal(length(unique(rooms)), 4L)
    expect_true("!dup:ex" %in% rooms)
    expect_true(all(sprintf("!u%d:ex", 1:3) %in% rooms))
})

# Deduplication survives across runs: state is keyed on what the
# conversation is, not where a copy of it lives. Previously, once the
# archive we had summarized aged out of the lookback, an identical copy
# at another path read as new and was summarized again.
if (requireNamespace("pensar", quietly = TRUE)) {
    local({
        v <- tempfile("vault-")
        pensar::init_vault(v)
        op <- options(pensar.vault = v)
        on.exit({
            options(op)
            unlink(v, recursive = TRUE)
        }, add = TRUE)

        mk <- function(path) {
            list(label = "room", path = path, source = "matrix",
                 room_id = "!persist:ex", time = Sys.time(),
                 turns = list(list(role = "user", text = "durable question"),
                              list(role = "assistant", text = "durable answer")))
        }

        first <- cerebro:::.thread_prepare_records(list(mk("/v/day1.md")), v,
                                                   list())
        expect_equal(length(first$records), 1L)
        # A record stays pending until the run that summarized it says
        # so, which is what a completed pass does.
        done <- cerebro:::.thread_mark_summarized(first$state, first$records)

        # Same conversation, new archive path, carrying forward the state
        # the first run wrote. Nothing new to summarize.
        again <- cerebro:::.thread_prepare_records(list(mk("/v/day9.md")), v,
                                                   done)
        expect_equal(length(again$records), 0L)

        # A genuinely different conversation in the same room is new.
        other <- list(label = "room", path = "/v/day9b.md", source = "matrix",
                      room_id = "!persist:ex", time = Sys.time(),
                      turns = list(list(role = "user", text = "new question")))
        third <- cerebro:::.thread_prepare_records(list(other), v, again$state)
        expect_equal(length(third$records), 1L)
    })
}

# Candidates are not truncated before identity is considered: a long
# run of newer duplicates must not hide older unique conversations.
local({
    d <- tempfile("cap-")
    dir.create(file.path(d, "matrix"), recursive = TRUE)
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    w <- function(name, room, body, age_days) {
        f <- file.path(d, "matrix", name)
        writeLines(c("---", paste0("source: '", room, "'"),
                     paste0("title: '", room, "'"), "---",
                     paste0("# ", room), "", "## user", "", body, ""), f)
        Sys.setFileTime(f, Sys.time() - age_days * 86400)
    }
    # 25 newer copies of one conversation, then 3 older unique ones.
    # With a 4-slot budget the old candidate cap was 24, so the unique
    # files fell outside the list entirely and were never seen.
    for (i in 1:25) w(sprintf("dup-%02d.md", i), "!dup:ex", "same talk", 1)
    for (i in 1:3) {
        w(sprintf("uniq-%d.md", i), sprintf("!u%d:ex", i),
          sprintf("distinct %d", i), 5)
    }
    ctx <- new.env(parent = emptyenv())
    ctx$date <- Sys.Date()
    ctx$instance_dir <- d
    got <- suppressMessages(cerebro:::.chat_collect(ctx, list(
        include = "matrix", matrix_root = file.path(d, "matrix"),
        days = 3650L, max_sessions = 4L, max_per_source = 4L)))
    expect_equal(length(got), 4L)
    rooms <- sort(vapply(got, function(s) s$room_id %||% "", character(1L)))
    expect_equal(rooms, c("!dup:ex", "!u1:ex", "!u2:ex", "!u3:ex"))
})

# Genuine pre-0.1.1.15 state migrates without reprocessing. The old
# scheme keyed on (source, path) and stored a SHA-1 of the rendered
# body, so the new fingerprint can never match it directly.
if (requireNamespace("pensar", quietly = TRUE)) {
    local({
        v <- tempfile("vault-")
        pensar::init_vault(v)
        op <- options(pensar.vault = v)
        on.exit({
            options(op)
            unlink(v, recursive = TRUE)
        }, add = TRUE)

        s <- list(label = "room", path = "/v/old.md", source = "matrix",
                  room_id = "!mig:ex", time = Sys.time(),
                  turns = list(list(role = "user", text = "old question"),
                               list(role = "assistant", text = "old answer")))

        # Reconstruct state exactly as the old scheme wrote it.
        turns <- cerebro:::.chat_select_turns(s$turns, 12L, TRUE)
        body <- cerebro:::.thread_session_markdown(s, turns)
        legacy_key <- digest::digest(paste(s$source, s$path, sep = "::"),
                                     algo = "sha1")
        # The raw evidence the old run wrote has to actually exist,
        # or a forced run correctly rewrites it instead of reusing it.
        dir.create(file.path(v, "raw", "chats"), recursive = TRUE,
                   showWarnings = FALSE)
        writeLines(body, file.path(v, "raw", "chats", "old.md"))
        old_state <- list()
        old_state[[legacy_key]] <- list(
            summary_hash = digest::digest(body, algo = "sha1"),
            raw_hash = digest::digest(body, algo = "sha1"),
            raw_path = "raw/chats/old.md")

        got <- cerebro:::.thread_prepare_records(list(s), v, old_state)
        # Already summarized under the old scheme: nothing to redo.
        expect_equal(length(got$records), 0L)
        # ... and it is carried forward under the new identity, so the
        # next run does not consult the legacy entry again.
        expect_true(cerebro:::.thread_session_fingerprint(s) %in%
                    names(got$state))

        # force = TRUE still reprocesses it. Migration used to return
        # early, skipping the force check, so an explicitly forced run
        # silently produced nothing for anything on legacy state.
        forced <- cerebro:::.thread_prepare_records(list(s), v, old_state,
                                                     force = TRUE)
        expect_equal(length(forced$records), 1L)
        # It reuses the raw evidence the legacy entry already points at
        # rather than writing it again.
        expect_equal(forced$records[[1L]]$raw_path, "raw/chats/old.md")
        # ... and the migrated identity is still recorded.
        expect_true(cerebro:::.thread_session_fingerprint(s) %in%
                    names(forced$state))

        # A conversation the old state never saw is still new.
        fresh <- s
        fresh$turns <- list(list(role = "user", text = "unseen"))
        expect_equal(length(
            cerebro:::.thread_prepare_records(list(fresh), v, old_state)$records
        ), 1L)
    })
}

# Identical conversations in two repositories stay distinct.
local({
    mk <- function(repo) {
        list(label = "session", path = paste0("/c/", repo, ".jsonl"),
             source = "claude", cwd = file.path("/home/user", repo),
             turns = list(list(role = "user", text = "run the tests")))
    }
    expect_false(identical(
        cerebro:::.thread_session_fingerprint(mk("alpha")),
        cerebro:::.thread_session_fingerprint(mk("beta"))))
})

# Destination grouping: several conversations routed to one topic
# produce one page update and one Matrix message, not one of each per
# source. This is the behaviour that put four summaries in one room.
local({
    mk <- function(topic, summary, label, when = "2026-07-28") {
        list(route = list(topic = topic, summary = summary, room_id = "",
                          room_name = "", existing_path = "", confidence = 1),
             records = list(list(key = paste0("k-", label), hash = "h",
                                 raw_path = "raw/chats/x.md",
                                 link = "raw/chats/x",
                                 session = list(label = label,
                                                time = as.POSIXct(when)))))
    }

    # One source keeps its summary verbatim, so the common case is
    # unchanged by grouping.
    one <- list(mk("Coffee", "just the summary", "room a"))
    expect_identical(cerebro:::.thread_group_body(one), "just the summary")

    # Several sources become sections, each attributed, in order.
    # Each summary carries its own citation, as the router is required
    # to produce, so duplication is observable.
    many <- list(mk("Coffee", "first summary [[raw/chats/a]]", "morning room"),
                 mk("Coffee", "second summary [[raw/chats/b]]", "evening room"))
    body <- cerebro:::.thread_group_body(many)
    n_hits <- function(pat, x) sum(gregexpr(pat, x, fixed = TRUE)[[1L]] > 0L)
    # Headings pair the readable label with a date: the label alone
    # repeats whenever one room contributes twice, which is common.
    expect_true(grepl("### morning room \u2014 2026-07-28", body, fixed = TRUE))
    expect_true(grepl("### evening room \u2014 2026-07-28", body, fixed = TRUE))
    # Each summary already carries its citation, so the body must not
    # append another. Count positive matches: gregexpr returns -1 for no
    # match, and length(-1) is 1, so a length check passes on zero hits.
    expect_equal(n_hits("raw/chats/a", body), 1L)
    expect_equal(n_hits("raw/chats/b", body), 1L)
    expect_equal(n_hits("Source: [[", body), 0L)
    # Guard the counter itself against the trap it exists to avoid.
    expect_equal(n_hits("raw/chats/absent", body), 0L)
    expect_true(grepl("first summary", body, fixed = TRUE))
    expect_true(grepl("second summary", body, fixed = TRUE))
    # Distinct sources are preserved, not merged into one blob.
    expect_true(regexpr("morning", body, fixed = TRUE) <
                regexpr("evening", body, fixed = TRUE))

    # A missing label degrades to the topic rather than erroring.
    unlabelled <- list(mk("Coffee", "a", NULL), mk("Coffee", "b", NULL))
    expect_true(grepl("### Coffee", cerebro:::.thread_group_body(unlabelled),
                      fixed = TRUE))

    # The marker covers the COMPLETE source set, in canonical order.
    # Keyed on the first record only, a retry that gained or lost a
    # source wrote nothing while those sources could still be marked
    # summarized.
    r <- function(h) list(hash = h)
    m1 <- cerebro:::.thread_group_marker(list(r("a"), r("b")))
    expect_false(identical(m1, cerebro:::.thread_group_marker(list(r("a")))))
    # Changing a NON-first contributor changes it.
    expect_false(identical(m1, cerebro:::.thread_group_marker(
        list(r("a"), r("c")))))
    # Order of arrival does not.
    expect_identical(m1, cerebro:::.thread_group_marker(list(r("b"), r("a"))))

    # The Matrix message is a digest, not the concatenated body: with
    # many independently generated summaries the combination is
    # unbounded, and the page is where the full sections live.
    tv <- tempfile("gv-"); dir.create(file.path(tv, "wiki"), recursive = TRUE)
    on.exit(unlink(tv, recursive = TRUE), add = TRUE)
    records <- unlist(lapply(many, `[[`, "records"), recursive = FALSE)
    msg <- cerebro:::.thread_update_message("Coffee", records,
                                           file.path(tv, "wiki", "coffee.md"), tv)
    expect_true(grepl("2 conversations filed", msg, fixed = TRUE))
    expect_true(grepl("wiki/coffee", msg, fixed = TRUE))
    expect_false(grepl("first summary", msg, fixed = TRUE))
    # A single source is also a linked handoff. Full summaries stay on
    # the page so mixed-origin prose cannot leak into a filtered post.
    single <- cerebro:::.thread_update_message("Coffee", one[[1L]]$records,
                                               file.path(tv, "wiki", "c.md"), tv)
    expect_true(grepl("1 conversation filed", single, fixed = TRUE))
    expect_true(grepl("[[raw/chats/x]]", single, fixed = TRUE))
    expect_false(grepl("just the summary", single, fixed = TRUE))
})

# End to end: two conversations routed to one topic produce one page
# update, not one per conversation. Run as a dry run so nothing is
# written or sent; the reported updates are the observable.
if (requireNamespace("pensar", quietly = TRUE)) {
    local({
        v <- tempfile("vault-")
        pensar::init_vault(v)
        op <- options(pensar.vault = v)
        inst <- tempfile("inst-")
        dir.create(file.path(inst, "vault", "raw", "matrix"), recursive = TRUE)
        on.exit({
            options(op)
            unlink(c(v, inst), recursive = TRUE)
        }, add = TRUE)
        # A dry run still resolves the destination room, which builds a
        # real Matrix client from this machine's config. Replace the
        # lookup with a fixed room so the test needs no config.
        ensure_orig <- get("ensure_matrix_room", envir = asNamespace("cerebro"))
        assignInNamespace("ensure_matrix_room", function(...) "!stub:ex",
                          "cerebro")
        on.exit(assignInNamespace("ensure_matrix_room", ensure_orig, "cerebro"),
                add = TRUE)

        # Two genuinely distinct conversations, different rooms.
        for (i in 1:2) {
            writeLines(c("---", sprintf("source: '!room%d:ex'", i),
                         sprintf("title: '!room%d:ex'", i), "---",
                         sprintf("# !room%d:ex", i), "",
                         "## user", "", sprintf("brewing question %d", i), "",
                         "## assistant", "", sprintf("brewing answer %d", i), "",
                         "## user", "", sprintf("brewing follow-up %d", i), "",
                         "## assistant", "", "Steeping time matters.", "",
                         "## user", "", sprintf("brewing conclusion %d", i), ""),
                       file.path(inst, "vault", "raw", "matrix",
                                 sprintf("2026-07-28-room%d.md", i)))
        }

        # Route both to the same topic, with a citation so the summary
        # passes the citation requirement.
        stub <- function(...) {
            args <- list(...)
            ev <- paste(unlist(args), collapse = " ")
            link <- regmatches(ev, regexpr("raw/chats/[^ \")]+", ev))
            cite <- if (length(link)) sprintf("[[%s]]", sub("\\.md$", "", link[[1L]])) else ""
            sprintf('{"topic":"Coffee brewing","slug":"coffee-brewing","confidence":0.99,"existing_path":"","room_id":"","room_name":"","summary":"Notes. %s"}', cite)
        }

        # dry_run reports each intended page append via message() and
        # prints each intended send via cat(), so both side effects are
        # countable rather than inferred from the return value.
        msgs <- character()
        sent <- utils::capture.output(
            got <- withCallingHandlers(
                cerebro::sync_general_conversation_threads(
                    vault = v, instance_dir = inst,
                    matrix_roots = file.path(inst, "vault", "raw", "matrix"),
                    days = 30L, dry_run = TRUE, generate_fn = stub),
                message = function(m) {
                    msgs <<- c(msgs, conditionMessage(m))
                    invokeRestart("muffleMessage")
                }))
        # Exactly one page append and one send for two conversations.
        expect_equal(sum(grepl("would update", msgs, fixed = TRUE)), 1L)
        expect_equal(sum(grepl("matrix_send (dry-run)", sent, fixed = TRUE)), 1L)
        # The one message is the digest, not the concatenated summaries.
        expect_true(any(grepl("2 conversations filed", sent, fixed = TRUE)))

        # One destination, carrying both contributing conversations,
        # where before this was one destination entry per source.
        expect_equal(length(got), 1L)
        expect_equal(got[[1L]]$topic, "Coffee brewing")
        expect_equal(got[[1L]]$sources, 2L)
        # Keyed by destination page, not by topic: two pages sharing a
        # topic string used to overwrite each other here.
        expect_true(grepl("coffee-brewing", names(got)[[1L]], fixed = TRUE))
    })
}

# Conflicting destination rooms are refused rather than guessed at. The
# router chooses existing_path and room_id independently, so routes
# sharing a page can disagree about the room; taking the first one would
# post a merged update into whichever happened to sort first, and could
# create a duplicate room alongside an existing one.
local({
    g <- function(room) list(route = list(room_id = room))

    # None known: the caller creates one.
    expect_equal(length(cerebro:::.thread_group_room_ids(
        list(g(""), g("")))), 0L)
    # One known, however many routes mention it.
    expect_equal(cerebro:::.thread_group_room_ids(
        list(g("!a:ex"), g(""), g("!a:ex"))), "!a:ex")
    # Two known is a disagreement, and both are reported so the caller
    # cannot silently take one.
    expect_equal(length(cerebro:::.thread_group_room_ids(
        list(g("!a:ex"), g("!b:ex")))), 2L)
    # A NULL room_id is absent, not an empty room id.
    expect_equal(length(cerebro:::.thread_group_room_ids(
        list(list(route = list())))), 0L)
})

# exclude_matrix_rooms suppresses a room entirely, by stable room id.
# matrix_roots cannot express this: it selects directories, and every
# room in an instance shares one.
local({
    d <- tempfile("excl-")
    dir.create(file.path(d, "matrix"), recursive = TRUE)
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    w <- function(name, room, body) {
        writeLines(c("---", paste0("source: '", room, "'"),
                     paste0("title: '", room, "'"), "---",
                     paste0("# ", room), "", "## user", "", body, ""),
                   file.path(d, "matrix", name))
    }
    w("a.md", "!keep:ex", "keep this one")
    w("b.md", "!drop:ex", "suppress this one")
    w("c.md", "!drop:ex", "suppress this too")
    # Make the excluded archives strictly newer than the kept one, so
    # the budget test below genuinely exercises ordering rather than
    # passing on equal-resolution mtimes.
    Sys.setFileTime(file.path(d, "matrix", "a.md"), Sys.time() - 3 * 86400)
    Sys.setFileTime(file.path(d, "matrix", "b.md"), Sys.time() - 60)
    Sys.setFileTime(file.path(d, "matrix", "c.md"), Sys.time() - 30)

    ctx <- new.env(parent = emptyenv())
    ctx$date <- Sys.Date()
    ctx$instance_dir <- d
    collect <- function(excl) {
        suppressMessages(cerebro:::.chat_collect(ctx, list(
            include = "matrix", matrix_root = file.path(d, "matrix"),
            exclude_matrix_rooms = excl,
            days = 3650L, max_sessions = 10L, max_per_source = 10L)))
    }

    # Unset: everything comes through, so the default is unchanged.
    expect_equal(length(collect(character())), 3L)

    # Excluded: only the kept room survives, and the exclusion is by
    # room id rather than by anything in the filename.
    got <- collect("!drop:ex")
    expect_equal(length(got), 1L)
    expect_equal(got[[1L]]$room_id, "!keep:ex")

    # Several ids at once, and an id matching nothing is harmless.
    expect_equal(length(collect(c("!drop:ex", "!keep:ex"))), 0L)
    # Excluding everything still reports why nothing was collected: the
    # count used to be skipped by the empty-result return above it.
    expect_message(cerebro:::.chat_collect(ctx, list(
        include = "matrix", matrix_root = file.path(d, "matrix"),
        exclude_matrix_rooms = c("!drop:ex", "!keep:ex"),
        days = 3650L, max_sessions = 10L, max_per_source = 10L)),
        pattern = "3 archive\\(s\\) from excluded room")
    expect_equal(length(collect("!absent:ex")), 3L)

    # It reports what it suppressed rather than dropping silently.
    expect_message(cerebro:::.chat_collect(ctx, list(
        include = "matrix", matrix_root = file.path(d, "matrix"),
        exclude_matrix_rooms = "!drop:ex",
        days = 3650L, max_sessions = 10L, max_per_source = 10L)),
        pattern = "excluded room")

    # Exclusion happens before the budget is spent, so a suppressed room
    # cannot crowd out material the caller does want.
    budgeted <- suppressMessages(cerebro:::.chat_collect(ctx, list(
        include = "matrix", matrix_root = file.path(d, "matrix"),
        exclude_matrix_rooms = "!drop:ex",
        days = 3650L, max_sessions = 1L, max_per_source = 1L)))
    expect_equal(budgeted[[1L]]$room_id, "!keep:ex")
})
