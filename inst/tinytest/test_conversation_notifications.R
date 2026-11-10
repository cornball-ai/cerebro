# Exercise both real filers, including wiki writes and processed-source state.
# Only collection, generation and Matrix access are replaced with offline fakes.
local({
    ns <- asNamespace("cerebro")
    names <- c(".chat_collect", ".thread_topic_candidates", "ensure_matrix_room",
               ".stamp_topic_room", "matrix_send")
    originals <- lapply(names, get, envir = ns)
    on.exit({
        for (i in seq_along(names)) {
            assignInNamespace(names[[i]], originals[[i]], "cerebro")
        }
    }, add = TRUE)
    root <- tempfile("conversation-notifications-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    repo <- file.path(root, "mypkg")
    dir.create(repo)
    stopifnot(system2("git", c("-C", shQuote(repo), "init", "-q")) == 0L)

    destination <- "!destination:example.invalid"
    collected <- list()
    sent <- list()
    generated <- 0L
    collected_n <- 0L
    assignInNamespace(".chat_collect", function(...) {
        collected_n <<- collected_n + 1L
        collected
    }, "cerebro")
    assignInNamespace(".thread_topic_candidates", function(...) {
        list(pages = list(list(path = "wiki/topic.md", title = "Topic")),
             rooms = list(list(room_id = destination, name = "Topic")))
    }, "cerebro")
    assignInNamespace("ensure_matrix_room", function(...) destination, "cerebro")
    # Do not force the session argument: no network session is needed.
    assignInNamespace(".stamp_topic_room", function(...) invisible(TRUE), "cerebro")
    assignInNamespace("matrix_send", function(text, room, ...) {
        sent[[length(sent) + 1L]] <<- list(text = text, room = room)
        "$synthetic-event"
    }, "cerebro")

    make_session <- function(label, origin, kind, thread = NULL) {
        list(source = if (is.null(origin)) "claude" else "matrix",
             path = file.path(root, paste0(label, ".md")), label = label,
             cwd = if (kind == "repo") repo else root,
             room_id = origin, thread = thread, time = Sys.time(),
             turns = list(list(role = "user", text = paste(label, "durable fact")),
                          list(role = "assistant", text = "Recorded.")))
    }
    generate <- function(prompt, system, ...) {
        generated <<- generated + 1L
        links <- unique(regmatches(prompt,
            gregexpr("\\[\\[raw/[^]]+\\]\\]", prompt))[[1L]])
        labels <- c("local", "remote", "unknown", "thread")
        present <- labels[vapply(labels, function(label) {
            grepl(paste(label, "durable fact"), prompt, fixed = TRUE)
        }, logical(1L))]
        summary <- paste(paste(present, "durable fact", collapse = "\n"),
                         "\n\n## Source conversations\n", paste(links, collapse = "\n"))
        if (startsWith(system, "Route one")) {
            jsonlite::toJSON(list(topic = "Topic", slug = "topic",
                existing_path = "wiki/topic.md", room_id = destination,
                room_name = "Topic", confidence = 1, summary = summary),
                auto_unbox = TRUE)
        } else {
            summary
        }
    }

    for (kind in c("general", "repo")) {
        for (case in c("same", "mixed", "unknown", "external", "cli", "thread")) {
            vault <- file.path(root, paste(kind, case, "vault", sep = "-"))
            inst <- file.path(root, paste(kind, case, "instance", sep = "-"))
            dir.create(inst)
            pensar::init_vault(vault, rproj = FALSE, agent_instructions = FALSE)
            local_session <- make_session("local", destination, kind)
            remote_session <- make_session("remote", "!elsewhere:example.invalid", kind)
            collected <- switch(case,
                same = list(local_session),
                mixed = list(local_session, remote_session),
                unknown = list(local_session, make_session("unknown", NULL, kind)),
                external = list(remote_session),
                cli = list(make_session("unknown", NULL, kind)),
                thread = list(make_session("thread", destination, kind, "$thread-root")))
            sent <- list()
            generated <- 0L
            collected_n <- 0L
            sync <- if (kind == "repo") cerebro::sync_repo_conversation_threads else
                cerebro::sync_general_conversation_threads
            result <- sync(vault = vault, instance_dir = inst,
                           generate_fn = generate, dry_run = FALSE)
            expect_equal(collected_n, 1L)
            expect_true(generated > 0L)
            expect_equal(length(result), 1L)
            page <- result[[1L]]$page
            page_text <- paste(readLines(page), collapse = "\n")
            for (session in collected) {
                expect_true(grepl(paste(session$label, "durable fact"),
                                  page_text, fixed = TRUE))
            }
            state_key <- if (kind == "repo") "repo_sources" else "general_sources"
            state <- cerebro::read_instance_state(inst)$conversation_threads[[state_key]]
            expect_equal(length(state), length(collected))
            expect_true(all(vapply(state, function(record) {
                identical(record$raw_hash, record$summary_hash) &&
                    file.exists(file.path(vault, record$raw_path))
            }, logical(1L))))

            if (case %in% c("same", "thread")) {
                expect_equal(length(sent), 0L)
            } else {
                expect_equal(length(sent), 1L)
                expect_equal(sent[[1L]]$room, destination)
                text <- sent[[1L]]$text
                if (case %in% c("external", "cli")) {
                    label <- collected[[1L]]$label
                    expect_true(grepl(paste(label, "durable fact"), text, fixed = TRUE))
                    expect_false(grepl("1 conversation filed", text, fixed = TRUE))
                } else {
                    expect_true(grepl("1 conversation filed", text, fixed = TRUE))
                    expect_true(grepl("Full notes:", text, fixed = TRUE))
                }
                for (session in collected) {
                    key <- cerebro:::.thread_session_fingerprint(session)
                    link <- sub("\\.md$", "", state[[key]]$raw_path)
                    expect_equal(grepl(paste0("[[", link, "]]"), text, fixed = TRUE),
                                 !identical(session$room_id, destination))
                }
                # Repo prose combines origins; none of that prose belongs
                # in a handoff assembled from only the external records.
                expect_false(grepl("local durable fact", text, fixed = TRUE))
            }

            # Suppressed sources are finished too. A second sweep must not
            # regenerate, append to the page, or attempt a send for them.
            previous_generated <- generated
            previous_sent <- length(sent)
            previous_page <- readLines(page)
            again <- sync(vault = vault, instance_dir = inst,
                          generate_fn = generate, dry_run = FALSE)
            expect_equal(length(again), 0L)
            expect_equal(generated, previous_generated)
            expect_equal(length(sent), previous_sent)
            expect_identical(readLines(page), previous_page)
        }
    }
})
