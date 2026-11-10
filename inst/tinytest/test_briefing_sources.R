library(tinytest)

idir <- file.path(tempdir(),
                  paste0("cerebro-instance-", Sys.getpid(), "-",
                         sample.int(1e6, 1L)))
for (d in c(file.path(idir, "todo", "this_week"),
            file.path(idir, "todo", "archive"),
            file.path(idir, "wiki", "wiki")))
{
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    expect_true(dir.exists(d))
}

writeLines(c(
    "# Monday",
    "[/] - source default task",
    "# Tuesday",
    "[ ] - later task"
), file.path(idir, "todo", "this_week", "ToDo_260504_Daily.md"))

ctx <- new.env(parent = emptyenv())
ctx$date <- as.Date("2026-05-04")
ctx$instance_dir <- idir

out <- cerebro::briefing_source_hacer_todos("daily", ctx, list())
expect_true("TODAY'S TODO SECTION" %in% names(out))
expect_true(grepl("source default task",
                  out[["TODAY'S TODO SECTION"]],
                  fixed = TRUE))

writeLines(c("one", "two"),
           file.path(idir, "wiki", "log.md"))
writeLines("---\ntitle: Note\n---",
           file.path(idir, "wiki", "wiki", "note.md"))

out <- cerebro::briefing_source_wiki("daily", ctx,
                                     list(tail_lines = 1L,
                                          recent_days = 1L))
expect_equal(out[["WIKI LOG (tail)"]], "two")
expect_true(grepl("note.md",
                  out[["WIKI PAGES TOUCHED THIS WEEK"]],
                  fixed = TRUE))

# Recent vault excerpts are bounded and include both raw and wiki material.
vault <- file.path(idir, "vault")
dir.create(file.path(vault, "raw", "repos"), recursive = TRUE)
dir.create(file.path(vault, "wiki"), recursive = TRUE)
writeLines(c("---", "title: Repo", "---", "repo evidence"),
           file.path(vault, "raw", "repos", "demo.md"))
writeLines("synthesis page", file.path(vault, "wiki", "idea.md"))
dir.create(file.path(vault, "raw", "matrix"), recursive = TRUE)
writeLines(c("---", "title: Chat", "---", "matrix evidence"),
           file.path(vault, "raw", "matrix", "chat.md"))
vault_files <- c(
    file.path(vault, "raw", "repos", "demo.md"),
    file.path(vault, "wiki", "idea.md"),
    file.path(vault, "raw", "matrix", "chat.md")
)
Sys.setFileTime(
    vault_files,
    as.POSIXct("2026-05-04 12:00:00", tz = Sys.timezone())
)
out <- cerebro::briefing_source_vault_recent(
    "daily", ctx,
    list(vault = vault, days = 1L, max_chars = 100L,
         exclude_groups = "matrix")
)
expect_true(grepl("raw/repos/demo.md", out[["RECENT VAULT CONTENT"]],
                  fixed = TRUE))
expect_true(grepl("repo evidence", out[["RECENT VAULT CONTENT"]],
                  fixed = TRUE))
expect_true(grepl("wiki/idea.md", out[["RECENT VAULT CONTENT"]],
                  fixed = TRUE))
expect_false(grepl("raw/matrix/chat.md", out[["RECENT VAULT CONTENT"]],
                   fixed = TRUE))

# Todo-independent Git activity can run against explicitly supplied repos.
repo <- file.path(idir, "repo")
dir.create(repo)
system2("git", c("-C", repo, "init", "-q"))
writeLines("x", file.path(repo, "file.txt"))
system2("git", c("-C", repo, "add", "file.txt"))
system2("git", c("-C", repo, "-c", "user.name=Test",
                 "-c", "user.email=test@example.com", "commit", "-qm",
                 "Evidence"))
out <- cerebro::briefing_source_git_recent(
    "daily", ctx,
    list(discover = FALSE, days = 1L,
         extra_repos = list(demo = repo))
)
expect_true(grepl("demo (1 commit(s) in window)",
                  out[["RECENT GIT ACTIVITY"]], fixed = TRUE))
expect_true(grepl("Evidence", out[["RECENT GIT ACTIVITY"]],
                  fixed = TRUE))

if (requireNamespace("fortunes", quietly = TRUE)) {
    ctx$git_recent_text <- "software code model data"
    ctx$vault_recent_text <- "programming language design"
    ctx$chat_recent_text <- "conversation architecture memory"
    out <- cerebro::briefing_source_fortune_candidates(
        "daily", ctx, list(max_candidates = 3L)
    )
    expect_true("FORTUNE CANDIDATES" %in% names(out))
    expect_true(grepl("[1]", out[["FORTUNE CANDIDATES"]], fixed = TRUE))
}

unlink(idir, recursive = TRUE)
