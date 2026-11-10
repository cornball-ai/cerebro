# Tests for the sync capability: week-file resolution, summary, and a
# dry-run that writes nothing. The merge itself is covered in hacer.

library(tinytest)

expect_error(
    cerebro::sync_todo(source_root = tempdir(), target_root = tempdir(),
                       dry_run = TRUE),
    "source_path must be a non-empty character scalar"
)

# ---- .sync_week_file picks latest <= date ----
root <- tempfile()
dir.create(file.path(root, "this_week"), recursive = TRUE)
tw <- file.path(root, "this_week")
for (f in c("todo_260601_week.md", "todo_260608_week.md",
            "todo_260615_week.md")) {
    writeLines("# wk", file.path(tw, f))
}
got <- cerebro:::.sync_week_file(tw, as.Date("2026-06-10"))
expect_equal(basename(got), "todo_260608_week.md")  # 06-15 is in the future
expect_null(cerebro:::.sync_week_file(tw, as.Date("2026-05-01")))

# ---- .sync_summary strips the target prefix and labels sections ----
plan <- list(added = c("cornball.ai > NewThing"),
             restatused = c("cornball.ai > OSS"))
sm <- cerebro:::.sync_summary(plan, "cornball.ai")
expect_true(grepl("Added", sm))
expect_true(grepl("- NewThing", sm))
expect_true(grepl("Status updated", sm))
expect_true(grepl("- OSS", sm))

if (requireNamespace("hacer", quietly = TRUE)) {
    # ---- sync_todo(dry_run = TRUE) writes nothing, returns a plan ----
    src_root <- file.path(tempfile(), "this_week")
    tgt_root <- file.path(tempfile(), "this_week")
    dir.create(src_root, recursive = TRUE)
    dir.create(tgt_root, recursive = TRUE)
    writeLines(c("# wk", "", "- [ ] Owner", "  - [ ] cornball.ai",
                 "    - [x] OSS", "    - [ ] NewThing"),
               file.path(src_root, "todo_260608_week.md"))
    tgt_file <- file.path(tgt_root, "todo_260608_week.md")
    writeLines(c("# wk", "", "- [ ] cornball.ai", "  - [ ] OSS"), tgt_file)
    before <- readLines(tgt_file)

    plan <- cerebro::sync_todo(
        name = "t", source_repo = NULL,
        source_root = src_root, target_root = tgt_root,
        source_path = "Owner > cornball.ai", target_path = "cornball.ai",
        pull = FALSE, dry_run = TRUE, date = as.Date("2026-06-09"))

    expect_equal(readLines(tgt_file), before)               # nothing written
    expect_true("cornball.ai > NewThing" %in% plan$added)   # plan computed
    expect_true("cornball.ai > OSS" %in% plan$restatused)   # [ ] -> [x]
}
