library(tinytest)

empty <- tempfile("vault-empty-")
dirty <- tempfile("vault-dirty-")
dir.create(empty, recursive = TRUE)
dir.create(file.path(dirty, "wiki"), recursive = TRUE)
writeLines(c("---",
             "title: Bad Link",
             "type: article",
             "source: test",
             "---",
             "Points at [[missing-page]]."),
           file.path(dirty, "wiki", "bad.md"))

clean <- cerebro::vault_check(c(empty = empty), notify = "never")
expect_equal(clean$total_findings, 0L)
expect_equal(clean$errors, 0L)
expect_true(grepl("clean", clean$report, fixed = TRUE))

checked <- cerebro::vault_check(c(dirty = dirty), notify = "never")
expect_true(checked$total_findings >= 2L)
expect_equal(checked$errors, 0L)
expect_true(grepl("missing-page", checked$report, fixed = TRUE))

expect_error(
    cerebro::vault_check(c(dirty = dirty), notify = "never",
                         fail_on_findings = TRUE),
    "Vault check found"
)

missing <- file.path(tempdir(), "not-a-vault")
bad <- cerebro::vault_check(c(missing = missing), notify = "never")
expect_equal(bad$errors, 1L)
expect_equal(bad$total_findings, 0L)
expect_true(grepl("ERROR", bad$report, fixed = TRUE))
expect_false(grepl("1 finding", bad$report, fixed = TRUE))

expect_error(
    cerebro::vault_check(c(empty = empty), notify = "never",
                         min_cluster_size = "many"),
    "min_cluster_size must be an integer"
)

unlink(c(empty, dirty, missing), recursive = TRUE)
