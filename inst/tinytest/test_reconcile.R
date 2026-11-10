# Tests for the reconcile capability's pure logic: section ranges,
# candidate extraction, structured-op parsing, and the guarded apply.

library(tinytest)

lines <- c(
    "# todo_260608_daily.md",   # 1
    "",                          # 2
    "## Monday",                 # 3
    "- [x] shipped monday",      # 4
    "- [ ] monday open",         # 5
    "",                          # 6
    "## Tuesday",                # 7
    "- [ ] corteza matrix work", # 8
    "  - [/] markdown subpath",  # 9
    "- [!] blocked on review",   # 10
    "- [x] already done",        # 11
    "",                          # 12
    "## Wednesday",              # 13
    "- [ ] weds thing"           # 14
)

# ---- .reconcile_section_range ----
rng <- cerebro:::.reconcile_section_range(lines, "Tuesday")
expect_equal(rng, c(8L, 12L))

expect_null(cerebro:::.reconcile_section_range(lines, "Saturday"))

# empty section: range start > end signalled as c(start, start-1)
empty <- c("## Friday", "## Monday", "- [ ] x")
er <- cerebro:::.reconcile_section_range(empty, "Friday")
expect_true(er[2L] < er[1L])

# ---- .reconcile_candidates ----
cand <- cerebro:::.reconcile_candidates(lines, rng)
# only the two open items ([ ] and [/]); blocked and done excluded
expect_equal(cand$line, c(8L, 9L))
expect_equal(cand$status, c(" ", "/"))
expect_equal(cand$text, c("corteza matrix work", "markdown subpath"))

# ---- .reconcile_parse_ops ----
reply <- paste0(
    "Here's my read:\n",
    '{"done":[8],"unclear":[{"line":9,"why":"no matching commit"}],',
    '"offlist":["fixed CI flake"]}\n',
    "Let me know if that's off.")
ops <- cerebro:::.reconcile_parse_ops(reply, valid_lines = c(8L, 9L))
expect_equal(ops$done, 8L)
expect_equal(ops$unclear[[1]]$line, 9L)
expect_equal(ops$offlist, "fixed CI flake")

# invalid line numbers are dropped against valid_lines
ops2 <- cerebro:::.reconcile_parse_ops('{"done":[8,99],"offlist":[]}',
                                       valid_lines = c(8L, 9L))
expect_equal(ops2$done, 8L)

# a reply with no JSON yields NULL (caller leaves the list untouched)
expect_null(cerebro:::.reconcile_parse_ops("no json here", c(8L, 9L)))

# ---- .reconcile_apply ----
ops3 <- list(done = 8L, unclear = list(), offlist = "fixed CI flake")
applied <- cerebro:::.reconcile_apply(lines, rng, cand, ops3)
expect_true(applied$changed)
# line 8 flipped to done
expect_equal(applied$lines[8], "- [x] corteza matrix work")
# line 9 (in-progress, not marked done) untouched
expect_equal(applied$lines[9], "  - [/] markdown subpath")
# blocked line never altered
expect_true(any(grepl("\\[!\\] blocked on review", applied$lines)))
# off-list item appended inside the section
expect_true(any(grepl("fixed CI flake", applied$lines)))

# a done id that isn't a candidate (e.g. the blocked line 10) is ignored
ops4 <- list(done = c(8L, 10L), unclear = list(), offlist = character())
applied2 <- cerebro:::.reconcile_apply(lines, rng, cand, ops4)
expect_equal(applied2$lines[10], "- [!] blocked on review")
expect_equal(applied2$done_lines, 8L)

# ---- .reconcile_summary ----
sm <- cerebro:::.reconcile_summary(applied, cand, ops3, "Tuesday")
expect_true(grepl("Marked done", sm))
expect_true(grepl("corteza matrix work", sm))
expect_true(grepl("Off-list", sm))
