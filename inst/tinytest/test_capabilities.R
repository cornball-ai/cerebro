library(tinytest)

# Every registered capability builds a routine function from a
# representative entry, and the representative set covers the registry,
# so a new capability has to bring an entry here.
caps <- cerebro:::.capabilities()
expect_true(length(caps) > 0L)
expect_true(all(vapply(caps, is.function, logical(1L))))

briefings <- list(gold = list(room = "lab", system_prompt = "p",
                              model = "m"))
entries <- list(
    briefing = list(name = "b", capability = "briefing", briefing = "gold"),
    briefing_prepare = list(name = "bp", capability = "briefing_prepare",
                            briefing = "gold"),
    briefing_send = list(name = "bs", capability = "briefing_send",
                         briefing = "gold"),
    reconcile = list(name = "rec", capability = "reconcile",
                     briefing = "gold"),
    sync = list(name = "sy", capability = "sync", source_root = tempdir(),
                source_path = "a", target_path = "b"),
    matrix_archive = list(name = "ma", capability = "matrix_archive"),
    lifecycle_sweep = list(name = "ls", capability = "lifecycle_sweep",
                           model = "m"),
    vault_check = list(name = "vc", capability = "vault_check"),
    shell = list(name = "sh", capability = "shell", command = "true"),
    r = list(name = "r", capability = "r", expr = "NULL")
)
expect_equal(sort(names(entries)), sort(names(caps)))
for (nm in names(caps)) {
    fn <- cerebro:::.build_routine_fn(entries[[nm]], briefings, list(),
                                      tempdir())
    expect_true(is.function(fn), info = nm)
    expect_equal(length(formals(fn)), 0L, info = nm)
}

# cerebro picks no default model: a capability that calls one stops
# registration when its config names none, and says which entry.
no_model <- list(gold = list(room = "lab", system_prompt = "p"))
for (nm in c("briefing", "briefing_prepare", "reconcile")) {
    expect_error(cerebro:::.build_routine_fn(entries[[nm]], no_model, list(),
                                             tempdir()),
                 "briefing 'gold'.* needs a `model`", info = nm)
}
expect_error(cerebro:::.build_routine_fn(
    list(name = "ls", capability = "lifecycle_sweep"), list(), list(),
    tempdir()), "routine ls needs a `model`")
# briefing_send only delivers a stored draft, so it needs none.
expect_true(is.function(cerebro:::.build_routine_fn(
    entries$briefing_send, no_model, list(), tempdir())))

# The exported functions fail the same way, before any work.
expect_error(cerebro::briefing("b", system_prompt = "p", dry_run = TRUE),
             "`model` is required")
expect_error(cerebro::prepare_briefing("b", system_prompt = "p"),
             "`model` is required")
expect_error(cerebro::reconcile_todo(root = tempdir(), system_prompt = "p"),
             "`model` is required")
expect_error(cerebro::sweep_segments(client = NULL, model = ""),
             "`model` is required")

# An unknown or missing capability is a registration error that names
# the valid set.
err <- tryCatch(
    cerebro:::.build_routine_fn(list(name = "x", capability = "nope"),
                                list(), list(), tempdir()),
    error = identity)
expect_inherits(err, "error")
expect_true(grepl("unknown capability 'nope' for routine x",
                  conditionMessage(err), fixed = TRUE))
for (nm in names(caps)) {
    expect_true(grepl(nm, conditionMessage(err), fixed = TRUE), info = nm)
}
expect_error(
    cerebro:::.build_routine_fn(list(name = "x"), list(), list(), tempdir()),
    "routine x has no capability")

# A builder that declines (shell without a command) returns NULL with a
# message, and the entry is skipped rather than failing registration.
expect_null(suppressMessages(cerebro:::.build_routine_fn(
    list(name = "sh", capability = "shell"), list(), list(), tempdir())))

# pre: runs before the capability, whichever capability; a failing pre
# is reported and the capability still runs; a list runs in order.
Sys.unsetenv("CEREBRO_TEST_PRE")
fn <- cerebro:::.build_routine_fn(
    list(name = "with-pre", capability = "r",
         pre = "Sys.setenv(CEREBRO_TEST_PRE = 'ran')",
         expr = "Sys.getenv('CEREBRO_TEST_PRE')"),
    list(), list(), tempdir())
expect_equal(fn(), "ran")

fn <- cerebro:::.build_routine_fn(
    list(name = "bad-pre", capability = "r", pre = "stop('boom')",
         expr = "'still ran'"),
    list(), list(), tempdir())
msgs <- character()
out <- withCallingHandlers(fn(), message = function(m) {
    msgs <<- c(msgs, conditionMessage(m))
    invokeRestart("muffleMessage")
})
expect_equal(out, "still ran")
expect_true(any(grepl("pre hook for bad-pre failed: boom", msgs,
                      fixed = TRUE)))

fn <- cerebro:::.build_routine_fn(
    list(name = "pre-list", capability = "r",
         pre = list("Sys.setenv(CEREBRO_TEST_PRE = 'one')",
                    "Sys.setenv(CEREBRO_TEST_PRE = paste0(Sys.getenv('CEREBRO_TEST_PRE'), 'two'))"),
         expr = "Sys.getenv('CEREBRO_TEST_PRE')"),
    list(), list(), tempdir())
expect_equal(fn(), "onetwo")
Sys.unsetenv("CEREBRO_TEST_PRE")

# register_from_config() surfaces the error for a typo in config.yaml.
idir <- file.path(tempdir(), paste0("cerebro-typo-", Sys.getpid()))
dir.create(idir, recursive = TRUE, showWarnings = FALSE)
writeLines(c(
    "routines:",
    "  - name: typo",
    "    schedule: hourly",
    "    capability: breifing"
), file.path(idir, "config.yaml"))
cerebro:::.clear_routines()
expect_error(suppressMessages(cerebro::register_from_config(idir)),
             "unknown capability 'breifing'")
expect_equal(nrow(cerebro::list_routines()), 0L)
cerebro:::.clear_routines()
unlink(idir, recursive = TRUE)
