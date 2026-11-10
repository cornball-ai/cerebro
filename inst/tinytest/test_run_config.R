library(tinytest)

cmd <- paste(shQuote(file.path(R.home("bin"), "Rscript")),
             "-e", shQuote("quit(status = 7L)"))
expect_error(cerebro:::.run_shell_command(cmd),
             "shell command failed with status 7")

expect_error(
    cerebro:::.build_routine_fn(
        list(name = "bad-vault", capability = "vault_check",
             min_cluster_size = "many"),
        list(), list(), tempdir()
    ),
    "min_cluster_size must be an integer"
)

expect_error(
    cerebro:::.build_routine_fn(
        list(name = "bad-sync", capability = "sync",
             source_root = tempdir()),
        list(), list(), tempdir()
    ),
    "requires source_path and target_path"
)

# Matrix loop gate: explicit arg wins, else read matrix.enabled from cfg.
expect_false(cerebro:::.matrix_enabled(NULL, list()))
expect_false(cerebro:::.matrix_enabled(NULL, list(matrix = list(enabled = FALSE))))
expect_true(cerebro:::.matrix_enabled(NULL, list(matrix = list(enabled = TRUE))))
expect_true(cerebro:::.matrix_enabled(TRUE, list()))
expect_false(cerebro:::.matrix_enabled(FALSE, list(matrix = list(enabled = TRUE))))

# run() exposes the matrix toggle.
expect_true("matrix" %in% names(formals(cerebro::run)))

# Matrix poll timeout: config matrix.timeout_ms wins, else tick * 1000.
expect_equal(cerebro:::.matrix_timeout(list(), 30L), 30000L)
expect_equal(cerebro:::.matrix_timeout(list(timeout_ms = 5000), 30L), 5000L)
expect_equal(cerebro:::.matrix_timeout(list(timeout_ms = 0), 30L), 0L)
expect_error(
    cerebro:::.matrix_timeout(list(timeout_ms = "soon"), 30L),
    "timeout_ms must be an integer"
)

# .load_renviron: loads the file, overriding inherited values; missing
# file or NULL is a quiet no-op.
tmp_renviron <- tempfile()
writeLines("CEREBRO_TEST_RENVIRON=from_file", tmp_renviron)
Sys.setenv(CEREBRO_TEST_RENVIRON = "from_env")
expect_true(suppressMessages(cerebro:::.load_renviron(tmp_renviron)))
expect_equal(Sys.getenv("CEREBRO_TEST_RENVIRON"), "from_file")
Sys.unsetenv("CEREBRO_TEST_RENVIRON")
file.remove(tmp_renviron)
expect_false(cerebro:::.load_renviron(tmp_renviron))
expect_false(cerebro:::.load_renviron(NULL))

# run() exposes the renviron opt-out.
expect_true("renviron" %in% names(formals(cerebro::run)))

# .resolve_renviron: config key wins when the caller didn't pass one;
# false disables; anything else is an error.
expect_equal(cerebro:::.resolve_renviron(NULL, "~/.Renviron"), "~/.Renviron")
expect_null(cerebro:::.resolve_renviron(FALSE, "~/.Renviron"))
expect_equal(cerebro:::.resolve_renviron("/keys.env", "~/.Renviron"),
             "/keys.env")
expect_error(cerebro:::.resolve_renviron(5, "~/.Renviron"),
             "single path or false")

# .default_renviron honors R_ENVIRON_USER only when non-empty. littler
# sets it to "" to suppress R's own startup files, so empty must fall
# through to ~/.Renviron rather than silently disable loading.
old_reu <- Sys.getenv("R_ENVIRON_USER", unset = NA)
Sys.setenv(R_ENVIRON_USER = "/custom/Renviron")
expect_equal(cerebro:::.default_renviron(), "/custom/Renviron")
Sys.setenv(R_ENVIRON_USER = "")
expect_equal(cerebro:::.default_renviron(), "~/.Renviron")
Sys.unsetenv("R_ENVIRON_USER")
expect_equal(cerebro:::.default_renviron(), "~/.Renviron")
if (!is.na(old_reu)) Sys.setenv(R_ENVIRON_USER = old_reu)

# YAML routines can be left in config but disabled explicitly.
idir <- file.path(tempdir(), paste0("cerebro-disabled-", Sys.getpid()))
dir.create(idir, recursive = TRUE, showWarnings = FALSE)
writeLines(c(
    "routines:",
    "  - name: disabled",
    "    enabled: false",
    "    schedule: hourly",
    "    capability: r",
    "    expr: \"stop('should not register')\"",
    "  - name: active",
    "    schedule: hourly",
    "    capability: r",
    "    expr: \"NULL\""
), file.path(idir, "config.yaml"))
cerebro:::.clear_routines()
n <- suppressMessages(cerebro::register_from_config(idir))
expect_equal(n, 1L)
expect_equal(cerebro::list_routines()$name, "active")
cerebro:::.clear_routines()
unlink(idir, recursive = TRUE)

# Routines state path is instance-scoped when given an instance name.
expect_equal(basename(cerebro::routines_state_path()), "routines.json")
expect_equal(basename(cerebro::routines_state_path("mybot")), "routines-mybot.json")

# Shell timeout kills a wedged command instead of blocking forever.
expect_error(cerebro:::.run_shell_command("sleep 5", timeout = 1),
             "status 124")

# Invalid shell timeout fails loudly at registration, not at fire time.
idir <- file.path(tempdir(), paste0("cerebro-badtmo-", Sys.getpid()))
dir.create(idir, recursive = TRUE, showWarnings = FALSE)
writeLines(c(
    "routines:",
    "  - name: bad_timeout",
    "    schedule: hourly",
    "    capability: shell",
    "    command: \"true\"",
    "    timeout: soon",
    "  - name: good_timeout",
    "    schedule: hourly",
    "    capability: shell",
    "    command: \"true\"",
    "    timeout: 5"
), file.path(idir, "config.yaml"))
cerebro:::.clear_routines()
n <- suppressMessages(cerebro::register_from_config(idir))
expect_equal(n, 1L)
expect_equal(cerebro::list_routines()$name, "good_timeout")
cerebro:::.clear_routines()
unlink(idir, recursive = TRUE)
