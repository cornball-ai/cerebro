library(tinytest)

# run() returns on an interrupt. A container's stop signal arrives as
# SIGINT (R drops SIGTERM as PID 1), and the loop has to turn it into a
# normal return so the process exits cleanly instead of waiting out the
# runtime's SIGKILL timeout. The interrupt is raised as a condition from
# a stubbed tick rather than as a real signal: littler leaves R's SIGINT
# handler uninstalled, so a real one would kill a test session run under
# `r`, which is the documented local workflow.
local({
    inst <- tempfile("cerebro-stop-")
    dir.create(inst)
    tick_orig <- get("tick_routines", envir = asNamespace("cerebro"))
    op <- options(cerebro.instance_dir = getOption("cerebro.instance_dir"))
    env_orig <- Sys.getenv("CEREBRO_INSTANCE_DIR", unset = NA)
    on.exit({
        assignInNamespace("tick_routines", tick_orig, "cerebro")
        options(op)
        if (is.na(env_orig)) {
            Sys.unsetenv("CEREBRO_INSTANCE_DIR")
        } else {
            Sys.setenv(CEREBRO_INSTANCE_DIR = env_orig)
        }
        unlink(inst, recursive = TRUE)
    }, add = TRUE)

    ticks <- 0L
    assignInNamespace("tick_routines", function(...) {
        ticks <<- ticks + 1L
        signalCondition(structure(list(message = "stop", call = NULL),
                                  class = c("interrupt", "condition")))
        invisible(NULL)
    }, "cerebro")

    msgs <- character()
    started <- proc.time()[["elapsed"]]
    got <- withCallingHandlers(
        cerebro::run(inst, tick_seconds = 30L, state_path = NULL,
                     matrix = FALSE, renviron = NULL),
        message = function(m) {
            msgs <<- c(msgs, conditionMessage(m))
            invokeRestart("muffleMessage")
        })
    elapsed <- proc.time()[["elapsed"]] - started

    expect_null(got)
    # One tick, then out: the interrupt is not an error, so the tick's
    # own error handler lets it through to the loop's handler.
    expect_equal(ticks, 1L)
    expect_true(any(grepl("interrupted; stopping", msgs, fixed = TRUE)))
    # The 30 s sleep after the tick never ran.
    expect_true(elapsed < 10)
})
