library(tinytest)

cerebro:::.clear_routines()
on.exit({
    cerebro:::.clear_routines()
    Sys.unsetenv("CEREBRO_STATE_DIR")
}, add = TRUE)

dir <- tempfile("cerebro-state-")
Sys.setenv(CEREBRO_STATE_DIR = dir)
on.exit(unlink(dir, recursive = TRUE), add = TRUE)

# routines_state_path honors env override
expect_equal(cerebro::routines_state_path(),
             file.path(dir, "routines.json"))

# save then load: last_fired round-trips through JSON
cerebro::add_routine("a", "hourly", function() NULL)
cerebro::add_routine("b", "Mon 09:00", function() NULL)
ts <- as.numeric(as.POSIXct("2026-04-25 14:00:00"))
cerebro:::.set_last_fired("a", ts)
cerebro:::save_routines_state(cerebro::routines_state_path())
expect_true(file.exists(cerebro::routines_state_path()))

# Wipe in-memory last_fired, reload from disk.
cerebro:::.set_last_fired("a", NA_real_)
n <- cerebro:::load_routines_state(cerebro::routines_state_path())
expect_equal(n, 1L)  # only "a" had a saved last_fired
expect_equal(cerebro:::.get_last_fired("a"), ts)
expect_true(is.na(cerebro:::.get_last_fired("b")))

# load_routines_state ignores entries for routines no longer registered
cerebro::remove_routine("a")
n <- cerebro:::load_routines_state(cerebro::routines_state_path())
expect_equal(n, 0L)

# load_routines_state on missing file is a no-op
file.remove(cerebro::routines_state_path())
expect_equal(cerebro:::load_routines_state(cerebro::routines_state_path()),
             0L)

cerebro:::.clear_routines()
