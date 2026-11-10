library(tinytest)

idir <- tempfile("cerebro-prepared-")
dir.create(idir)

date <- as.Date("2026-06-24")
state <- list(
    prepared_briefings = list(
        gold = list(
            date = as.character(date),
            prepared_at = "2026-06-24T03:30:00-0500",
            text = "Three ideas and a fortune"
        ),
        stale = list(
            date = "2026-06-23",
            prepared_at = "2026-06-23T03:30:00-0500",
            text = "Old draft"
        )
    )
)
cerebro::write_instance_state(state, idir)

out <- capture.output(
    text <- cerebro::send_prepared_briefing(
        "gold", room = "To Do", dry_run = TRUE,
        instance_dir = idir, date = date
    )
)
expect_identical(text, "Three ideas and a fortune")
expect_true(any(grepl("Three ideas and a fortune", out, fixed = TRUE)))

stale <- cerebro::send_prepared_briefing(
    "stale", room = "To Do", dry_run = TRUE,
    instance_dir = idir, date = date
)
expect_null(stale)

# An existing same-day draft skips preparation before invoking the model.
skipped <- cerebro::prepare_briefing(
    "gold", system_prompt = "unused", model = "m", instance_dir = idir,
    date = date
)
expect_null(skipped)

briefings <- list(gold = list(
    room = "To Do",
    system_prompt = "prompt",
    model = "m",
    sources = list()
))
prepare_fn <- cerebro:::.build_routine_fn(
    list(name = "gold-prepare", capability = "briefing_prepare",
         briefing = "gold"),
    briefings, list(), idir
)
send_fn <- cerebro:::.build_routine_fn(
    list(name = "gold-send", capability = "briefing_send",
         briefing = "gold"),
    briefings, list(), idir
)
expect_true(is.function(prepare_fn))
expect_true(is.function(send_fn))

unlink(idir, recursive = TRUE)
