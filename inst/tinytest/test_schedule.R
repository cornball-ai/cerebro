library(tinytest)

ps <- cerebro:::parse_schedule
nf <- cerebro:::next_fire

# parse_schedule: hourly
expect_equal(ps("hourly"), list(type = "hourly"))

# parse_schedule: daily HH:MM expands to all 7 days
s <- ps("daily 05:30")
expect_equal(s$type, "time")
expect_equal(s$days, 1:7)
expect_equal(s$hour, 5L)
expect_equal(s$minute, 30L)

# parse_schedule: single day
s <- ps("Sun 20:00")
expect_equal(s$days, 7L)
expect_equal(s$hour, 20L)
expect_equal(s$minute, 0L)

# parse_schedule: day range
s <- ps("Mon-Sat 05:30")
expect_equal(s$days, 1:6)

# parse_schedule: invalid
expect_error(ps("bogus"))
expect_error(ps("Tue 25:00"))
expect_error(ps("Tue 12:99"))
expect_error(ps("Notaday 12:00"))
expect_error(ps("Mon-Notaday 12:00"))
expect_error(ps("Mon-Tue-Wed 12:00"))

# next_fire: hourly rounds to next hour
t <- as.POSIXct("2026-04-25 14:23:17")
expect_equal(nf(ps("hourly"), after = t),
             as.POSIXct("2026-04-25 15:00:00"))

# next_fire: hourly when at exactly :00 advances
t <- as.POSIXct("2026-04-25 14:00:00")
expect_equal(nf(ps("hourly"), after = t),
             as.POSIXct("2026-04-25 15:00:00"))

# next_fire: daily HH:MM later today if before, else tomorrow
t <- as.POSIXct("2026-04-25 04:00:00")  # Sat 04:00
expect_equal(nf(ps("daily 05:30"), after = t),
             as.POSIXct("2026-04-25 05:30:00"))
t <- as.POSIXct("2026-04-25 06:00:00")  # past 05:30 today
expect_equal(nf(ps("daily 05:30"), after = t),
             as.POSIXct("2026-04-26 05:30:00"))

# next_fire: single day skips to that weekday
# 2026-04-25 is a Saturday. Sun = next day.
t <- as.POSIXct("2026-04-25 10:00:00")
expect_equal(nf(ps("Sun 20:00"), after = t),
             as.POSIXct("2026-04-26 20:00:00"))

# next_fire: day range honors range membership
# Sat 06:00, Mon-Sat 05:30 should skip to Mon (Sat 05:30 has passed)
t <- as.POSIXct("2026-04-25 06:00:00")  # Saturday
expect_equal(nf(ps("Mon-Sat 05:30"), after = t),
             as.POSIXct("2026-04-27 05:30:00"))  # Monday

# Sunday 10:00 with Mon-Sat -> next Monday
t <- as.POSIXct("2026-04-26 10:00:00")  # Sunday
expect_equal(nf(ps("Mon-Sat 05:30"), after = t),
             as.POSIXct("2026-04-27 05:30:00"))

# ---- cron: five-field grammar ---------------------------------------
# Same grammar as Kubernetes CronJob / Vixie cron. The named forms
# above cannot express a sub-hourly cadence; this is the reason it
# exists.

s <- ps("*/30 * * * *")
expect_equal(s$type, "cron")
expect_equal(s$minute, c(0L, 30L))
expect_equal(s$hour, 0:23)
expect_equal(s$dom, 1:31)
expect_equal(s$month, 1:12)
expect_equal(s$dow, 1:7)
expect_true(s$dom_any)
expect_true(s$dow_any)

# lists, ranges, steps on ranges, names, and Sunday as 0 or 7
s <- ps("0,30 9-17 * * Mon-Fri")
expect_equal(s$minute, c(0L, 30L))
expect_equal(s$hour, 9:17)
expect_equal(s$dow, 1:5)
expect_false(s$dow_any)
expect_equal(ps("5 4 1 * *")$dom, 1L)
expect_equal(ps("0 0 * Jan,Jul *")$month, c(1L, 7L))
expect_equal(ps("0 0 * jan-mar *")$month, 1:3)
expect_equal(ps("0 0 * * 0")$dow, 7L)
expect_equal(ps("0 0 * * 7")$dow, 7L)
expect_equal(ps("0 0 * * Sun")$dow, 7L)
expect_equal(ps("0 0 * * 0,3")$dow, c(3L, 7L))
expect_equal(ps("10-40/10 * * * *")$minute, c(10L, 20L, 30L, 40L))
expect_equal(ps("15/20 * * * *")$minute, c(15L, 35L, 55L))   # n/step = from n

# invalid fields are refused with the field named, not accepted as
# something else
expect_error(ps("60 * * * *"), pattern = "Bad cron field")
expect_error(ps("* 24 * * *"), pattern = "Bad cron field")
expect_error(ps("* * 0 * *"), pattern = "Bad cron field")
expect_error(ps("* * * 13 *"), pattern = "Bad cron field")
expect_error(ps("* * * * 8"), pattern = "Bad cron field")
expect_error(ps("* * * * Funday"), pattern = "Bad cron field")
expect_error(ps("*/0 * * * *"), pattern = "Bad cron field")
expect_error(ps("5-3 * * * *"), pattern = "Bad cron field")
expect_error(ps("* * * *"))            # four fields is neither grammar
expect_error(ps("* * * * * *"))        # six is not either

# next_fire: every half hour, from a mid-interval instant
t <- as.POSIXct("2026-04-25 14:23:17")
expect_equal(nf(ps("*/30 * * * *"), after = t),
             as.POSIXct("2026-04-25 14:30:00"))
# exactly on a boundary advances (strictly greater than `after`)
t <- as.POSIXct("2026-04-25 14:30:00")
expect_equal(nf(ps("*/30 * * * *"), after = t),
             as.POSIXct("2026-04-25 15:00:00"))
# 59 seconds past a boundary also advances a full interval
t <- as.POSIXct("2026-04-25 14:30:59")
expect_equal(nf(ps("*/30 * * * *"), after = t),
             as.POSIXct("2026-04-25 15:00:00"))

# next_fire: hour restriction skips forward by hours, then days
t <- as.POSIXct("2026-04-25 18:10:00")   # Saturday evening
expect_equal(nf(ps("0,30 9-17 * * Mon-Fri"), after = t),
             as.POSIXct("2026-04-27 09:00:00"))   # Monday 09:00
t <- as.POSIXct("2026-04-27 09:00:00")   # Monday, on the boundary
expect_equal(nf(ps("0,30 9-17 * * Mon-Fri"), after = t),
             as.POSIXct("2026-04-27 09:30:00"))

# next_fire: cron equivalents agree with the named forms
t <- as.POSIXct("2026-04-25 14:23:17")
expect_equal(nf(ps("0 * * * *"), after = t), nf(ps("hourly"), after = t))
expect_equal(nf(ps("30 5 * * *"), after = t), nf(ps("daily 05:30"), after = t))
expect_equal(nf(ps("0 20 * * Sun"), after = t), nf(ps("Sun 20:00"), after = t))
expect_equal(nf(ps("30 5 * * Mon-Sat"), after = t),
             nf(ps("Mon-Sat 05:30"), after = t))

# next_fire: day-of-month, and month rollover
t <- as.POSIXct("2026-04-25 10:00:00")
expect_equal(nf(ps("5 4 1 * *"), after = t),
             as.POSIXct("2026-05-01 04:05:00"))
# 31st only exists in some months: April has none, so May 31
expect_equal(nf(ps("0 12 31 * *"), after = t),
             as.POSIXct("2026-05-31 12:00:00"))

# next_fire: Vixie day rule -- dom AND dow both restricted means
# either matches. 2026-04-25 is a Saturday; "1st or any Monday":
# Monday the 27th comes before May 1st.
t <- as.POSIXct("2026-04-25 10:00:00")
expect_equal(nf(ps("0 9 1 * Mon"), after = t),
             as.POSIXct("2026-04-27 09:00:00"))
# ...and when the 1st comes first, the 1st wins
t <- as.POSIXct("2026-04-30 10:00:00")   # Thursday
expect_equal(nf(ps("0 9 1 * Mon"), after = t),
             as.POSIXct("2026-05-01 09:00:00"))
