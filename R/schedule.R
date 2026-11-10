# Schedule grammar for cerebro::add_routine().
#
# Supported forms:
#   "hourly"           -- every hour at :00
#   "daily HH:MM"      -- every day at HH:MM
#   "DAY HH:MM"        -- single day Mon|Tue|Wed|Thu|Fri|Sat|Sun
#   "DAY1-DAY2 HH:MM"  -- day range, e.g. "Mon-Sat 05:30"
#   "M H DOM MON DOW"  -- five-field cron, the grammar Kubernetes
#                         CronJob and Vixie cron use: each field is
#                         "*", a value, a list "a,b", a range "a-b",
#                         or a step "*/n" / "a-b/n". Months and
#                         weekdays accept names (Jan, Mon). DOW is
#                         0-7 with both 0 and 7 meaning Sunday, or
#                         Mon..Sun. E.g. "*/30 * * * *" (every half
#                         hour), "0,30 9-17 * * Mon-Fri".
#
# The named forms predate the cron one and stay because config.yaml
# routines use them; anything they express, cron expresses too.
#
# All times are local (Sys.time()). Schedules return a list:
#   list(type = "hourly")
#   list(type = "time", days = integer(7), hour = h, minute = m)
#   list(type = "cron", minute = , hour = , dom = , month = , dow = ,
#        dom_any = , dow_any = )
# where days/dow are 1-7 sets with 1=Mon, 7=Sun, and the cron
# integer vectors are the allowed values of each field. dom_any /
# dow_any record whether the field was "*": cron matches a day when
# BOTH restricted fields match, or EITHER does if only one is
# restricted, Vixie's rule, and that needs the wildcard remembered
# rather than expanded.

WEEKDAYS <- c(Mon = 1L, Tue = 2L, Wed = 3L, Thu = 4L, Fri = 5L, Sat = 6L,
              Sun = 7L)
MONTHS <- c(Jan = 1L, Feb = 2L, Mar = 3L, Apr = 4L, May = 5L, Jun = 6L,
            Jul = 7L, Aug = 8L, Sep = 9L, Oct = 10L, Nov = 11L, Dec = 12L)

parse_schedule <- function(spec) {
    spec <- trimws(spec)
    if (identical(spec, "hourly")) {
        return(list(type = "hourly"))
    }
    parts <- strsplit(spec, "\\s+")[[1L]]
    if (length(parts) == 5L) {
        return(parse_cron(parts, spec))
    }
    if (length(parts) != 2L) {
        stop("Unrecognized schedule: ", shQuote(spec), call. = FALSE)
    }
    days <- parse_days(parts[1L], spec)
    hm <- parse_hm(parts[2L], spec)
    list(type = "time", days = days, hour = hm[1L], minute = hm[2L])
}

# Five-field cron. Each field independently expands to the set of
# integers it allows; nothing here is time-aware except the
# day-of-week normalisation (Sunday is 0 or 7 in cron, 7 here).
parse_cron <- function(parts, spec) {
    names_dow <- c(WEEKDAYS, Sun = 0L) # accept 0 for Sunday by name too
    minute <- parse_cron_field(parts[1L], 0L, 59L, spec = spec)
    hour <- parse_cron_field(parts[2L], 0L, 23L, spec = spec)
    dom <- parse_cron_field(parts[3L], 1L, 31L, spec = spec)
    month <- parse_cron_field(parts[4L], 1L, 12L, names = MONTHS, spec = spec)
    dow <- parse_cron_field(parts[5L], 0L, 7L, names = names_dow, spec = spec)
    dow[dow == 0L] <- 7L
    list(type = "cron", minute = minute, hour = hour, dom = dom,
         month = month, dow = sort(unique(dow)),
         dom_any = identical(parts[3L], "*"),
         dow_any = identical(parts[5L], "*"))
}

# One cron field -> sorted integer set within [lo, hi]. Lists split on
# ",", each item is "*", "n", "a-b", optionally "/step". Names (when
# given) are matched case-insensitively by their first three letters.
parse_cron_field <- function(field, lo, hi, names = NULL, spec) {
    bad <- function() {
        stop("Bad cron field ", shQuote(field), " in schedule: ",
             shQuote(spec), call. = FALSE)
    }
    tok <- function(x) {
        if (grepl("^[0-9]+$", x)) {
            return(as.integer(x))
        }
        if (!is.null(names)) {
            i <- match(tolower(substr(x, 1L, 3L)),
                       tolower(substr(names(names), 1L, 3L)))
            if (!is.na(i) && nchar(x) == 3L) {
                return(unname(names[i]))
            }
        }
        bad()
    }
    out <- integer()
    for (item in strsplit(field, ",", fixed = TRUE)[[1L]]) {
        step <- 1L
        if (grepl("/", item, fixed = TRUE)) {
            halves <- strsplit(item, "/", fixed = TRUE)[[1L]]
            if (length(halves) != 2L || !grepl("^[0-9]+$", halves[2L])) {
                bad()
            }
            step <- as.integer(halves[2L])
            if (step < 1L) {
                bad()
            }
            item <- halves[1L]
        }
        if (identical(item, "*")) {
            a <- lo
            b <- hi
        } else if (grepl("-", item, fixed = TRUE)) {
            ends <- strsplit(item, "-", fixed = TRUE)[[1L]]
            if (length(ends) != 2L) {
                bad()
            }
            a <- tok(ends[1L])
            b <- tok(ends[2L])
        } else {
            a <- tok(item)
            # "n/step" means "from n to the end of the range"
            if (step > 1L) {
                b <- hi
            } else {
                b <- a
            }
        }
        if (a < lo || b > hi || a > b) {
            bad()
        }
        out <- c(out, seq.int(a, b, by = step))
    }
    if (!length(out)) {
        bad()
    }
    sort(unique(out))
}

parse_days <- function(token, spec) {
    if (identical(token, "daily")) {
        return(unname(WEEKDAYS))
    }
    if (grepl("-", token, fixed = TRUE)) {
        ends <- strsplit(token, "-", fixed = TRUE)[[1L]]
        if (length(ends) != 2L) {
            stop("Bad day range in schedule: ", shQuote(spec), call. = FALSE)
        }
        a <- WEEKDAYS[ends[1L]]
        b <- WEEKDAYS[ends[2L]]
        if (is.na(a) || is.na(b)) {
            stop("Unknown day in schedule: ", shQuote(spec), call. = FALSE)
        }
        return(seq.int(a, b))
    }
    d <- WEEKDAYS[token]
    if (is.na(d)) {
        stop("Unknown day in schedule: ", shQuote(spec), call. = FALSE)
    }
    unname(d)
}

parse_hm <- function(token, spec) {
    if (!grepl("^[0-9]{1,2}:[0-9]{2}$", token)) {
        stop("Time must be HH:MM in schedule: ", shQuote(spec), call. = FALSE)
    }
    parts <- as.integer(strsplit(token, ":", fixed = TRUE)[[1L]])
    if (parts[1L] < 0L || parts[1L] > 23L ||
        parts[2L] < 0L || parts[2L] > 59L) {
        stop("HH:MM out of range in schedule: ", shQuote(spec), call. = FALSE)
    }
    parts
}

# Compute the next firing time strictly greater than `after`.
# Returns a POSIXct in local time.
next_fire <- function(schedule, after = Sys.time()) {
    after <- as.POSIXct(after)
    if (identical(schedule$type, "hourly")) {
        # Round down to the hour, add one. e.g. after = 14:23 -> 15:00.
        flo <- trunc(after, "hours")
        return(flo + 3600)
    }
    if (identical(schedule$type, "time")) {
        # Walk forward day by day from after's date until a matching
        # weekday + a (date, hour, minute) strictly later than `after`.
        for (offset in 0:7) {
            cand_date <- as.Date(after) + offset
            wday <- iso_wday(cand_date)
            if (!(wday %in% schedule$days)) {
                next
            }
            cand <- as.POSIXct(sprintf("%s %02d:%02d:00", format(cand_date),
                                       schedule$hour, schedule$minute))
            if (cand > after) {
                return(cand)
            }
        }
        stop("next_fire could not resolve schedule (internal): ", call. = FALSE)
    }
    if (identical(schedule$type, "cron")) {
        return(next_fire_cron(schedule, after))
    }
    stop("Unknown schedule type: ", schedule$type, call. = FALSE)
}

# Cron: walk forward from the next whole minute until every field
# matches. Days are skipped a whole day at a time and hours an hour at
# a time when they cannot match, so the walk is at most 60 + 24 + 366
# probes rather than half a million minutes. Vixie's day rule: when
# both day-of-month and day-of-week are restricted, either matching is
# enough; when only one is, that one decides.
next_fire_cron <- function(s, after) {
    t <- trunc(as.POSIXct(after), "mins") + 60
    day_ok <- function(lt) {
        dom_ok <- lt$mday %in% s$dom
        dow_ok <- iso_wday(as.Date(lt)) %in% s$dow
        if (s$dom_any && s$dow_any) {
            TRUE
        } else
        if (s$dom_any) {
            dow_ok
        } else
        if (s$dow_any) {
            dom_ok
        } else {
            dom_ok || dow_ok
        }

    }
    for (i in seq_len(60 * 24 * 366)) {
        lt <- as.POSIXlt(t)
        if (!((lt$mon + 1L) %in% s$month) || !day_ok(lt)) {
            # Jump to 00:00 *local* of the next day. Built from a
            # string like the "time" branch above: as.POSIXct() on a
            # Date is midnight UTC, which west of Greenwich is the
            # evening before, and the walk would go backwards.
            t <- as.POSIXct(sprintf("%s 00:00:00", format(as.Date(lt) + 1)))
            next
        }
        if (!(lt$hour %in% s$hour)) {
            t <- trunc(t, "hours") + 3600
            next
        }
        if (!(lt$min %in% s$minute)) {
            t <- t + 60
            next
        }
        return(t)
    }
    stop("next_fire could not resolve cron schedule within a year",
         call. = FALSE)
}

# 1=Monday ... 7=Sunday, matching ISO 8601 (and our WEEKDAYS map).
iso_wday <- function(date) {
    # POSIXlt$wday is 0=Sunday .. 6=Saturday. Shift to ISO.
    base <- as.POSIXlt(date)$wday
    if (base == 0L) {
        7L
    } else {
        base
    }
}
