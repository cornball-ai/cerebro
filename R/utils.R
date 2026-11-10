`%||%` <- function(x, y) {
    if (is.null(x) || !length(x)) {
        y
    } else {
        x
    }
}

# normalizePath() resolves symlinks only for a path that exists, so the
# same path reads two ways before and after it is created, and a vault
# or instance dir behind a symlink (macOS's /var, a linked home) fails
# prefix checks against its resolved form. Resolve the longest existing
# ancestor and re-append the rest, so a path normalizes the same way
# whether or not it exists yet.
.resolve_path <- function(path) {
    path <- path.expand(path)
    rest <- character()
    while (!file.exists(path)) {
        parent <- dirname(path)
        if (identical(parent, path)) {
            break
        }
        rest <- c(basename(path), rest)
        path <- parent
    }
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    if (length(rest)) {
        do.call(file.path, as.list(c(path, rest)))
    } else {
        path
    }
}

.local_date_window <- function(date, days, tz = Sys.timezone()) {
    date <- as.Date(date)
    days <- as.integer(days)
    if (length(date) != 1L || is.na(date)) {
        stop("date must be a non-missing Date scalar", call. = FALSE)
    }
    if (length(days) != 1L || is.na(days) || days < 0L) {
        stop("days must be a non-negative integer", call. = FALSE)
    }
    end_date <- date + 1L
    list(cutoff = as.POSIXct(as.character(end_date - days), tz = tz),
         end = as.POSIXct(as.character(end_date), tz = tz))
}
