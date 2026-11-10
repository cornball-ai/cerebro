# Source builders for cerebro::briefing(). Each source takes a `ctx`
# environment (shared across sources within one briefing run) and a
# `params` list (the source's config block from the YAML), and returns a
# named list of section texts (label -> character) or NULL.
#
# Order in the briefing's `sources:` list matters — later sources can
# read state stashed in ctx by earlier ones (e.g. saber_git uses the
# combined todo text from hacer_todos to match project tokens).

.BRIEFING_WEEKDAYS <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

# ---- hacer_todos ------------------------------------------------------
# Reads daily/week/month/quarter ToDo files matching the hacer naming
# convention and emits sections appropriate to the mode. Stashes the
# combined text on ctx for downstream sources.

#' @export
#' Briefing source builders
#'
#' Build bounded evidence sections for cerebro briefings.
#'
#' @param mode Briefing mode such as daily or weekly.
#' @param ctx Shared briefing context environment.
#' @param params Source configuration from the instance YAML.
#' @return A named list of briefing sections, or NULL.
#' @rdname briefing_sources
briefing_source_hacer_todos <- function(mode, ctx, params) {
    todo_root <- params$root %||% .source_instance_path(ctx, "todo",
        "this_week")
    archive <- params$archive %||% .source_instance_path(ctx, "todo", "archive")
    roots <- c(path.expand(todo_root), path.expand(archive))
    d <- ctx$date %||% Sys.Date()

    sections <- list()
    combined <- character()

    if (mode == "sunday") {
        # Friday's section as the end-of-week retrospective.
        fri <- .daily_section_for(roots, d - 2L, .name_of_wday(d - 2L))
        if (nzchar(fri)) {
            sections[["FRIDAY'S DAILY SECTION (end of week)"]] <- fri
            combined <- c(combined, fri)
        }
    } else if (mode != "weekly" && mode != "monthly" && mode != "quarterly") {
        # daily mode (Tue-Sat): today + yesterday
        today_name <- .name_of_wday(d)
        if (!is.na(today_name)) {
            t <- .daily_section_for(roots, d, today_name)
            if (nzchar(t)) {
                sections[["TODAY'S TODO SECTION"]] <- t
                combined <- c(combined, t)
            }
            yd <- d - 1L
            if (as.POSIXlt(yd)$wday == 0L) {
                yd <- d - 3L
            }
            yname <- .name_of_wday(yd)
            if (!is.na(yname)) {
                y <- .daily_section_for(roots, yd, yname)
                if (nzchar(y)) {
                    sections[["YESTERDAY'S TODO SECTION"]] <- y
                    combined <- c(combined, y)
                }
            }
        }
    }

    # Higher-zoom files for retrospective modes
    if (mode %in% c("sunday", "weekly", "monthly", "quarterly")) {
        wk <- .read_kind(roots, "Week", d)
        if (nzchar(wk)) {
            sections[["WEEK FILE"]] <- wk
            combined <- c(combined, wk)
        }
    }
    if (mode %in% c("monthly", "quarterly")) {
        mo <- .read_kind(roots, "Month", d)
        if (nzchar(mo)) {
            sections[["MONTH FILE"]] <- mo
            combined <- c(combined, mo)
        }
    }
    if (mode == "quarterly") {
        qt <- .read_kind(roots, "Quarter", d)
        if (nzchar(qt)) {
            sections[["QUARTER FILE"]] <- qt
            combined <- c(combined, qt)
        }
    }

    ctx$todo_text <- paste(combined, collapse = "\n")
    sections
}

.name_of_wday <- function(d) {
    w <- as.POSIXlt(d)$wday
    if (w >= 1L && w <= 5L) {
        .BRIEFING_WEEKDAYS[w]
    } else {
        NA_character_
    }
}

.daily_file_for <- function(roots, d) {
    files <- unlist(lapply(roots, function(r) {
        if (!dir.exists(r)) return(character())
        list.files(r, pattern = "^ToDo_[0-9]{6}_Daily\\.(txt|md)$",
                   full.names = TRUE)
    }))
    if (!length(files)) {
        return(NULL)
    }
    starts <- as.Date(sub("^.*ToDo_([0-9]{6}).*$", "\\1", files),
                      format = "%y%m%d")
    ok <- !is.na(starts) & starts <= d
    if (!any(ok)) {
        return(NULL)
    }
    files[ok][which.max(starts[ok])]
}

.split_daily <- function(path) {
    if (is.null(path) || !file.exists(path)) {
        return(list())
    }
    txt <- readLines(path, warn = FALSE)
    pat <- sprintf("^#+\\s+(%s)\\s*$",
                   paste(.BRIEFING_WEEKDAYS, collapse = "|"))
    is_hdr <- grepl(pat, txt)
    if (!any(is_hdr)) {
        return(list())
    }
    hdr_idx <- which(is_hdr)
    day_names <- sub(pat, "\\1", txt[hdr_idx])
    out <- list()
    for (i in seq_along(hdr_idx)) {
        start <- hdr_idx[i] + 1L
        if (i < length(hdr_idx)) {
            end <- hdr_idx[i + 1L] - 1L
        } else {
            end <- length(txt)
        }
        block <- txt[start:end]
        block <- block[!grepl("^#+\\s*$", block)]
        out[[day_names[i]]] <- paste(block, collapse = "\n")
    }
    out
}

.daily_section_for <- function(roots, d, day_name) {
    sections <- .split_daily(.daily_file_for(roots, d))
    sections[[day_name]] %||% ""
}

.read_kind <- function(roots, kind, d) {
    files <- unlist(lapply(roots, function(r) {
        if (!dir.exists(r)) return(character())
        list.files(r,
                   pattern = sprintf("^ToDo_[0-9]{6}_%s\\.(txt|md)$", kind),
                   full.names = TRUE)
    }))
    if (!length(files)) {
        return("")
    }
    starts <- as.Date(sub("^.*ToDo_([0-9]{6}).*$", "\\1", files),
                      format = "%y%m%d")
    ok <- !is.na(starts) & starts <= d
    if (!any(ok)) {
        return("")
    }
    p <- files[ok][which.max(starts[ok])]
    paste(readLines(p, warn = FALSE), collapse = "\n")
}

# ---- saber_git --------------------------------------------------------
# Tokenize ctx$todo_text -> match against saber::projects() +
# extra_repos + aliases -> pull git logs for matched projects. Stashes
# the matched-project list and known-project map on ctx for offlist_git.

#' @export
#' @rdname briefing_sources
#' @param mode Briefing mode such as daily or weekly.
#' @param ctx Shared briefing context environment.
#' @param params Source configuration from the instance YAML.
briefing_source_saber_git <- function(mode, ctx, params) {
    extra <- as.list(params$extra_repos %||% list())
    aliases <- .aliases_from_param(params$aliases)
    n <- as.integer(params$n %||% 15L)
    text <- ctx$todo_text %||% ""

    projects_df <- if (requireNamespace("saber", quietly = TRUE)) {
        tryCatch(saber::projects(), error = function(e) NULL)
    } else {
        NULL
    }
    if (is.data.frame(projects_df)) {
        proj_names <- projects_df$package
    } else {
        proj_names <- character()
    }

    matched <- .match_projects(text, proj_names, aliases, extra)
    ctx$matched_projects <- matched
    ctx$projects_df <- projects_df
    ctx$extra_repos <- extra

    if (!length(matched)) {
        return(NULL)
    }

    logs <- list()
    for (p in matched) {
        path <- .project_path(p, projects_df, extra)
        if (is.null(path)) {
            next
        }
        log <- .git_log_text(path, NULL, n)
        if (nzchar(log)) {
            logs[[p]] <- log
        }
    }
    ctx$git_logs <- logs
    if (!length(logs)) {
        return(NULL)
    }

    body <- vapply(names(logs), function(p) {
        sprintf("-- %s --\n%s", p, logs[[p]])
    }, character(1))
    list(`RECENT COMMITS (last ~15 per project, ISO timestamps)` =
         paste(body, collapse = "\n"))
}

.aliases_from_param <- function(x) {
    if (is.null(x)) {
        return(list())
    }
    out <- list()
    if (is.list(x)) {
        for (k in names(x)) {
            out[[tolower(k)]] <- as.character(x[[k]])
        }
        return(out)
    }
    if (is.character(x) && length(x) == 1L && file.exists(path.expand(x))) {
        lines <- readLines(path.expand(x), warn = FALSE)
        lines <- trimws(lines[!grepl("^\\s*(#|$)", lines)])
        for (ln in lines) {
            parts <- strsplit(ln, "->", fixed = TRUE)[[1]]
            if (length(parts) == 2L) {
                out[[tolower(trimws(parts[1]))]] <- trimws(parts[2])
            }
        }
    }
    out
}

.extract_tokens <- function(text) {
    if (!nzchar(text)) {
        return(character())
    }
    raw <- unlist(strsplit(text, "[^A-Za-z0-9._+-]+"))
    tolower(raw[nzchar(raw)])
}

.match_projects <- function(text, known, aliases, extra) {
    toks <- unique(.extract_tokens(text))
    toks <- toks[nchar(toks) >= 3L & !grepl("^[0-9]+$", toks)]
    known_lc <- tolower(known)
    extra_lc <- tolower(names(extra))
    matched <- character()
    for (t in toks) {
        if (t %in% known_lc) {
            matched <- c(matched, known[match(t, known_lc)])
        } else if (!is.null(aliases[[t]])) {
            matched <- c(matched, aliases[[t]])
        } else if (t %in% extra_lc) {
            matched <- c(matched, names(extra)[match(t, extra_lc)])
        }
    }
    unique(matched)
}

.project_path <- function(name, projects_df, extra) {
    if (is.data.frame(projects_df) && nrow(projects_df)) {
        row <- projects_df[projects_df$package == name,]
        if (nrow(row)) {
            return(row$path[1])
        }
    }
    if (!is.null(extra[[name]])) {
        return(path.expand(extra[[name]]))
    }
    NULL
}

# Git plumbing lives in saber (git_log_since/git_commit_count_since,
# the primitives behind saber::heartbeat()). This shim keeps the old
# collapsed-string contract: one ISO-format line per commit, "" when
# there is nothing to show.
.git_log_text <- function(project_path, since_date, n) {
    if (is.null(project_path)) {
        return("")
    }
    lines <- saber::git_log_since(project_path, since_date, n = n,
                                  format = "iso")
    paste(lines, collapse = "\n")
}

# ---- offlist_git ------------------------------------------------------
# Scan known projects (saber + extra_repos) for commits in the lookback
# window that aren't already in ctx$matched_projects. Catches work that
# shipped without making it to the todo list.

#' @export
#' @rdname briefing_sources
#' @param mode Briefing mode such as daily or weekly.
#' @param ctx Shared briefing context environment.
#' @param params Source configuration from the instance YAML.
briefing_source_offlist_git <- function(mode, ctx, params) {
    lookback <- params$lookback_days %||% list(daily = 2, sunday = 7,
        weekly = 7, monthly = 31, quarterly = 92)
    n <- as.integer(params$n %||% 10L)
    min_commits <- as.integer(params$min_commits %||% 1L)
    days <- as.integer(lookback[[mode]] %||% 2L)
    since_date <- (ctx$date %||% Sys.Date()) - days

    matched_lc <- tolower(ctx$matched_projects %||% character())
    candidates <- list()
    pdf <- ctx$projects_df
    if (is.data.frame(pdf) && nrow(pdf)) {
        for (i in seq_len(nrow(pdf))) {
            nm <- pdf$package[i]
            if (tolower(nm) %in% matched_lc) {
                next
            }
            candidates[[nm]] <- pdf$path[i]
        }
    }
    extra <- ctx$extra_repos %||% list()
    for (nm in names(extra)) {
        if (tolower(nm) %in% matched_lc) {
            next
        }
        if (is.null(candidates[[nm]])) {
            candidates[[nm]] <- path.expand(extra[[nm]])
        }
    }

    out <- list()
    for (nm in names(candidates)) {
        cnt <- saber::git_commit_count_since(candidates[[nm]], since_date)
        if (cnt < min_commits) {
            next
        }
        log <- .git_log_text(candidates[[nm]], NULL, n)
        if (!nzchar(log)) {
            next
        }
        out[[nm]] <- list(count = cnt, log = log)
    }
    if (!length(out)) {
        return(NULL)
    }

    lines <- c(
               "These projects shipped work in the lookback window without appearing in the todo sections. Surface them in the briefing - the user clearly worked on them, and they deserve mention proportional to commit volume."
    )
    for (p in names(out)) {
        lines <- c(lines,
                   sprintf("-- %s (%d commit(s) in window) --", p, out[[p]]$count),
                   out[[p]]$log)
    }
    list(`OFF-LIST ACTIVITY (projects with recent commits but NOT on the todo)` =
         paste(lines, collapse = "\n"))
}

# ---- git_recent -------------------------------------------------------
# Todo-independent recent Git activity across saber-discovered projects and
# explicitly configured repositories.

#' @export
#' @rdname briefing_sources
#' @param mode Briefing mode such as daily or weekly.
#' @param ctx Shared briefing context environment.
#' @param params Source configuration from the instance YAML.
briefing_source_git_recent <- function(mode, ctx, params) {
    lookback <- params$lookback_days %||% list(daily = 2, sunday = 7,
        weekly = 7, monthly = 31, quarterly = 92)
    days <- as.integer(params$days %||% lookback[[mode]] %||% 2L)
    n <- as.integer(params$n %||% 10L)
    min_commits <- as.integer(params$min_commits %||% 1L)
    max_projects <- as.integer(params$max_projects %||% 20L)
    since_date <- (ctx$date %||% Sys.Date()) - days

    projects <- list()
    if (is.null(params$discover)) {
        discover <- TRUE
    } else {
        discover <- isTRUE(params$discover)
    }
    if (discover && requireNamespace("saber", quietly = TRUE)) {
        pdf <- tryCatch(saber::projects(), error = function(e) NULL)
        if (is.data.frame(pdf) && nrow(pdf)) {
            for (i in seq_len(nrow(pdf))) {
                projects[[pdf$package[[i]]]] <- pdf$path[[i]]
            }
        }
    }
    extra <- as.list(params$extra_repos %||% list())
    for (nm in names(extra)) {
        projects[[nm]] <- path.expand(extra[[nm]])
    }
    if (!length(projects)) {
        return(NULL)
    }

    found <- list()
    for (nm in names(projects)) {
        path <- path.expand(projects[[nm]])
        count <- saber::git_commit_count_since(path, since_date)
        if (count < min_commits) {
            next
        }
        log <- .git_log_text(path, since_date, n)
        if (nzchar(log)) {
            found[[nm]] <- list(count = count, log = log)
        }
    }
    if (!length(found)) {
        return(NULL)
    }
    counts <- vapply(found, `[[`, integer(1L), "count")
    ord <- order(-counts, tolower(names(found)))
    found <- found[utils::head(ord, max_projects)]

    lines <- unlist(lapply(names(found), function(nm) {
        c(sprintf("-- %s (%d commit(s) in window) --", nm, found[[nm]]$count),
            found[[nm]]$log)
    }), use.names = FALSE)
    text <- paste(lines, collapse = "\n")
    ctx$git_recent_text <- text
    list(`RECENT GIT ACTIVITY` = text)
}

# ---- vault_recent -----------------------------------------------------
# Bounded excerpts from recently changed raw and wiki pages, balanced across
# source categories so frequently refreshed repo briefings do not crowd out
# article or synthesis pages. Matrix transcripts can be excluded when the
# dedicated chat_recent source is also configured.

#' @export
#' @rdname briefing_sources
#' @param mode Briefing mode such as daily or weekly.
#' @param ctx Shared briefing context environment.
#' @param params Source configuration from the instance YAML.
briefing_source_vault_recent <- function(mode, ctx, params) {
    vault <- params$vault %||% params$root %||%
    .source_instance_path(ctx, "vault")
    vault <- path.expand(vault)
    if (!dir.exists(vault)) {
        return(NULL)
    }
    days <- as.integer(params$days %||% 3L)
    max_files <- as.integer(params$max_files %||% 16L)
    max_per_group <- as.integer(params$max_per_group %||% 4L)
    max_chars <- as.integer(params$max_chars %||% 1800L)
    exclude_groups <- as.character(unlist(params$exclude_groups %||%
            character(), use.names = FALSE))

    roots <- file.path(vault, c("raw", "wiki"))
    files <- unlist(lapply(roots[dir.exists(roots)], function(root) {
        list.files(root, pattern = "\\.md$", recursive = TRUE,
                   full.names = TRUE)
    }), use.names = FALSE)
    if (!length(files)) {
        return(NULL)
    }
    info <- file.info(files)
    window <- .local_date_window(ctx$date %||% Sys.Date(), days)
    keep <- !is.na(info$mtime) & info$mtime >= window$cutoff &
    info$mtime < window$end
    files <- files[keep]
    info <- info[keep,, drop = FALSE]
    if (!length(files)) {
        return(NULL)
    }

    rel <- substring(files, nchar(vault) + 2L)
    group <- vapply(rel, .vault_recent_group, character(1L))
    if (length(exclude_groups)) {
        keep <- !group %in% exclude_groups
        files <- files[keep]
        info <- info[keep,, drop = FALSE]
        rel <- rel[keep]
        group <- group[keep]
    }
    if (!length(files)) {
        return(NULL)
    }
    selected <- integer()
    for (g in unique(group)) {
        idx <- which(group == g)
        idx <- idx[order(info$mtime[idx], decreasing = TRUE)]
        selected <- c(selected, utils::head(idx, max_per_group))
    }
    selected <- unique(selected)
    selected <- selected[order(info$mtime[selected], decreasing = TRUE)]
    selected <- utils::head(selected, max_files)

    blocks <- vapply(selected, function(i) {
        excerpt <- .vault_recent_excerpt(files[[i]], max_chars)
        sprintf("-- %s (%s) --\n%s", rel[[i]],
                format(info$mtime[[i]], "%Y-%m-%d %H:%M:%S %Z"), excerpt)
    }, character(1L))
    text <- paste(blocks, collapse = "\n\n")
    ctx$vault_recent_text <- paste(ctx$vault_recent_text %||% "", text,
                                   sep = "\n\n")
    list(`RECENT VAULT CONTENT` = text)
}

.vault_recent_group <- function(path) {
    if (startsWith(path, "wiki/")) {
        return("wiki")
    }
    if (startsWith(path, "raw/repos/")) {
        return("repos")
    }
    if (startsWith(path, "raw/matrix/")) {
        return("matrix")
    }
    if (startsWith(path, "raw/articles/")) {
        return("articles")
    }
    "raw_other"
}

.vault_recent_excerpt <- function(path, max_chars) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    if (length(lines) && identical(trimws(lines[[1L]]), "---")) {
        ends <- which(trimws(lines[-1L]) == "---")
        if (length(ends)) {
            lines <- lines[-seq_len(ends[[1L]] + 1L)]
        }
    }
    text <- trimws(paste(lines, collapse = "\n"))
    if (nchar(text) > max_chars) {
        paste0(substr(text, 1L, max_chars), "...")
    } else {
        text
    }
}

# ---- fortune_candidates ----------------------------------------------
# Score R fortunes against terms in the combined Git and vault evidence. The
# model receives a small candidate set and selects one without paraphrasing it.

#' @export
#' @rdname briefing_sources
#' @param mode Briefing mode such as daily or weekly.
#' @param ctx Shared briefing context environment.
#' @param params Source configuration from the instance YAML.
briefing_source_fortune_candidates <- function(mode, ctx, params) {
    if (!requireNamespace("fortunes", quietly = TRUE)) {
        stop("fortunes package required for fortune candidates", call. = FALSE)
    }
    evidence <- paste(ctx$git_recent_text %||% "",
                      ctx$vault_recent_text %||% "",
                      ctx$chat_recent_text %||% "")
    terms <- .fortune_terms(evidence, as.integer(params$max_terms %||% 30L))
    data <- fortunes::read.fortunes()
    searchable <- apply(data, 1L, function(row) {
        tolower(paste(row, collapse = " "))
    })
    score <- numeric(nrow(data))
    if (length(terms)) {
        weights <- rev(seq_along(terms))
        for (i in seq_along(terms)) {
            score <- score + weights[[i]] *
            grepl(terms[[i]], searchable, fixed = TRUE)
        }
    }
    quote_limit <- as.integer(params$max_quote_chars %||% 500L)
    score[nchar(data$quote) > quote_limit] <- 0
    if (!any(score > 0)) {
        fallback <- c("code", "software", "data", "model", "programming")
        for (term in fallback) {
            score <- score + grepl(term, searchable, fixed = TRUE)
        }
    }
    idx <- which(score > 0)
    if (!length(idx)) {
        return(NULL)
    }
    idx <- idx[order(-score[idx], idx)]
    idx <- utils::head(idx, as.integer(params$max_candidates %||% 5L))
    candidates <- vapply(seq_along(idx), function(j) {
        row <- data[idx[[j]],, drop = FALSE]
        context <- if (!is.na(row$context) && nzchar(row$context)) {
            paste0(" (", row$context, ")")
        } else {
            ""
        }
        sprintf("[%d] %s\n-- %s%s, %s (%s)", j, row$quote,
                row$author, context, row$source, row$date)
    }, character(1L))
    list(`FORTUNE CANDIDATES` = paste(candidates, collapse = "\n\n"))
}

.fortune_terms <- function(text, max_terms) {
    words <- unlist(strsplit(tolower(text), "[^a-z0-9]+"), use.names = FALSE)
    stopwords <- c("about", "after", "again", "also", "been", "before",
                   "being", "between", "commit", "commits", "could",
                   "from", "have", "into", "latest", "more", "recent",
                   "that", "their", "there", "these", "they", "this",
                   "those", "through", "using", "were", "what", "when",
                   "where", "which", "while", "with", "would")
    words <- words[nchar(words) >= 4L & !words %in% stopwords]
    if (!length(words)) {
        return(character())
    }
    counts <- sort(table(words), decreasing = TRUE)
    utils::head(names(counts), max_terms)
}

# ---- wiki -------------------------------------------------------------
# Pull a tail of the wiki log + names of pages touched in the last N days.

#' @export
#' @rdname briefing_sources
#' @param mode Briefing mode such as daily or weekly.
#' @param ctx Shared briefing context environment.
#' @param params Source configuration from the instance YAML.
briefing_source_wiki <- function(mode, ctx, params) {
    root <- path.expand(params$root %||% .source_instance_path(ctx, "wiki"))
    tail_lines <- as.integer(params$tail_lines %||% 80L)
    recent_days <- as.integer(params$recent_days %||% 7L)
    d <- ctx$date %||% Sys.Date()

    out <- list()
    log_file <- file.path(root, "log.md")
    if (file.exists(log_file)) {
        txt <- readLines(log_file, warn = FALSE)
        out[["WIKI LOG (tail)"]] <- paste(utils::tail(txt, tail_lines),
            collapse = "\n")
    }
    pages_dir <- file.path(root, "wiki")
    if (dir.exists(pages_dir)) {
        files <- list.files(pages_dir, pattern = "\\.md$", full.names = TRUE)
        if (length(files)) {
            mt <- as.Date(file.info(files)$mtime)
            recent <- basename(files[!is.na(mt) & mt >= (d - recent_days)])
            if (length(recent)) {
                out[["WIKI PAGES TOUCHED THIS WEEK"]] <- paste(recent,
                    collapse = ", ")
            }
        }
    }
    if (!length(out)) {
        return(NULL)
    }
    out
}

# ---- registry ---------------------------------------------------------

.briefing_source_registry <- list(
                                  hacer_todos = briefing_source_hacer_todos,
                                  saber_git = briefing_source_saber_git,
                                  offlist_git = briefing_source_offlist_git,
                                  git_recent = briefing_source_git_recent,
                                  vault_recent = briefing_source_vault_recent,
                                  chat_recent = function(mode, ctx, params)
                                  briefing_source_chat_recent(mode, ctx, params),
                                  fortune_candidates = briefing_source_fortune_candidates,
                                  wiki = briefing_source_wiki
)

.run_source <- function(spec, mode, ctx) {
    type <- spec$type
    if (is.null(type)) {
        stop("source spec missing $type", call. = FALSE)
    }
    fn <- .briefing_source_registry[[type]]
    if (is.null(fn)) {
        stop("unknown briefing source type: ", type, call. = FALSE)
    }
    fn(mode, ctx, spec)
}

.source_instance_path <- function(ctx, ...) {
    idir <- ctx$instance_dir %||% NULL
    if (is.null(idir) || !nzchar(idir)) {
        idir <- tryCatch(instance_dir(NULL), error = function(e) NULL)
    }
    if (is.null(idir) || !nzchar(idir)) {
        stop("source root not set and no active cerebro instance dir. ",
             "Pass root/archive explicitly or set instance_dir.",
             call. = FALSE)
    }
    file.path(idir, ...)
}
