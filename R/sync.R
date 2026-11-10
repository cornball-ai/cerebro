#' Sync a todo subtree from a source repo into this instance
#'
#' Pulls \code{source_repo} (fast-forward only, so a dirty or diverged
#' source never blocks the routine), then mirrors the \code{source_path}
#' subtree of the source's current week file down into \code{target_path}
#' of this instance's week file via the optional
#' \code{\link[hacer]{mirror_subtree}} integration. The source wins on shared
#' paths; nothing is deleted.
#'
#'
#' @param name Character. Idempotency key + log label.
#' @param source_repo Character or NULL. Git repo to fast-forward pull
#'   before mirroring (NULL to skip the pull).
#' @param source_root Directory holding the source week file.
#' @param target_root Directory holding this instance's week file.
#' @param source_path Non-empty source subtree breadcrumb.
#' @param target_path Non-empty target subtree breadcrumb.
#' @param period Character period label.
#' @param room Character or NULL. Matrix room for the summary.
#' @param pull Logical. Fast-forward pull \code{source_repo} first.
#' @param force Logical. Override once-per-day guard.
#' @param dry_run Logical. Print a unified diff of what would change and
#'   write/send nothing (and never pull).
#' @param instance_dir Character or NULL.
#' @param date Date. For testing; defaults to today.
#' @return The mirror plan (list of \code{added}/\code{restatused}),
#'   invisibly. NULL when skipped.
#' @export
sync_todo <- function(name = "sync", source_repo = NULL, source_root,
                      target_root, source_path = NULL, target_path = NULL,
                      period = "Week", room = NULL, pull = TRUE,
                      force = FALSE, dry_run = FALSE, instance_dir = NULL,
                      date = Sys.Date()) {
    if (!is.character(source_path) || length(source_path) != 1L ||
        is.na(source_path) || !nzchar(source_path)) {
        stop("source_path must be a non-empty character scalar", call. = FALSE)
    }
    if (!is.character(target_path) || length(target_path) != 1L ||
        is.na(target_path) || !nzchar(target_path)) {
        stop("target_path must be a non-empty character scalar", call. = FALSE)
    }

    runner <- function() {
        if (isTRUE(pull) && !is.null(source_repo) && !isTRUE(dry_run)) {
            .sync_git_pull(source_repo)
        }
        src_file <- .sync_week_file(source_root, date)
        tgt_file <- .sync_week_file(target_root, date)
        if (is.null(src_file) || is.null(tgt_file)) {
            message("cerebro: sync \u2014 missing week file (source found: ",
                    !is.null(src_file), ", target found: ",
                    !is.null(tgt_file), "); skipping")
            return(invisible(NULL))
        }
        if (!requireNamespace("hacer", quietly = TRUE)) {
            stop("hacer required for sync", call. = FALSE)
        }

        plan <- hacer::mirror_subtree(src_file, tgt_file, source_path,
                                      target_path, period = period,
                                      conflict = "source", preview = TRUE)

        if (isTRUE(dry_run)) {
            diff <- .sync_unified_diff(src_file, tgt_file, source_path,
                                       target_path, period)
            cat("=== sync ", name, " (dry-run) ===\n", sep = "")
            cat("would add: ", length(plan$added), "   restatus: ",
                length(plan$restatused), "\n\n", sep = "")
            if (length(diff)) {
                cat(diff, sep = "\n")
            } else {
                cat("(no changes)\n")
            }
            return(invisible(plan))
        }

        if (!length(plan$added) && !length(plan$restatused)) {
            message("cerebro: sync \u2014 already in step with ", basename(src_file))
            return(invisible(plan))
        }

        hacer::mirror_subtree(src_file, tgt_file, source_path, target_path,
                              period = period, conflict = "source",
                              preview = FALSE)
        message(sprintf("cerebro: sync wrote %d add / %d restatus to %s",
                        length(plan$added), length(plan$restatused),
                        basename(tgt_file)))
        if (!is.null(room)) {
            matrix_send(.sync_summary(plan, target_path), room = room,
                        instance_dir = instance_dir, markdown = TRUE)
        }
        invisible(plan)
    }

    if (isTRUE(dry_run)) {
        invisible(runner())
    } else {
        once_today(paste0("sync_", name), runner, force = force,
                   instance_dir = instance_dir)
    }
}

# Capability builder. `source_path` and `target_path` are required and
# checked at registration; `target_root` defaults to the instance's
# `todo/this_week`; `pull` defaults to TRUE.
.cap_sync <- function(r, briefings, cfg, instance_dir) {
    source_path <- r$source_path %||% NULL
    target_path <- r$target_path %||% NULL
    if (is.null(source_path) || is.null(target_path)) {
        stop("cerebro: sync routine ", r$name,
             " requires source_path and target_path", call. = FALSE)
    }
    source_repo <- r$source_repo %||% NULL
    source_root <- r$source_root
    target_root <- r$target_root %||% file.path(
        tryCatch(instance_dir(instance_dir), error = function(e) "."),
        "todo", "this_week")
    room <- r$room %||% NULL
    pull <- isTRUE(r$pull %||% TRUE)
    nm <- r$name %||% "sync"
    idir <- instance_dir
    function() {
        sync_todo(name = nm, source_repo = source_repo,
                  source_root = source_root, target_root = target_root,
                  source_path = source_path, target_path = target_path,
                  room = room, pull = pull, instance_dir = idir)
    }
}

# Latest todo_<yymmdd>_week.md in `root` whose date is on or before `date`.
.sync_week_file <- function(root, date) {
    if (is.null(root) || !dir.exists(root)) {
        return(NULL)
    }
    files <- list.files(root, pattern = "^todo_[0-9]{6}_week\\.(md|txt)$",
                        full.names = TRUE, ignore.case = TRUE)
    if (!length(files)) {
        return(NULL)
    }
    starts <- as.Date(sub("^.*todo_([0-9]{6}).*$", "\\1", basename(files)),
                      format = "%y%m%d")
    ok <- !is.na(starts) & starts <= date
    if (!any(ok)) {
        return(NULL)
    }
    files[ok][which.max(starts[ok])]
}

# Fast-forward-only pull. Never errors the routine: a dirty or diverged
# source just logs and proceeds against the current on-disk state.
.sync_git_pull <- function(repo) {
    repo <- path.expand(repo)
    if (!dir.exists(file.path(repo, ".git"))) {
        message("cerebro: sync \u2014 ", repo,
                " is not a git repo; skipping pull")
        return(invisible(FALSE))
    }
    out <- suppressWarnings(tryCatch(
                                     system2("git", c("-C", shQuote(repo), "pull", "--ff-only"),
                stdout = TRUE, stderr = TRUE),
                                     error = function(e) {
        structure(conditionMessage(e), status = 1L)
    }))
    st <- attr(out, "status")
    ok <- is.null(st) || identical(st, 0L)
    if (!ok) {
        message("cerebro: sync \u2014 git pull --ff-only on ", repo,
                " skipped: ", paste(out, collapse = " "))
    }
    invisible(ok)
}

# Unified diff of the target week file before vs after a mirror, computed
# on a throwaway copy so nothing on disk changes. The temp copy keeps the
# same basename so build_todo_txt_lines writes an identical header line
# (no spurious header diff). system2 goes through the shell, so the paths
# are shQuote'd.
.sync_unified_diff <- function(src_file, tgt_file, source_path, target_path,
                               period) {
    tdir <- tempfile()
    dir.create(tdir)
    on.exit(unlink(tdir, recursive = TRUE), add = TRUE)
    tmp <- file.path(tdir, basename(tgt_file))
    file.copy(tgt_file, tmp, overwrite = TRUE)
    hacer::mirror_subtree(src_file, tmp, source_path, target_path,
                          period = period, conflict = "source",
                          preview = FALSE)
    suppressWarnings(system2("diff",
                             c("-u", shQuote(tgt_file), shQuote(tmp)),
                             stdout = TRUE, stderr = TRUE))
}

.sync_summary <- function(plan, target_path) {
    pre <- paste0(target_path, " > ")
    strip <- function(p) {
        ifelse(startsWith(p, pre), substring(p, nchar(pre) + 1L), p)
    }
    parts <- "**Synced todo:**"
    if (length(plan$added)) {
        parts <- c(parts, "", "Added:", paste0("- ", strip(plan$added)))
    }
    if (length(plan$restatused)) {
        parts <- c(parts, "", "Status updated:",
                   paste0("- ", strip(plan$restatused)))
    }
    paste(parts, collapse = "\n")
}
