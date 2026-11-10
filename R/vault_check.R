#' Run health checks for one or more pensar vaults
#'
#' Runs \code{pensar::lint()} for each configured vault, summarizes
#' findings, and optionally sends the report to Matrix.
#'
#' @param vaults Character vector or named list of vault paths. If NULL,
#'   uses \code{<instance_dir>/vault} when present, otherwise
#'   \code{<instance_dir>/wiki}.
#' @param room Matrix room name/id for notifications.
#' @param notify One of \code{"findings"}, \code{"always"}, or
#'   \code{"never"}.
#' @param min_cluster_size Passed to \code{pensar::lint()}.
#' @param max_items Maximum example items per finding type in the report.
#' @param fail_on_findings Logical. If TRUE, throw an error when any vault
#'   has findings or a lint error.
#' @param dry_run Logical. Print Matrix sends instead of sending.
#' @param instance_dir Character or NULL.
#' @return A list with class \code{cerebro_vault_check}.
#' @export
vault_check <- function(vaults = NULL, room = NULL,
                        notify = c("findings", "always", "never"),
                        min_cluster_size = 3L, max_items = 5L,
                        fail_on_findings = FALSE, dry_run = FALSE,
                        instance_dir = NULL) {
    notify <- match.arg(notify)
    min_cluster_size <- .config_integer(min_cluster_size, 3L,
                                        "min_cluster_size", min = 1L)
    max_items <- .config_integer(max_items, 5L, "max_items", min = 0L)

    vaults <- .normalize_vaults(vaults, instance_dir)
    entries <- lapply(names(vaults), function(nm) {
        .check_one_vault(nm, vaults[[nm]], min_cluster_size)
    })
    names(entries) <- names(vaults)

    total <- sum(vapply(entries, .vault_entry_count, integer(1L)))
    errors <- sum(vapply(entries, function(e) !is.null(e$error), logical(1L)))
    has_findings <- total > 0L || errors > 0L
    report <- .format_vault_check(entries, total, errors, max_items)

    result <- list(vaults = entries,
                   total_findings = total,
                   errors = errors,
                   report = report)
    class(result) <- "cerebro_vault_check"

    should_notify <- switch(notify,
                            always = TRUE,
                            findings = has_findings,
                            never = FALSE)
    if (isTRUE(should_notify)) {
        matrix_send(report, room = room, dry_run = dry_run,
                    instance_dir = instance_dir)
    } else if (!identical(notify, "never")) {
        message(report)
    }

    if (isTRUE(fail_on_findings) && has_findings) {
        stop(report, call. = FALSE)
    }
    invisible(result)
}

#' @export
print.cerebro_vault_check <- function(x, ...) {
    cat(x$report, "\n", sep = "")
    invisible(x)
}

# Capability builder. Every argument is a field on the routine entry;
# the integer knobs are validated at registration.
.cap_vault_check <- function(r, briefings, cfg, instance_dir) {
    vaults <- r$vaults %||% r$vault %||% NULL
    room <- r$room %||% NULL
    notify <- r$notify %||% "findings"
    min_cluster_size <- .config_integer(r$min_cluster_size, 3L,
                                        "min_cluster_size", min = 1L)
    max_items <- .config_integer(r$max_items, 5L, "max_items", min = 0L)
    fail_on_findings <- isTRUE(r$fail_on_findings)
    dry_run <- isTRUE(r$dry_run)
    idir <- instance_dir
    function() {
        vault_check(vaults = vaults, room = room, notify = notify,
                    min_cluster_size = min_cluster_size,
                    max_items = max_items,
                    fail_on_findings = fail_on_findings,
                    dry_run = dry_run, instance_dir = idir)
    }
}

.normalize_vaults <- function(vaults, instance_dir = NULL) {
    if (is.null(vaults)) {
        idir <- instance_dir(instance_dir)
        candidate <- file.path(idir, "vault")
        if (!dir.exists(candidate)) {
            candidate <- file.path(idir, "wiki")
        }
        vaults <- stats::setNames(candidate, basename(idir))
    }

    if (is.list(vaults)) {
        vals <- vapply(vaults, function(x) as.character(x)[1L], character(1L))
        nms <- names(vaults)
    } else {
        vals <- as.character(vaults)
        nms <- names(vaults)
    }
    vals <- path.expand(vals)
    if (is.null(nms) || length(nms) != length(vals)) {
        nms <- rep("", length(vals))
    }
    missing <- !nzchar(nms)
    nms[missing] <- basename(vals[missing])
    nms[!nzchar(nms)] <- vals[!nzchar(nms)]
    stats::setNames(as.list(vals), nms)
}

.check_one_vault <- function(name, path, min_cluster_size) {
    path <- path.expand(path)
    lint <- tryCatch(pensar::lint(path, min_cluster_size = min_cluster_size),
                     error = function(e) e)
    if (inherits(lint, "error")) {
        return(list(name = name,
                    path = path,
                    lint = NULL,
                    error = conditionMessage(lint)))
    }
    list(name = name,
         path = normalizePath(path, mustWork = FALSE),
         lint = lint,
         error = NULL)
}

.vault_entry_count <- function(entry) {
    if (!is.null(entry$error)) {
        return(0L)
    }
    lint <- entry$lint
    length(lint$orphans) +
    nrow(lint$broken_links) +
    nrow(lint$suggested_clusters)
}

.format_vault_check <- function(entries, total, errors, max_items) {
    stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    status <- if (errors > 0L) {
        sprintf("failed with %d error(s) and %d finding(s)", errors, total)
    } else if (total > 0L) {
        sprintf("found %d finding(s)", total)
    } else {
        "was clean"
    }
    lines <- sprintf("Vault check %s at %s", status, stamp)
    for (entry in entries) {
        lines <- c(lines, .format_vault_entry(entry, max_items))
    }
    paste(lines, collapse = "\n")
}

.format_vault_entry <- function(entry, max_items) {
    if (!is.null(entry$error)) {
        return(sprintf("- %s (%s): ERROR: %s", entry$name, entry$path,
                       entry$error))
    }
    lint <- entry$lint
    out <- sprintf("- %s (%s): %d orphan(s), %d broken link(s), %d tag cluster(s)",
                   entry$name, entry$path,
                   length(lint$orphans),
                   nrow(lint$broken_links),
                   nrow(lint$suggested_clusters))
    out <- c(out, .format_examples("Orphans", lint$orphans, max_items))
    broken <- if (nrow(lint$broken_links)) {
        sprintf("[[%s]] in %s", lint$broken_links$link, lint$broken_links$file)
    } else {
        character()
    }
    out <- c(out, .format_examples("Broken", broken, max_items))
    clusters <- if (nrow(lint$suggested_clusters)) {
        sprintf("%s (%d raw pages)",
                lint$suggested_clusters$tag,
                lint$suggested_clusters$raw_pages)
    } else {
        character()
    }
    c(out, .format_examples("Clusters", clusters, max_items))
}

.format_examples <- function(label, x, max_items) {
    if (!length(x)) {
        return(character())
    }
    n <- min(length(x), as.integer(max_items))
    suffix <- if (length(x) > n) {
        sprintf(" ... and %d more", length(x) - n)
    } else {
        ""
    }
    sprintf("  %s: %s%s", label, paste(x[seq_len(n)], collapse = "; "), suffix)
}
