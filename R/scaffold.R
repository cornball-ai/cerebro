#' Scaffold a cerebro persona repository
#'
#' Creates the generic structural files and directories used by a cerebro
#' persona. Existing files are preserved by default: files that differ from
#' the current template are reported as conflicts unless \code{overwrite} is
#' true. The managed cerebro block in \code{.gitignore} is updated in place
#' while preserving all content outside that block.
#'
#' The generated config is intentionally inactive: Matrix is disabled and no
#' routines, credentials, provider, model, rooms, or persona content are
#' supplied. Those remain explicit instance-owned choices.
#'
#' @param name Character. Persona identifier used in config and service names.
#'   Must contain only letters, numbers, underscores, and hyphens.
#' @param instance_dir Character. Persona repository or project directory.
#' @param vault Character. Vault policy: \code{"local"} creates a vault ignored
#'   by Git, \code{"tracked"} creates a vault whose source may be tracked while
#'   its rendered site is ignored, and \code{"none"} creates no vault.
#' @param timezone Character. IANA timezone written to \code{todo/config.yaml}.
#' @param timer Character or NULL. Optional systemd OnCalendar value for a
#'   generated \code{<name>.timer} targeting \code{<name>.service}. The default
#'   service is long-running, so use this only when the instance overrides that
#'   service to be one-shot or normally inactive.
#' @param overwrite Logical. Replace existing scaffold files that differ from
#'   the current templates.
#' @param dry_run Logical. Report planned actions without writing.
#' @return A data frame with one row per managed path and its action, invisibly.
#' @export
scaffold_persona <- function(name, instance_dir = file.path("~", name),
                             vault = c("local", "tracked", "none"),
                             timezone = Sys.timezone(), timer = NULL,
                             overwrite = FALSE, dry_run = FALSE) {
    .scaffold_validate_name(name)
    .scaffold_scalar(instance_dir, "instance_dir", type = "character")
    .scaffold_scalar(overwrite, "overwrite", type = "logical")
    .scaffold_scalar(dry_run, "dry_run", type = "logical")
    if (!is.null(timer)) {
        .scaffold_scalar(timer, "timer", type = "character")
    }
    vault <- match.arg(vault)
    timezone <- .scaffold_timezone(timezone)

    base <- .resolve_path(instance_dir)
    if (file.exists(base) && !dir.exists(base)) {
        stop("instance_dir exists but is not a directory: ", base,
             call. = FALSE)
    }

    values <- list(
                   name = name,
                   name_yaml = .scaffold_yaml_quote(name),
                   instance_dir_systemd = .scaffold_systemd_value(base),
                   todo_live_yaml = .scaffold_yaml_quote(
            file.path(base, "todo", "this_week")
        ),
                   todo_archive_yaml = .scaffold_yaml_quote(
            file.path(base, "todo", "archive")
        ),
                   todo_dir_systemd = .scaffold_systemd_value(file.path(base, "todo")),
                   timezone_yaml = .scaffold_yaml_quote(timezone),
                   timer_calendar_systemd = if (is.null(timer)) "" else
                   .scaffold_systemd_value(timer)
    )
    templates <- .scaffold_templates(timer)
    targets <- vapply(unname(templates), function(target) {
        file.path(base, .scaffold_replace(target, "{{name}}", name))
    }, character(1), USE.NAMES = FALSE)
    contents <- lapply(names(templates), function(template) {
        .scaffold_render(template, values)
    })
    names(contents) <- targets
    ignore_path <- file.path(base, ".gitignore")
    ignore <- .scaffold_gitignore(ignore_path, vault)

    dirs <- c(
              base,
              file.path(base, "prompts"),
              file.path(base, "scripts"),
              file.path(base, "todo", "this_week"),
              file.path(base, "todo", "archive"),
              file.path(base, ".corteza"),
              file.path(base, ".claude")
    )
    if (vault != "none") {
        dirs <- c(dirs,
                  file.path(base, "vault", "raw"),
                  file.path(base, "vault", "wiki"))
    }

    report <- .scaffold_plan_dirs(dirs, dry_run = dry_run)
    for (path in names(contents)) {
        row <- .scaffold_apply_file(
                                    path,
                                    contents[[path]],
                                    overwrite = overwrite,
                                    dry_run = dry_run
        )
        report <- rbind(report, row)
    }

    report <- rbind(
                    report,
                    .scaffold_apply_file(
            ignore_path,
            ignore,
            overwrite = TRUE,
            dry_run = dry_run,
            update_action = TRUE
        )
    )
    rownames(report) <- NULL

    conflicts <- report$action == "conflict"
    if (any(conflicts)) {
        message(
                "cerebro: preserved ", sum(conflicts),
                " existing scaffold file(s) with local changes; ",
                "set overwrite = TRUE to replace them"
        )
    }
    invisible(report)
}

.scaffold_templates <- function(timer = NULL) {
    templates <-
    c("config.yaml.tmpl" = "config.yaml", "CLAUDE.md.tmpl" = "CLAUDE.md",
        "AGENTS.md.tmpl" = "AGENTS.md",
        "persona.service.tmpl" = "{{name}}.service",
        "corteza/config.json.tmpl" = ".corteza/config.json",
        "claude/settings.local.json.tmpl" = ".claude/settings.local.json",
        "prompts/system.md.tmpl" = "prompts/system.md",
        "todo/config.yaml.tmpl" = "todo/config.yaml",
        "todo/recurring.txt.tmpl" = "todo/recurring.txt")
    if (!is.null(timer)) {
        templates <- c(templates, "persona.timer.tmpl" = "{{name}}.timer")
    }
    templates
}

.scaffold_render <- function(template, values) {
    root <- system.file("persona", package = "cerebro")
    if (!nzchar(root)) {
        stop("cerebro persona templates are not installed", call. = FALSE)
    }
    path <- file.path(root, template)
    if (!file.exists(path)) {
        stop("missing cerebro persona template: ", template, call. = FALSE)
    }
    out <- readLines(path, warn = FALSE, encoding = "UTF-8")
    for (key in names(values)) {
        out <- .scaffold_replace(out, paste0("{{", key, "}}"), values[[key]])
    }
    unresolved <- grepl("\\{\\{[^}]+\\}\\}", out)
    if (any(unresolved)) {
        stop("unresolved placeholder in persona template: ", template,
             call. = FALSE)
    }
    out
}

.scaffold_replace <- function(x, pattern, replacement) {
    replace_one <- function(value) {
        matches <- gregexpr(pattern, value, fixed = TRUE)[[1L]]
        if (matches[[1L]] == -1L) {
            return(value)
        }
        lengths <- attr(matches, "match.length")
        pieces <- character()
        cursor <- 1L
        for (i in seq_along(matches)) {
            prefix <- if (matches[[i]] > cursor) {
                substr(value, cursor, matches[[i]] - 1L)
            } else {
                ""
            }
            pieces <- c(pieces, prefix, replacement)
            cursor <- matches[[i]] + lengths[[i]]
        }
        suffix <- if (cursor <= nchar(value)) {
            substr(value, cursor, nchar(value))
        } else {
            ""
        }
        paste0(c(pieces, suffix), collapse = "")
    }
    vapply(x, replace_one, character(1), USE.NAMES = FALSE)
}

.scaffold_plan_dirs <- function(paths, dry_run) {
    rows <- lapply(unique(paths), function(path) {
        exists <- dir.exists(path)
        created <- FALSE
        if (!exists && !dry_run) {
            dir.create(path, recursive = TRUE, showWarnings = FALSE)
            exists <- dir.exists(path)
            if (!exists) {
                stop("could not create directory: ", path, call. = FALSE)
            }
            created <- TRUE
        }
        data.frame(
                   path = path,
                   type = "directory",
                   action = if (created) {
                "created"
            } else if (exists) {
                "unchanged"
            } else {
                "would_create"
            },
                   stringsAsFactors = FALSE
        )
    })
    do.call(rbind, rows)
}

.scaffold_apply_file <- function(path, content, overwrite, dry_run,
                                 update_action = FALSE) {
    if (!file.exists(path)) {
        if (dry_run) {
            action <- "would_create"
        } else {
            action <- "created"
        }
        if (!dry_run) {
            .scaffold_write(path, content)
        }
    } else {
        current <- readLines(path, warn = FALSE, encoding = "UTF-8")
        if (identical(current, content)) {
            action <- "unchanged"
        } else if (!overwrite) {
            action <- "conflict"
        } else {
            action <- if (dry_run) {
                if (update_action) {
                    "would_update"
                } else {
                    "would_overwrite"
                }
            } else {
                if (update_action) {
                    "updated"
                } else {
                    "overwritten"
                }
            }
            if (!dry_run) {
                .scaffold_write(path, content)
            }
        }
    }
    data.frame(path = path, type = "file", action = action,
               stringsAsFactors = FALSE)
}

.scaffold_write <- function(path, content) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile(".cerebro-", tmpdir = dirname(path))
    on.exit(unlink(tmp), add = TRUE)
    writeLines(enc2utf8(content), tmp, useBytes = TRUE)
    if (!file.copy(tmp, path, overwrite = TRUE, copy.mode = TRUE)) {
        stop("could not write scaffold file: ", path, call. = FALSE)
    }
    invisible(path)
}

.scaffold_gitignore <- function(path, vault) {
    current <- if (file.exists(path)) {
        readLines(path, warn = FALSE, encoding = "UTF-8")
    } else {
        character()
    }
    begin <- "# >>> cerebro scaffold >>>"
    end <- "# <<< cerebro scaffold <<<"
    starts <- which(current == begin)
    finishes <- which(current == end)
    has_block <- length(starts) > 0L || length(finishes) > 0L
    valid_block <- length(starts) == 1L && length(finishes) == 1L &&
    finishes[[1L]] >= starts[[1L]]
    if (has_block && !valid_block) {
        stop("malformed cerebro scaffold block in ", path, call. = FALSE)
    }
    if (valid_block) {
        start <- starts[[1L]]
        finish <- finishes[[1L]]
        current <- current[-seq.int(start, finish)]
    }
    while (length(current) && !nzchar(current[[length(current)]])) {
        current <- current[-length(current)]
    }
    vault_rule <- switch(vault, local = "/vault/", tracked = "/vault/site/",
                         none = character())
    block <- c(
               begin,
               "/state.json",
               "/*.log",
               "/.cran-watch.json",
               "/todo/this_week/",
               vault_rule,
               end
    )
    if (length(current)) {
        c(current, "", block)
    } else {
        block
    }
}

.scaffold_validate_name <- function(name) {
    .scaffold_scalar(name, "name", type = "character")
    if (!grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", name)) {
        stop("name must start with a letter or number and contain only ",
             "letters, numbers, underscores, and hyphens", call. = FALSE)
    }
    invisible(name)
}

.scaffold_scalar <- function(x, name, type) {
    valid <- length(x) == 1L && !is.na(x) && typeof(x) == type
    if (type == "character") {
        valid <- valid && nzchar(x)
    }
    if (!valid) {
        stop(name, " must be a non-missing ", type, " scalar", call. = FALSE)
    }
    invisible(x)
}

.scaffold_timezone <- function(timezone) {
    .scaffold_scalar(timezone, "timezone", type = "character")
    timezone
}

.scaffold_yaml_quote <- function(x) {
    paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

.scaffold_systemd_value <- function(x) {
    x <- gsub("\\", "\\\\", x, fixed = TRUE)
    x <- gsub("\"", "\\\"", x, fixed = TRUE)
    gsub("%", "%%", x, fixed = TRUE)
}
