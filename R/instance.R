# A cerebro "instance" is a directory holding a config.yaml plus runtime
# state (state.json, logs). cerebro
# itself ships no instance — it ships the machinery, the instance ships
# the data.

#' Standard paths for a named cerebro agent
#'
#' @param name Character. Agent/instance name under \code{root}, e.g.
#'   \code{"myassistant"} resolves under \code{~/myassistant}.
#' @param root Character. Parent directory for named agents.
#' @return Named list of standard paths.
#' @export
agent_paths <- function(name, root = "~") {
    if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
        stop("name must be a non-empty character scalar", call. = FALSE)
    }
    if (grepl("[/\\\\]", name)) {
        stop("name must be a simple directory name, not a path", call. = FALSE)
    }
    base <- file.path(path.expand(root), name)
    list(instance_dir = base, config = file.path(base, "config.yaml"),
         todo = file.path(base, "todo"),
         todo_current = file.path(base, "todo", "this_week"),
         todo_archive = file.path(base, "todo", "archive"),
         wiki = file.path(base, "wiki"),
         wiki_pages = file.path(base, "wiki", "wiki"),
         wiki_raw = file.path(base, "wiki", "raw"))
}

#' Create or activate a named cerebro agent instance
#'
#' Creates the standard directory layout used by instance-relative
#' briefing defaults: \code{todo/this_week}, \code{todo/archive},
#' \code{wiki/wiki}, and \code{wiki/raw}.
#'
#' @param name Character. Agent/instance name under \code{root}.
#' @param root Character. Parent directory for named agents.
#' @param create Logical. Create the standard directories.
#' @param set_active Logical. Set option/env so this becomes the active
#'   cerebro instance for the current R process.
#' @return Named list from \code{\link{agent_paths}}.
#' @export
instantiate_agent <- function(name, root = "~", create = TRUE,
                              set_active = TRUE) {
    paths <- agent_paths(name, root = root)
    if (isTRUE(create)) {
        dirs <- unlist(paths[c("instance_dir", "todo_current",
                               "todo_archive", "wiki_pages", "wiki_raw")],
                       use.names = FALSE)
        for (d in dirs) {
            dir.create(d, recursive = TRUE, showWarnings = FALSE)
        }
    }
    if (isTRUE(set_active)) {
        options(cerebro.instance_dir = paths$instance_dir)
        Sys.setenv(CEREBRO_INSTANCE_DIR = paths$instance_dir)
    }
    paths
}

#' Resolve the active instance directory
#'
#' Order: \code{instance_dir} arg, \code{getOption("cerebro.instance_dir")},
#' env \code{CEREBRO_INSTANCE_DIR}. Errors if none of those resolve to an
#' existing directory.
#'
#' @param instance_dir Character or NULL.
#' @return Normalized absolute path.
#' @export
instance_dir <- function(instance_dir = NULL) {
    cand <- instance_dir %||%
    getOption("cerebro.instance_dir", NULL) %||%
    Sys.getenv("CEREBRO_INSTANCE_DIR", "")
    if (!nzchar(cand)) {
        stop("No cerebro instance dir set. Pass instance_dir, set ",
             "options(cerebro.instance_dir=) or env CEREBRO_INSTANCE_DIR.",
             call. = FALSE)
    }
    cand <- path.expand(cand)
    if (!dir.exists(cand)) {
        stop("instance dir does not exist: ", cand, call. = FALSE)
    }
    normalizePath(cand, mustWork = TRUE)
}

#' Path to an instance's state.json
#' @param instance_dir Character or NULL. See \code{\link{instance_dir}}.
#' @return Character path. May not yet exist.
#' @export
instance_state_path <- function(instance_dir = NULL) {
    file.path(instance_dir(instance_dir), "state.json")
}

#' Path to an instance's config.yaml
#' @param instance_dir Character or NULL.
#' @return Character path.
#' @export
instance_config_path <- function(instance_dir = NULL) {
    file.path(instance_dir(instance_dir), "config.yaml")
}

#' Read an instance's state.json (returns empty list if absent)
#' @param instance_dir Character or NULL.
#' @return Named list.
#' @export
read_instance_state <- function(instance_dir = NULL) {
    p <- instance_state_path(instance_dir)
    if (!file.exists(p)) {
        return(list())
    }
    tryCatch(jsonlite::fromJSON(p, simplifyVector = TRUE,
                                simplifyDataFrame = FALSE),
             error = function(e) list())
}

#' Write an instance's state.json
#' @param state Named list.
#' @param instance_dir Character or NULL.
#' @return Invisible path.
#' @export
write_instance_state <- function(state, instance_dir = NULL) {
    p <- instance_state_path(instance_dir)
    dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE)
    writeLines(jsonlite::toJSON(state, auto_unbox = TRUE, pretty = TRUE), p)
    invisible(p)
}

#' Update one or more keys in an instance's state.json
#'
#' Read-modify-write convenience. Pass \code{NULL} as a value to remove a
#' key.
#'
#' @param ... Named arguments: \code{key = value}.
#' @param instance_dir Character or NULL.
#' @return Updated state, invisibly.
#' @export
update_instance_state <- function(..., instance_dir = NULL) {
    s <- read_instance_state(instance_dir)
    updates <- list(...)
    for (k in names(updates)) {
        s[[k]] <- updates[[k]]
    }
    write_instance_state(s, instance_dir)
    invisible(s)
}

#' Read an instance's config.yaml
#'
#' Resolves \code{$instance_dir/...} placeholders in string values to the
#' instance's absolute path. Returns the parsed list as-is otherwise.
#'
#' @param instance_dir Character or NULL.
#' @return Parsed config list.
#' @export
read_instance_config <- function(instance_dir = NULL) {
    if (!requireNamespace("yaml", quietly = TRUE)) {
        stop("yaml package required to read instance config", call. = FALSE)
    }
    p <- instance_config_path(instance_dir)
    if (!file.exists(p)) {
        stop("instance config not found: ", p, call. = FALSE)
    }
    cfg <- yaml::read_yaml(p)
    .resolve_instance_paths(cfg, instance_dir(instance_dir))
}

# Recursively replace `$instance_dir` and `~` prefixes in string values.
.resolve_instance_paths <- function(x, idir) {
    if (is.character(x)) {
        out <- vapply(x, function(s) {
            if (!nzchar(s)) return(s)
            s <- sub("^\\$instance_dir(/|$)", paste0(idir, "\\1"), s)
            if (startsWith(s, "~")) s <- path.expand(s)
            s
        }, character(1), USE.NAMES = FALSE)
        return(out)
    }
    if (is.list(x)) {
        return(lapply(x, .resolve_instance_paths, idir = idir))
    }
    x
}
