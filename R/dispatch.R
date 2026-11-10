#' Parse CLI flags from argv / commandArgs / env vars
#'
#' Common flags every capability script wants. Recognizes
#' \code{--force}, \code{--dry-run}, and their env equivalents
#' (\code{CEREBRO_FORCE}, \code{CEREBRO_DRY_RUN}). Also supports
#' \code{--instance <path>} / \code{CEREBRO_INSTANCE_DIR}.
#'
#' @param args Character vector. Defaults to \code{argv} (littler) or
#'   \code{commandArgs(trailingOnly = TRUE)}.
#' @return A list with \code{force}, \code{dry_run}, \code{instance_dir}.
#' @export
dispatch_args <- function(args = NULL) {
    if (is.null(args)) {
        args <- if (exists("argv", inherits = TRUE)) {
            get("argv", inherits = TRUE)
        } else {
            commandArgs(trailingOnly = TRUE)
        }
    }

    force <- "--force" %in% args || nzchar(Sys.getenv("CEREBRO_FORCE"))
    dry_run <- "--dry-run" %in% args || nzchar(Sys.getenv("CEREBRO_DRY_RUN"))

    idir <- NULL
    i <- which(args == "--instance")
    if (length(i)) {
        if (i[1] < length(args)) {
            idir <- args[i[1] + 1L]
        }
    }
    if (is.null(idir)) {
        env <- Sys.getenv("CEREBRO_INSTANCE_DIR", "")
        if (nzchar(env)) {
            idir <- env
        }
    }

    list(force = force, dry_run = dry_run, instance_dir = idir)
}
