# Routine capabilities: the actions a config.yaml routine can name.
#
# A builder takes the routine entry `r`, the config's `briefings:` map,
# the whole config `cfg` and the instance dir, and returns the
# zero-argument function the scheduler runs, or NULL after saying why
# the entry cannot be registered. Each builder sits beside the function
# it wraps (`.cap_briefing` in briefing.R, `.cap_sync` in sync.R, ...);
# the runtime glue ones are below. The registry is a function because R
# sources this file before most builders exist (alphabetical collation).
.capabilities <- function() {
    list(briefing = .cap_briefing, briefing_prepare = .cap_briefing_prepare,
         briefing_send = .cap_briefing_send, reconcile = .cap_reconcile,
         sync = .cap_sync, matrix_archive = .cap_matrix_archive,
         lifecycle_sweep = .cap_lifecycle_sweep,
         vault_check = .cap_vault_check, shell = .cap_shell, r = .cap_r)
}

# Resolve a routine entry to the function the scheduler runs. No
# capability, or one the registry does not know, is a config error:
# stop at registration so a typo halts the boot instead of dropping the
# routine quietly. A builder that declines returns NULL; the caller
# skips those.
.build_routine_fn <- function(r, briefings, cfg, instance_dir) {
    caps <- .capabilities()
    cap <- r$capability
    nm <- r$name %||% "(unnamed)"
    if (is.null(cap)) {
        stop("cerebro: routine ", nm, " has no capability; valid: ",
             paste(names(caps), collapse = ", "), call. = FALSE)
    }
    if (length(cap) != 1L || !is.character(cap) || !cap %in% names(caps)) {
        stop("cerebro: unknown capability '", format(cap), "' for routine ",
             nm, "; valid: ", paste(names(caps), collapse = ", "),
             call. = FALSE)
    }
    fn <- caps[[cap]](r, briefings, cfg, instance_dir)
    if (is.null(fn)) {
        return(NULL)
    }
    .with_pre_hook(fn, .build_pre_hook(r$pre), nm)
}

# Run the entry's `pre:` expressions, then the capability. A failing pre
# is reported and the capability still runs.
.with_pre_hook <- function(fn, pre, name) {
    if (is.null(pre)) {
        return(fn)
    }
    force(fn)
    force(name)
    function() {
        tryCatch(pre(), error = function(e) {
            message(sprintf("cerebro: pre hook for %s failed: %s", name,
                            conditionMessage(e)))
        })
        fn()
    }
}

# Ask the separately running corteza bot to flush and archive its
# Matrix session.
.cap_matrix_archive <- function(r, briefings, cfg, instance_dir) {
    function() {
        if (requireNamespace("corteza", quietly = TRUE)) {
            corteza::bot_request_flush()
        }
    }
}

# One shell command. `timeout` (seconds) kills a wedged command instead
# of letting it block the scheduler; 0 means none. A bad timeout
# declines at registration rather than failing at fire time.
.cap_shell <- function(r, briefings, cfg, instance_dir) {
    cmd <- r$command
    if (is.null(cmd) || !nzchar(cmd)) {
        message("cerebro: routine ", r$name, " shell missing command")
        return(NULL)
    }
    tmo <- r$timeout %||% 0
    if (is.atomic(tmo) && length(tmo) == 1L) {
        tmo <- suppressWarnings(as.numeric(tmo))
    } else {
        tmo <- NA_real_
    }
    if (is.na(tmo) || tmo < 0) {
        message("cerebro: routine ", r$name, " invalid timeout (",
                format(r$timeout), "); not registering")
        return(NULL)
    }
    function() .run_shell_command(cmd, timeout = tmo)
}

# An R expression evaluated in the global environment.
.cap_r <- function(r, briefings, cfg, instance_dir) {
    expr <- r$expr
    if (is.null(expr) || !nzchar(expr)) {
        message("cerebro: routine ", r$name, " r missing expr")
        return(NULL)
    }
    function() eval(parse(text = expr), envir = globalenv())
}

.run_shell_command <- function(cmd, timeout = 0) {
    windows <- identical(.Platform$OS.type, "windows")
    shell <- if (windows) {
        Sys.getenv("COMSPEC", unset = "cmd.exe")
    } else {
        Sys.getenv("SHELL", unset = "/bin/sh")
    }
    if (windows) {
        flag <- "/c"
    } else {
        flag <- "-c"
    }
    # timeout > 0 kills a wedged command (status 124) instead of letting
    # it block the scheduler + Matrix loop forever
    status <- system2(shell, c(flag, shQuote(cmd)), timeout = timeout)
    if (is.null(status)) {
        status <- 0L
    }
    status <- as.integer(status)
    if (is.na(status) || status != 0L) {
        stop("shell command failed with status ", status, ": ", cmd,
             call. = FALSE)
    }
    invisible(status)
}

.build_pre_hook <- function(pre) {
    if (is.null(pre)) {
        return(NULL)
    }
    if (is.character(pre) && length(pre) == 1L) {
        expr <- pre
        force(expr)
        return(function() eval(parse(text = expr), envir = globalenv()))
    }
    if (is.list(pre)) {
        # list of expressions; run sequentially
        exprs <- vapply(pre, function(p) {
            if (is.character(p) && length(p) == 1L) p else ""
        }, character(1))
        exprs <- exprs[nzchar(exprs)]
        force(exprs)
        return(function() {
            for (e in exprs) {
                eval(parse(text = e), envir = globalenv())
            }
        })
    }
    NULL
}
