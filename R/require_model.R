# cerebro names no default LLM model: a model id goes stale, and which
# model an instance runs is the instance's choice. Every function that
# calls a model checks for one first, so a missing model fails before
# any work is done. `what` names the caller for a config-file error.
.require_model <- function(model, what = NULL) {
    ok <- !missing(model) && is.character(model) && length(model) == 1L &&
        !is.na(model) && nzchar(model)
    if (ok) {
        return(invisible(model))
    }
    if (is.null(what)) {
        stop("cerebro: `model` is required; pass an LLM model id ",
             "(cerebro picks no default)", call. = FALSE)
    }
    stop("cerebro: ", what, " needs a `model`; set an LLM model id ",
         "(cerebro picks no default)", call. = FALSE)
}
