#' @title cerebro: AI Agent Toolchain for R
#' @description Meta-package that installs the cerebro AI agent toolchain.
#'
#' @section Packages:
#' \describe{
#'   \item{saber}{AST symbol index, blast radius, package introspection}
#'   \item{pensar}{Knowledge graph from markdown files}
#'   \item{corteza}{Agent runtime and chat loop}
#'   \item{llm.api}{LLM provider connectivity}
#'   \item{hacer}{Optional plain-text nested ToDo planning integration}
#' }
#' @importFrom corteza bot_run_init
#' @importFrom chat.api chat_send
#' @importFrom llm.api chat
#' @importFrom pensar status
#' @importFrom saber projects
NULL
# One import directive per dependency that carries a version floor. R
# checks an Imports floor when the namespace loads only through such a
# directive; a `pkg::` call loads the namespace with no version check
# at all. Depends floors are checked at load regardless.

#' Check which cerebro packages are installed
#'
#' @return A data.frame with columns: package, installed, version.
#' @importFrom utils packageVersion
#' @examples
#' cerebro::status()
#' @export
status <- function() {
    pkgs <- c("saber", "pensar", "corteza", "llm.api", "hacer")
    installed <- vapply(pkgs, function(p) {
        requireNamespace(p, quietly = TRUE)
    }, logical(1L))
    versions <- vapply(pkgs, function(p) {
        if (requireNamespace(p, quietly = TRUE)) {
            as.character(packageVersion(p))
        } else {
            NA_character_
        }
    }, character(1L))
    data.frame(package = pkgs, installed = installed, version = versions,
               row.names = NULL, stringsAsFactors = FALSE)
}
