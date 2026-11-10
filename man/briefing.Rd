% tinyrox says don't edit this manually, but it can't stop you!
\name{briefing}
\alias{briefing}
\title{Run a briefing capability}
\usage{
briefing(
  name,
  room = NULL,
  sources = list(),
  system_prompt,
  mode_hints = list(),
  mode_fn = briefing_mode_default,
  model,
  provider = "anthropic",
  max_tokens = 800L,
  force = FALSE,
  dry_run = FALSE,
  instance_dir = NULL,
  date = Sys.Date()
)
}
\arguments{
\item{name}{Character. Idempotency key + log label.}

\item{room}{Character or NULL. Matrix room name or id (see
\code{\link{matrix_send}}).}

\item{sources}{List of source specs. Each is a list with at least
\code{type = "<source_name>"} plus the source's own params.}

\item{system_prompt}{Character. Base system prompt.}

\item{mode_hints}{Named list. mode -> text appended to system prompt.}

\item{mode_fn}{Function (Date) -> character. Defaults to
\code{\link{briefing_mode_default}}.}

\item{model}{Character. LLM model id. Required; there is no default.}

\item{provider}{Character. LLM provider id.}

\item{max_tokens}{Integer.}

\item{force}{Logical. Override once-per-day guard.}

\item{dry_run}{Logical. Print synthesized briefing instead of sending.}

\item{instance_dir}{Character or NULL.}

\item{date}{Date. For testing; defaults to today.}
}
\value{
The synthesized text, invisibly. NULL when skipped.
}
\description{
The orchestrator: select a mode, run the configured sources, assemble
a prompt, synthesize via llm.api, send via matrix. Idempotent per day
on \code{name}, with \code{force} / \code{dry_run} overrides.
}
\details{
Sources are run in order. Each gets a shared \code{ctx} environment
so later sources can read state stashed by earlier ones (for
instance, \code{saber_git} reads \code{ctx$todo_text} populated by
\code{hacer_todos}).

}
