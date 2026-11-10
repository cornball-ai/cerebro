% tinyrox says don't edit this manually, but it can't stop you!
\name{prepare_briefing}
\alias{prepare_briefing}
\title{Prepare a briefing for later delivery}
\usage{
prepare_briefing(
  name,
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
Synthesizes a briefing immediately and stores the dated result in the
instance state. It does not send a Matrix message. A second call on the same
date is skipped unless \code{force} is true.
}
