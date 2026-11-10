% tinyrox says don't edit this manually, but it can't stop you!
\name{reconcile_todo}
\alias{reconcile_todo}
\title{Reconcile today's todo list against what actually shipped}
\usage{
reconcile_todo(
  name = "reconcile",
  root,
  room = NULL,
  sources = list(),
  system_prompt,
  model,
  provider = "anthropic",
  max_tokens = NULL,
  force = FALSE,
  dry_run = FALSE,
  instance_dir = NULL,
  date = Sys.Date()
)
}
\arguments{
\item{name}{Character. Idempotency key + log label.}

\item{root}{Character. Directory holding the live daily todo file.}

\item{room}{Character or NULL. Matrix room for the summary.}

\item{sources}{List of briefing source specs used to build git evidence.}

\item{system_prompt}{Character. Reconcile system prompt.}

\item{model}{Character. LLM model id. Required; there is no default.}

\item{provider}{Character. LLM provider id.}

\item{max_tokens}{Integer or NULL.}

\item{force}{Logical. Override once-per-day guard.}

\item{dry_run}{Logical. Print the plan instead of writing/sending.}

\item{instance_dir}{Character or NULL.}

\item{date}{Date. For testing; defaults to today.}
}
\value{
The summary text, invisibly. NULL when skipped.
}
\description{
A weekday-morning capability: read today's section of the daily todo,
gather the same git evidence the briefing uses, and ask the LLM for a
best guess of which open items actually shipped. The model returns only
structured ops (line numbers to mark done, items it is unsure about, and
off-list shipped work); R applies them deterministically with surgical
bracket edits. The model cannot reword, reorder, delete, or touch
\code{[!]} blocked items.
}
\details{
Whatever it cannot judge confidently is left untouched and reported under
"unclear" in the Matrix summary. Idempotent per day on \code{name}.

}
