% tinyrox says don't edit this manually, but it can't stop you!
\name{briefing_source_chat_recent}
\alias{briefing_source_chat_recent}
\title{Recent user-authored conversation evidence}
\usage{
briefing_source_chat_recent(mode, ctx, params)
}
\arguments{
\item{mode}{Character briefing mode. Present for the briefing-source API.}

\item{ctx}{Briefing context environment.}

\item{params}{Named source configuration.}
}
\value{
A named list containing recent chat evidence, or NULL.
}
\description{
Reads bounded, recent conversations from Matrix archives and the local
Claude Code and Codex session stores. Tool calls, tool results, reasoning,
developer messages, generated environment context, and assistant-only
Matrix notifications are excluded.
}
