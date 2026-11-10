% tinyrox says don't edit this manually, but it can't stop you!
\name{briefing_sources}
\alias{briefing_source_hacer_todos}
\alias{briefing_source_saber_git}
\alias{briefing_source_offlist_git}
\alias{briefing_source_git_recent}
\alias{briefing_source_vault_recent}
\alias{briefing_source_fortune_candidates}
\alias{briefing_source_wiki}
\title{briefing_sources}
\usage{
briefing_source_hacer_todos(mode, ctx, params)

briefing_source_saber_git(mode, ctx, params)

briefing_source_offlist_git(mode, ctx, params)

briefing_source_git_recent(mode, ctx, params)

briefing_source_vault_recent(mode, ctx, params)

briefing_source_fortune_candidates(mode, ctx, params)

briefing_source_wiki(mode, ctx, params)
}
\arguments{
\item{mode}{Briefing mode such as daily or weekly.}

\item{ctx}{Shared briefing context environment.}

\item{params}{Source configuration from the instance YAML.}
}
\value{
A named list of briefing sections, or NULL.
}
\description{
briefing_sources
}
