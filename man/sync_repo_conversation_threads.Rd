% tinyrox says don't edit this manually, but it can't stop you!
\name{sync_repo_conversation_threads}
\alias{sync_repo_conversation_threads}
\title{File coding conversations into repository threads}
\usage{
sync_repo_conversation_threads(
  vault = NULL,
  instance_dir = NULL,
  days = 3L,
  max_sessions = 24L,
  model = NULL,
  provider = "auto",
  max_tokens = 1400L,
  matrix_config = NULL,
  invite = character(),
  room_prefix = NULL,
  space = NULL,
  force = FALSE,
  dry_run = FALSE,
  date = Sys.Date(),
  generate_fn = NULL,
  client = NULL
)
}
\arguments{
\item{vault}{Vault root, or NULL to use \code{<instance_dir>/vault}.}

\item{instance_dir}{Instance directory. NULL uses \code{CEREBRO_INSTANCE_DIR}
and then the current working directory.}

\item{days}{Recent-session lookback.}

\item{max_sessions}{Maximum source sessions to inspect.}

\item{model}{LLM model identifier. Required unless \code{generate_fn}
is supplied; there is no default.}

\item{provider}{LLM provider identifier.}

\item{max_tokens}{Maximum generated tokens when supported by the provider.}

\item{matrix_config}{Matrix JSON config, or NULL for corteza resolution.}

\item{invite}{Matrix users invited to newly created rooms, or to
\code{space} once when one is named.}

\item{room_prefix}{Display-name prefix identifying rooms created before
the path-style titles, so they can be renamed. NULL uses the configured
instance name or the instance-directory basename.}

\item{space}{Character or NULL. Name of a Matrix space to file new repo
rooms under. One room per repository is a volume generator -- 45 had
accumulated before anyone counted them -- and a space collapses that to
a single sidebar entry without merging any histories.}

\item{force}{Reprocess unchanged conversations.}

\item{dry_run}{Generate and report without writes or Matrix sends.}

\item{date}{Date used to bound session discovery.}

\item{generate_fn}{Optional test/custom generation function.}

\item{client}{A chat.api client, or NULL to build one from
\code{matrix_config} the first time a room is needed.}
}
\value{
Invisibly, a list describing updated repo threads.
}
\description{
Finds recent Claude Code, Codex, and archived Matrix conversations that can
be tied to a git repository. Each changed conversation is preserved as raw
evidence and summarized into the matching repository wiki page. A linked
notification for conversations originating elsewhere is posted to
one persistent Matrix room per repository. Rooms are titled with the
shell-style repository path (\code{~/repo}, or the absolute path outside
the home directory); rooms created under the older
\code{"<prefix> · <repo>"} title are found and renamed automatically.
The room topic uses corteza's \code{"<repo path> | <description>"}
convention, so tools in the room are scoped to that repository.
}
