% tinyrox says don't edit this manually, but it can't stop you!
\name{sync_general_conversation_threads}
\alias{sync_general_conversation_threads}
\title{File general conversations into topical threads}
\usage{
sync_general_conversation_threads(
  vault = NULL,
  instance_dir = NULL,
  matrix_roots = NULL,
  days = 7L,
  max_sessions = 18L,
  reuse_threshold = 0.55,
  create_threshold = 0.72,
  create_min_turns = 3L,
  create_min_chars = 400L,
  model = NULL,
  provider = "auto",
  max_tokens = 1600L,
  matrix_config = NULL,
  invite = character(),
  room_prefix = NULL,
  space = NULL,
  force = FALSE,
  dry_run = FALSE,
  date = Sys.Date(),
  generate_fn = NULL,
  exclude_matrix_rooms = character(),
  client = NULL
)
}
\arguments{
\item{vault}{Personal pensar vault, or NULL to use \code{<instance_dir>/vault}.}

\item{instance_dir}{Instance directory. NULL uses \code{CEREBRO_INSTANCE_DIR}
and then the current working directory.}

\item{matrix_roots}{Matrix raw archives to inspect.}

\item{days}{Recent-session lookback.}

\item{max_sessions}{Maximum source sessions to inspect.}

\item{reuse_threshold}{Minimum confidence for reusing an existing topic.}

\item{create_threshold}{Minimum confidence for a new topic and room.}

\item{create_min_turns}{Substance floor for a new topic and room: the
conversation needs at least this many user turns, or it needs to
clear \code{create_min_chars}. Routing into an existing topic is
never gated by the floor.}

\item{create_min_chars}{The other arm of the substance floor: at
least this many characters of user-authored text opens a new topic
regardless of turn count.}

\item{model}{LLM model identifier. Required unless \code{generate_fn}
is supplied; there is no default.}

\item{provider}{LLM provider identifier.}

\item{max_tokens}{Maximum generated tokens when supported by the provider.}

\item{matrix_config}{Matrix JSON config, or NULL for corteza resolution.}

\item{invite}{Matrix users invited to newly created rooms, or to
\code{space} once when one is named.}

\item{room_prefix}{Legacy display-name prefix. Rooms are named for their
topic alone; this is the older \code{"<prefix> · <topic>"} form,
still used to find and rename rooms created under it, and to word the
room topic. NULL uses the configured instance name or the
instance-directory basename.}

\item{space}{Character or NULL. Name of a Matrix space to file new topic
rooms under. With one, \code{invite} goes to the space once instead of
to every room: eleven new topic rooms in a morning is eleven invites
without it, and one with.}

\item{force}{Reprocess unchanged conversations.}

\item{dry_run}{Generate and report without writes or Matrix sends.}

\item{date}{Date used to bound session discovery.}

\item{generate_fn}{Optional test/custom generation function.}

\item{exclude_matrix_rooms}{Character vector of Matrix room ids whose
archives are never routed. \code{matrix_roots} cannot express this:
it selects directories, and every room in an instance shares one.
Matching is on the room id recorded in the archive, so a room can be
suppressed without its name or its files changing.}

\item{client}{A chat.api client, or NULL to build one from
\code{matrix_config} the first time a room is needed.}
}
\value{
Invisibly, a list describing updated topical threads.
}
\description{
Reads recent archived Matrix conversations plus any
local Claude Code or Codex sessions not rooted in a git repository. An LLM
routes each changed conversation to an existing personal-wiki topic and
Matrix room when possible. A durable new topic creates one wiki page and one
private topical room only when the routing confidence clears
\code{create_threshold}. A linked notification resurfaces the room only for
records originating elsewhere. Conversations already in that room, including
its threads, still update the wiki and are marked processed without a post.
}
