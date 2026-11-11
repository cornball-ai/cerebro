# cerebro 0.2.0.1

- `ensure_matrix_space()` lowers the power level for filing a room under
  a space it creates to the member level, so the other personas sharing
  the space can file into it. The server's default reserved that for the
  creator, and from room version 12 for the creator alone, so every
  other persona's filing failed and fell back to the per-room invite the
  space exists to remove.

# cerebro 0.2.0

- cerebro names no default LLM model. `briefing()`, `prepare_briefing()`,
  `reconcile_todo()` and `sweep_segments()` lose their
  `"claude-sonnet-4-6"` default and take a required `model`.
  `sync_repo_conversation_threads()` and
  `sync_general_conversation_threads()` require one unless `generate_fn`
  is supplied; they used to pass `NULL` through to llm.api's own default.
  A call without a model stops before any work is done.
- The `briefing`, `briefing_prepare`, `reconcile` and `lifecycle_sweep`
  capabilities check for a model at registration. A `briefings:` entry or
  a `lifecycle_sweep` routine without `model:` now stops `run()` with a
  message naming the entry. Provider defaults are unchanged.

# cerebro 0.1.1.34

- A vault or instance dir behind a symlink resolves the same way whether
  or not the path under it exists yet (`.resolve_path()`).
  `.thread_relative()` no longer refuses a page that is not written yet
  as outside a resolved vault, and `scaffold_persona()` embeds the same
  instance path on the creating call as on every later one, so a
  symlinked instance dir no longer conflicts with itself from the second
  call on. macOS's tempdir is such a path.

# cerebro 0.1.1.33

- Routine capabilities are a registry, `cerebro:::.capabilities()`,
  with each builder beside the function it wraps (`.cap_sync()` in
  sync.R, `.cap_lifecycle_sweep()` in lifecycle.R, ...) and
  `.build_routine_fn()` a lookup over it. A `capability:` the registry
  does not know, or a routine without one, stops registration with the
  valid names listed; it used to log and drop the routine. `pre:` runs
  before every capability rather than only the briefing, reconcile and
  sync ones, and its failure message names the routine.

# cerebro 0.1.1.32

- The namespace imports from chat.api, so its version floor is checked
  when cerebro loads. R enforces an Imports floor only through a
  NAMESPACE import directive; `chat.api::` calls alone loaded whichever
  copy came first on the library path, floor or no floor. The corteza
  import is `bot_run_init()`, which `run()` calls, rather than the
  deprecated `matrix_run()`.

# cerebro 0.1.1.31

- **The lifecycle's open half lives here.** `segment_open()` is the
  room a cleared conversation becomes, for the rooms the bot config
  lists in `segment_rooms`; `fold_lookup()` is the archive a folded
  thread continues. Both were corteza's (`bot_segment_from_clear()`,
  `bot_thread_archive()`) and are now the `on_clear` and
  `thread_context` hooks `run()` hands `corteza::bot_run_init()`, so
  corteza runs the conversation and cerebro decides what becomes of
  it. corteza 0.7.1.77 or later.

# cerebro 0.1.1.30

- **Chat goes through chat.api.** Briefing sends, the fold sweep, topic
  and space rooms, and room lookup all use one chat.api client, built
  the way corteza builds the bot's own, instead of three Matrix paths
  (mx.api sessions, `mx.client::mx_send_text()`, `corteza::bot_send()`).
  `matrix_send()`, `resolve_room()`, `ensure_matrix_room()`,
  `ensure_matrix_space()`, `sweep_segments()` and the two
  `sync_*_conversation_threads()` take a `client` argument, which is
  how the tests run without a homeserver. chat.api is an Import, at
  0.1.0.3 or later for `chat_channel_invite()`, which the fallback
  invite when restricted join rules fail goes through, and for
  `chat_set_identity(avatar =)`, which the startup avatar upload from
  0.1.1.28 goes through. The topic-room stamp from 0.1.1.29 uses the
  same client. mx.api is no longer needed.
- **The conversation lifecycle has a written contract.** The
  `lifecycle` vignette records the state events corteza writes at
  `/clear` and cerebro writes at fold time, the fold index a thread
  reply is answered from, the fold policy and its knobs, and what a
  client has to read to group rooms by state.
- Dependencies install from CRAN and the cornball.ai drat, which now
  carries corteza, saber, llm.api and pensar. The `Remotes` field is
  gone, and CI no longer needs a GitHub token.
- Session timestamps parsed from the Claude Code and Codex stores
  carry no zone name, so comparing them with the local-zone lookback
  window no longer warns about inconsistent `tzone` attributes, which
  it did once per turn.
- The two non-ASCII string literals in `conversation_sync.R` are
  `\u` escapes, which clears the check warning.

# cerebro 0.1.1.29

- Keep conversation filing quiet in the source room. Both topical and
  repository filers preserve the full wiki update, but notify a destination
  only about records originating elsewhere. Mixed groups send a brief linked
  handoff for those records; same-room and same-room-thread records are marked
  processed without a post. Sources with no room metadata remain eligible.

# cerebro 0.1.1.28

- **A bot sets its own Matrix avatar from the instance config.** Add
  `avatar: /path/to/image` under `matrix:` in an instance's
  `config.yaml` and `run()` uploads it and sets the account's avatar
  once at startup, right after the long-poll comes up. Identity (the
  avatar, like the display name) belongs to the bot layer here rather
  than to corteza's turn engine; only the live model-badge rename stays
  in corteza. Best-effort (a bad path or an unreachable homeserver logs
  and never blocks the loop) and idempotent (the file's md5 is recorded
  in `state.json`, so an unchanged picture is not re-uploaded on every
  restart).

# cerebro 0.1.1.27

- **A topical thread room is a fold destination.**
  `sync_general_conversation_threads()` now stamps each general
  thread room with `ai.cornball.lifecycle state=topic`, so the fold
  sweep can enumerate it. Without the stamp `.topic_rooms()` saw only
  rooms the sweep had created itself, and classified against a
  near-empty menu: a conversation opened a new room while the existing
  thread that should have taken it was invisible. Stamping happens after the room is resolved, so rooms
  predating the fold subsystem are repaired on the sweep that next
  touches them. The Matrix session is built on first use. Repo threads
  stay unstamped.

- **A room can be marked "never a fold destination".** Any lifecycle
  state other than `topic` is now a veto that stamping leaves alone, so
  a room stamped `excluded` stays out of the classifier's menu instead
  of being re-stamped by the next sweep that touches its page. Renaming
  a room corrects a topic's label; this is how a room says it is not a
  topic at all. The case that forced it is a curated profile page: a
  fact sheet rather than a conversation bucket, and the most attractive
  label on the menu for any personal conversation. A missing or empty
  state is not a veto.

- **A refused topic stamp no longer abandons a fold sweep.** Stamping
  now goes through one tolerant helper that reports and swallows the
  Matrix failure, in the fold path as well as the thread path. Rooms
  resolve by name, so a destination can be a room the user created and
  the bot joined as a plain member; state events default to power level
  50, so the stamp comes back `M_FORBIDDEN` while the room is still a
  perfectly good destination. The sweep resolves every destination up
  front so one failure cannot scatter the folds, and erroring on the
  stamp defeated that.

# cerebro 0.1.1.26

- **Routines accept five-field cron.** `schedule:` now takes the
  grammar Kubernetes CronJob and Vixie cron use -- `"*/30 * * * *"`,
  `"0,30 9-17 * * Mon-Fri"`, `"0 9 1 * Mon"` -- with lists, ranges,
  steps, month and weekday names, and Sunday as 0 or 7. The named
  forms (`hourly`, `daily HH:MM`, `DAY HH:MM`, `DAY1-DAY2 HH:MM`) stay
  as they are; they could not express anything sub-hourly, which is
  what the ARC watch room needed. Vixie's day rule is kept: with both
  day-of-month and day-of-week restricted, either matching fires.
  `next_fire()` walks minutes, skipping by the hour and by the day when
  a field cannot match, and builds the day jump as a local timestamp --
  `as.POSIXct()` on a Date is midnight UTC, which west of Greenwich is
  the previous evening.

# cerebro 0.1.1.25

- **A conversation must show substance before it opens a topic room.**
  `sync_general_conversation_threads()` gains `create_min_turns` (3) and
  `create_min_chars` (400): a new topic needs that many user turns, or
  that many characters of user-authored text -- either clears. A
  two-line "can you see this?" ping can carry a confident,
  durable-sounding theme, and it used to mint a room; confidence
  measures how sure the router is, and the topic gate what it was sure
  of, but neither can see size. Routing into an existing topic is not
  gated: filing a small exchange into a room that exists is the cheap,
  useful case -- opening a room for it is the noise.

# cerebro 0.1.1.24

- **A disposable git worktree no longer gets a permanent room.**
  `rev-parse --show-toplevel` inside a linked worktree returns the
  worktree, so a session run in one was filed under that path.
  Worktrees now resolve to the repository they belong to, which keeps
  the conversation threaded rather than dropping it.

# cerebro 0.1.1.23

- **Topic rooms are named for their topic.** `Travel`, not
  `mybot · Travel`: the prefix said the same thing in every room of a
  sidebar that is already the persona's. The prefixed form is passed as
  `legacy_name`, so existing rooms are renamed and reused rather than
  duplicated alongside fresh unprefixed ones.

- **One invite instead of one per room.** `ensure_matrix_space()` creates
  a Matrix space, and `sync_general_conversation_threads()` and
  `sweep_segments()` gain `space` — new topic rooms are filed under it
  and opened to its members via restricted join rules, rather than each
  sending its own invite. Eleven topic rooms in one morning was eleven
  invites. Filing needs room version 9 or newer; where the join-rule
  write fails, the direct invite comes back, because a room nobody can
  enter is worse than the notification.

- **A router that finds no topic no longer opens a room for saying so.**
  It had no structural way to report the absence, so it reported it in
  prose, in the topic field, and `mybot · No durable topical thread`
  became a room. The confidence gate could not catch it: confidence
  measures how sure the model is, not what it was sure of. The router is
  now asked for an empty topic in that case, an empty topic leaves the
  conversation as raw evidence, and a guard rejects the prose forms. The
  `"General"` fallback for a missing topic is gone with it — a label
  nothing chose, with a room to match.

# cerebro 0.1.1.22

- **Each fold is indexed by its thread root.** `sweep_segments()` also
  writes an `ai.cornball.fold` state event into the topic room, keyed
  by the thread root and holding the segment room and its archive
  pointer, so a reply in that thread can be answered with the
  conversation it continues without scanning the room's history.

# cerebro 0.1.1.21

- **Folds classify into topics, confidence-gated.** Each fold is
  classified against the existing topic rooms with a self-rated
  confidence: at or above `topic_confidence` (0.7) it files silently,
  a confidently proposed new topic is created and announced as a
  notice, and everything else — low confidence, unreadable replies,
  provider outages — goes to `Unsorted`, silently. Never a question.

# cerebro 0.1.1.20

- **`sweep_segments()` folds stale conversation segments into topic
  rooms.** Segments fold once idle past `fold_after_days` (default 7,
  keyed to the last human message) or oldest-first under
  `max_segments` (12) sidebar pressure, guarded by `fold_min_days`
  (2). A fold posts the segment's name and vault pointer to a topic
  room as an `m.notice` thread root, flips the lifecycle event to
  `folded` with a `folded_into` pointer, and tombstones the segment
  room. Everything files into `Unsorted` for now; classification is a
  seam. Registered as the `lifecycle_sweep` routine capability.

# cerebro 0.1.1.19

- **Routine traffic sends as `m.notice`.** `matrix_send()` gains a
  `msgtype` parameter, forwarded to all three backends and defaulting to
  `m.notice`: proactive persona traffic renders formatted but no longer
  pings. Pass `msgtype = "m.text"` for a message that should notify.

# cerebro 0.1.1.17

- **`sync_general_conversation_threads()` gains `exclude_matrix_rooms`.**
  A room can now be suppressed entirely by id. `matrix_roots` could not
  express this, since it selects directories and every room in an
  instance shares one. Applied before the collection budget is spent, so
  a suppressed room cannot crowd out wanted material. Unset by default.

# cerebro 0.1.1.16

- **Conversation updates are grouped by destination.** Distinct
  conversations routed to the same topic each wrote their own page block
  and sent their own Matrix message. Routing and writing are now
  separate phases: one page append, one room resolution, and one message
  per destination, with state advanced for every contributing record
  only after a successful write. Grouped updates send a digest rather
  than the concatenated summaries; the full per-source sections live on
  the page.

# cerebro 0.1.1.15

- **Duplicate conversation sources collapse before routing.** The same
  room is archived repeatedly, so one conversation arrived as many
  sources, each summarized with its own LLM call and each posting its
  own message to the same destination. Sessions are now deduplicated on
  full content identity before routing. Against a live vault, 284 usable
  sources collapse to 167.

# cerebro 0.1.1.14

- **Generated summaries no longer feed themselves.** Thread rooms are
  archived like any other room, so a summary posted into one became a
  conversation source for the next run, which summarized it and posted
  again. Matrix archive parsing now discards assistant turns opening
  with the summary prefix, which covers historical archives and future
  ones alike.

# cerebro 0.1.1.13

- Briefing sources (`saber_git`, `offlist_git`, `git_recent`) now use
  saber's exported git primitives (`saber::git_commit_count_since()`,
  `saber::git_log_since()`) instead of private copies. Behavior is
  unchanged except commit counting is now rev-parse guarded, so
  worktrees and dubious-ownership repos no longer miscount as zero.
  Requires saber >= 0.7.2.2.

# cerebro 0.1.1.12

- Shell routine `timeout:` is validated at registration; an invalid
  value (non-numeric, non-scalar, negative) skips the routine with a
  startup message instead of failing when it first fires.

# cerebro 0.1.1.11

- Routine `last_fired` state is instance-scoped
  (`routines-<instance>.json`); previously all instances shared one
  file and the last daemon to write clobbered the others. Instances
  re-watermark once on first start after upgrading.
- Shell routines accept `timeout:` (seconds) so a wedged command is
  killed (status 124) instead of blocking the scheduler and Matrix
  loop.

# cerebro 0.1.1.10

- Matrix chat threads route by room ID from archive frontmatter instead
  of display name; room names and working directories are kept as
  metadata, and existing rooms are validated by ID.
- `register_from_config()` skips routine entries carrying
  `enabled: false` and logs the skip at startup, so retired routines can
  be parked as live YAML instead of comment slabs.
- Matrix archives with no frontmatter or room metadata fall back to the
  filename label again (regression in the room-id change).

# cerebro 0.1.1.9

- Repo thread rooms are titled with the shell-style repository path
  (`~/pensar`; the absolute path outside the home directory). Rooms
  created under the older `<prefix> · <repo>` title are found and renamed
  automatically on the next sync, and their `state.json` room-cache entry
  is re-keyed (room-cache writes no longer append duplicate keys).
  General topical rooms keep the `<prefix> · <topic>` format.

# cerebro 0.1.1.8

- Fixed the `renviron` default under littler: it sets `R_ENVIRON_USER=""`
  to suppress R's startup files, which silently disabled Renviron loading.
  Empty now falls through to `~/.Renviron`.

# cerebro 0.1.1.7

- The Renviron location `run()` loads is now configurable: a top-level
  `renviron:` key in `config.yaml` wins, then `R_ENVIRON_USER`, then
  `~/.Renviron`; `renviron: false` (or passing `NULL`) skips loading.

# cerebro 0.1.1.6

- `run()` loads `~/.Renviron` at startup (new `renviron` parameter, `NULL` to
  skip), so daemon frontends that skip R's startup files -- littler under
  systemd -- still see API keys after a reboot.

# cerebro 0.1.1.5

- `hacer` is now an optional integration supplied through the cornball.ai drat
  repository. Core cerebro installation and scaffolding no longer require it;
  `sync_todo()` checks for it when invoked.

- Fixed local-calendar time windows for chat and vault evidence, removed
  persona-specific todo-sync defaults, consolidated `%||%`, and routed shell
  capabilities through `system2()`.

- Added deferred briefings: `briefing_prepare` synthesizes and stores a dated
  draft, and `briefing_send` delivers that exact draft later while refusing
  missing or stale content.
- Added todo-independent `git_recent`, bounded `vault_recent`, and relevance-
  scored `fortune_candidates` briefing sources for synthesis workflows.
- Added bounded `chat_recent` evidence from Matrix, Claude Code, and Codex;
  generated context, tool traces, and assistant-only notifications are omitted.
- Added durable conversation threads. Coding sessions are filed into one
  source-cited wiki thread and persistent Matrix room per repository. General
  conversations are routed into existing personal topics when possible, and a
  new topic/room is created only above a configurable confidence threshold.
- Conversation-sync exports now derive generic defaults from the active instance
  instead of embedding persona names or host-specific paths. Persona
  deployments pass vaults, Matrix configs, invitees, and room prefixes explicitly.
- `scaffold_persona()` can optionally generate `<name>.timer` when given an
  explicit systemd `OnCalendar` value; it still invents no schedules.

- Matrix transport now supports explicit persona configs and idempotent private
  room creation with corteza's existing repo-cwd topic convention.

# cerebro 0.1.1.4

- Added `scaffold_persona()` and package-owned templates for creating or
  updating the canonical persona structure. The scaffold is dry-run capable,
  preserves locally changed files by default, manages only a marked
  `.gitignore` block, and leaves identity, providers, credentials, routines,
  live todos, and generated output to the instance.
- New persona services set `CEREBRO_INSTANCE_DIR` and `HACER_REPO`; todo
  metadata uses hacer's current `todo/config.yaml` interface rather than the
  duplicated legacy `hacer_config.R` files.

# cerebro 0.1.1.3

- `run()` can fold the corteza Matrix long-poll into its scheduler loop,
  so one process serves both the chat bot and the routine scheduler.
  Enable with a `matrix:` block in `config.yaml` (`enabled: true`, with
  optional `system`/`model`/`provider`/`tools_filter` overrides and a
  `timeout_ms` poll timeout) or `run(matrix = TRUE)`. Each iteration
  fires due routines, then runs one `corteza::matrix_run_step()`, whose
  long-poll doubles as the scheduler tick (`timeout_ms`, default
  `tick_seconds * 1000`). Off by default; the loop stays a plain ticker.
  The Matrix connection still comes from corteza's own config; the
  `matrix:` block only feeds corteza. Requires `corteza (>= 0.6.9.6)`
  for `matrix_run_init()`/`matrix_run_step()`.

# cerebro 0.1.1

- Added the YAML `vault_check` routine capability and `vault_check()`
  helper for reporting `pensar::lint()` results across configured vaults.
- Vault check reports now count lint errors separately from findings, and
  validate integer options before calling `pensar`.
- YAML `shell` routines now report non-zero exit statuses as routine errors
  instead of silently continuing.
