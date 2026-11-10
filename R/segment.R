# The open half of the conversation lifecycle: the segment room a
# conversation becomes at /clear, and the archive a folded thread stands
# for. Both are hooks corteza's loop asks the host (see
# corteza::bot_run_init()), and run() supplies them. The fold half, which
# sinks segments into topic rooms, is lifecycle.R; the schema both halves
# write is the lifecycle vignette.

#' Open a segment room for a conversation that just ended
#'
#' corteza's \code{on_clear} hook. When the cleared room is one the bot
#' config lists in \code{segment_rooms}, the ended conversation gets a
#' room of its own: a segment the user can reopen from the room list
#' while it is fresh. The room holds a notice pointing at the archived
#' transcript and the \code{ai.cornball.lifecycle} state event
#' (\code{state: "segment"}) that \code{\link{sweep_segments}} later folds
#' by. Everyone in the home room except the bot is invited.
#'
#' A room not listed, or a transport that cannot create rooms or set
#' state, gets no segment, and corteza acknowledges the clear without
#' one.
#'
#' @param chat A chat.api client, the one corteza runs on.
#' @param cfg The bot config corteza loaded. \code{cfg$segment_rooms} is
#'   the character vector of room ids whose conversations segment.
#' @param room_id Character. The room that was cleared.
#' @param title Character. The segment room's name; corteza derives it
#'   from the conversation's opening line.
#' @param vault_ref Character or NULL. The archived transcript's path,
#'   relative to the vault root, or NULL when nothing was archived.
#' @return \code{list(id, name)} for the room created, invisibly, or
#'   NULL for none. An error from the room creation propagates: a
#'   segment the user is told about must exist, so corteza reports the
#'   failure and acknowledges the clear without one.
#' @examples
#' cl <- chat.api::chat_loopback()
#' seg <- segment_open(cl, list(segment_rooms = "home"), "home",
#'                     "Fold formula (2026-08-19)", "raw/matrix/seg.md")
#' chat.api::chat_get_state(cl, seg$id, "ai.cornball.lifecycle")$state
#' @export
segment_open <- function(chat, cfg, room_id, title, vault_ref = NULL) {
    if (!room_id %in% as.character(cfg$segment_rooms %||% character())) {
        return(invisible(NULL))
    }
    caps <- chat.api::chat_capabilities(chat)
    if (!isTRUE(caps$channel_create) || !isTRUE(caps$set_state)) {
        message("cerebro: transport cannot create rooms or set state; ",
                "no segment for ", room_id)
        return(invisible(NULL))
    }
    # Everyone in the home room except the bot itself. A segment room
    # without its human is a sidebar entry no one sees. Best-effort: a
    # transport without a member list yields an uninvited room, which
    # still archives correctly.
    invite <- tryCatch({
        members <- chat.api::chat_members(chat, room_id)
        setdiff(members, .self_id(chat))
    }, error = function(e) character())
    seg_id <- chat.api::chat_channel_create(chat, name = title,
        preset = "private_chat",
        invite = invite)
    home_label <- .channel_info(chat, room_id)$name
    if (!is.character(home_label) || length(home_label) != 1L ||
        !nzchar(home_label)) {
        home_label <- room_id
    }
    body <- paste0(
                   "Continued from ", home_label, ".",
        if (!is.null(vault_ref)) {
            paste0("\nArchived transcript: ", vault_ref)
        },
                   "\nReply here to pick the topic back up.")
    # The summary is bot bookkeeping, not conversation: a notice renders
    # without a notification ping.
    tryCatch(chat.api::chat_send(chat, seg_id, body, kind = "notice"),
             error = function(e) {
        message("cerebro: segment summary send failed: ", conditionMessage(e))
        NULL
    })
    content <- list(state = "segment", segment_of = room_id,
                    since = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    if (!is.null(vault_ref)) {
        content$vault <- vault_ref
    }
    chat.api::chat_set_state(chat, seg_id, .LIFECYCLE_TYPE, content)
    invisible(list(id = seg_id, name = title))
}

#' Find the archive a folded thread stands for
#'
#' corteza's \code{thread_context} hook. \code{\link{sweep_segments}}
#' folds a segment into a topic room as a thread and indexes it there,
#' as an \code{ai.cornball.fold} state event keyed by the thread's root
#' event id. A reply in that thread continues the archived conversation,
#' and this is where corteza asks which one. Read rather than derived,
#' and O(1): scanning the room's history for the root would grow with
#' the room, which is the work the index exists to avoid.
#'
#' @param chat A chat.api client.
#' @param room_id Character. The topic room the thread is in.
#' @param thread_root Character. The thread's root event id.
#' @return \code{list(segment, vault)}: the folded segment room's id and
#'   the archive's path, relative to the vault root. NULL when the thread
#'   is not a fold (an ordinary thread someone started), the transport
#'   has no durable state, or the index could not be read, which is
#'   reported. Never an error: an unseeded reply is a worse conversation,
#'   a failed one is none.
#' @examples
#' cl <- chat.api::chat_loopback()
#' fold_lookup(cl, "topic", "$root") # NULL: nothing folded here yet
#' chat.api::chat_set_state(cl, "topic", "ai.cornball.fold",
#'                          list(segment = "!seg:ex",
#'                               vault = "raw/matrix/seg.md"),
#'                          state_key = "$root")
#' fold_lookup(cl, "topic", "$root")
#' @export
fold_lookup <- function(chat, room_id, thread_root) {
    if (is.null(thread_root) || !nzchar(thread_root)) {
        return(NULL)
    }
    caps <- tryCatch(chat.api::chat_capabilities(chat),
                     error = function(e) list())
    if (!isTRUE(caps$set_state)) {
        return(NULL)
    }
    st <- tryCatch(
                   chat.api::chat_get_state(chat, room_id, .FOLD_TYPE,
            state_key = thread_root),
                   error = function(e) {
        message("cerebro: could not read fold index: ", conditionMessage(e))
        NULL
    })
    if (!is.list(st)) {
        return(NULL)
    }
    ref <- st$vault
    if (!is.character(ref) || length(ref) != 1L || is.na(ref) || !nzchar(ref)) {
        return(NULL)
    }
    list(segment = st$segment, vault = ref)
}
