#' Send a message to a chat room
#'
#' Sends through chat.api with two conveniences for capability scripts:
#' room name resolution (cached in instance state) and dry-run support.
#'
#' Resolution order for \code{room}:
#' \enumerate{
#'   \item If \code{room} starts with \code{!}, treated as a literal id.
#'   \item Else looked up against
#'     \code{state.json:matrix_rooms[[room]]}, the per-instance cache.
#'   \item Else queried by display name against the bot's joined rooms;
#'     hit cached on success.
#'   \item Else falls back to \code{room_id} in the Matrix config (the
#'     default DM).
#' }
#'
#' @param text Character. Message body.
#' @param room Character or NULL. Room id (\code{!...}) or display name.
#' @param dry_run Logical. If TRUE, prints to stdout and returns NULL
#'   without sending.
#' @param instance_dir Character or NULL.
#' @param matrix_config Character or NULL. Explicit corteza Matrix JSON
#'   config. NULL uses \code{CORTEZA_MATRIX_CONFIG} and then corteza's
#'   default location.
#' @param markdown Logical. If TRUE, the body is rendered as markdown.
#' @param msgtype Character. Matrix msgtype. Defaults to \code{"m.notice"}:
#'   everything cerebro sends through this wrapper is proactive persona
#'   traffic (briefings, sync summaries, reconcile reports), which clients
#'   render without a notification ping. Pass \code{"m.text"} for a message
#'   that should notify.
#' @param client A chat.api client, or NULL to build one from
#'   \code{matrix_config}. Supplying one is how tests run without a
#'   homeserver.
#' @return Event id of the sent message, or NULL on dry-run.
#' @export
matrix_send <- function(text, room = NULL, dry_run = FALSE,
                        instance_dir = NULL, matrix_config = NULL,
                        markdown = TRUE, msgtype = "m.notice", client = NULL) {
    if (isTRUE(dry_run)) {
        cat("=== matrix_send (dry-run) [", room %||% "default", "] ===\n",
            text, "\n", sep = "")
        return(invisible(NULL))
    }
    client <- client %||% .chat_client(matrix_config)
    rid <- resolve_room(room, instance_dir = instance_dir,
                        matrix_config = matrix_config, client = client)
    ids <- chat.api::chat_send(client, rid, text,
                               markup = if (isTRUE(markdown)) "markdown" else "plain",
                               kind = .send_kind(msgtype))
    # One id per event sent; the text event comes last.
    as.character(ids)[[length(ids)]]
}

# chat.api's kind vocabulary covers m.text, m.notice and m.emote. Any
# other msgtype is passed as the kind itself, which the Matrix adapter
# forwards as a msgtype.
.send_kind <- function(msgtype) {
    switch(msgtype, m.text = "message", m.notice = "notice",
           m.emote = "emote", msgtype)
}

#' Resolve a Matrix room name or id to an id
#'
#' See \code{\link{matrix_send}} for the resolution order.
#'
#' @param room Character or NULL.
#' @param instance_dir Character or NULL.
#' @param matrix_config Character or NULL. Explicit Matrix config path.
#' @param client A chat.api client, or NULL to build one from
#'   \code{matrix_config} when a display name has to be looked up.
#' @return Character room id.
#' @export
resolve_room <- function(room, instance_dir = NULL, matrix_config = NULL,
                         client = NULL) {
    if (is.null(room) || !nzchar(room)) {
        return(.default_room(matrix_config))
    }
    if (startsWith(room, "!")) {
        return(room)
    }

    room_key <- .matrix_room_name_key(room)
    state <- read_instance_state(instance_dir)
    cached <- state$matrix_rooms[[room_key]] %||% state$matrix_rooms[[room]]
    if (!is.null(cached) && nzchar(cached)) {
        return(cached)
    }

    rid <- .lookup_room_by_name(room_key,
                                client %||% .chat_client(matrix_config))
    if (!is.null(rid)) {
        state$matrix_rooms <- c(state$matrix_rooms %||% list(),
                                stats::setNames(list(rid), room_key))
        write_instance_state(state, instance_dir)
        message(sprintf("cerebro: cached room %s -> %s", room, rid))
        return(rid)
    }
    message(sprintf("cerebro: no joined room named %s; using default", room))
    .default_room(matrix_config)
}

# The room a send with no room goes to: the config's room_id, which is
# the persona's DM.
.default_room <- function(matrix_config = NULL) {
    rid <- .chat_config(matrix_config)$room_id
    if (!is.character(rid) || length(rid) != 1L || !nzchar(rid)) {
        stop("no default room: the Matrix config has no room_id", call. = FALSE)
    }
    rid
}

# corteza's Matrix config. An explicit path wins; otherwise chat.api
# resolves CORTEZA_MATRIX_CONFIG and then corteza's default location,
# exactly as corteza itself does, so cerebro and the bot it hosts read
# the same file.
.chat_config <- function(path = NULL) {
    if (is.null(path) || !nzchar(path)) {
        return(chat.api::chat_matrix_config(app = "corteza",
                env_var = "CORTEZA_MATRIX_CONFIG"))
    }
    chat.api::chat_matrix_config(app = "corteza", path = path)
}

# The client cerebro's out-of-band traffic goes through: briefing
# sends, the fold sweep, room creation. Built the way corteza builds the
# bot's own client, from the same config, so the two agree on the
# account, the homeserver, and whether rooms are encrypted.
#
# save_cursor is off because this client never polls: a side client
# that saved a sync cursor would move the running bot's replay position.
.chat_client <- function(matrix_config = NULL) {
    cfg <- .chat_config(matrix_config)
    chat.api::chat_matrix(mx = cfg, save_cursor = FALSE,
                          e2ee = isTRUE(cfg$e2ee))
}

# A client built on first call and reused after. The sync routines
# resolve rooms and send per destination, and a run that files nothing
# should not need a Matrix config at all.
.lazy_client <- function(client = NULL, matrix_config = NULL) {
    function() {
        if (is.null(client)) {
            client <<- .chat_client(matrix_config)
        }
        client
    }
}

# The bot's own user id, or "" when the transport cannot say.
.self_id <- function(client) {
    id <- tryCatch(chat.api::chat_whoami(client)$id, error = function(e) NULL)
    if (is.character(id) && length(id) == 1L && !is.na(id)) {
        id
    } else {
        ""
    }
}

# A room's name and topic, or an empty answer. chat_channel_info()
# distinguishes "there is no name" (NULL field) from "I cannot ask" (an
# error), and both mean the same thing to every caller here: fall back
# to a default.
.channel_info <- function(client, room_id) {
    empty <- list(id = room_id, name = NULL, topic = NULL)
    tryCatch(chat.api::chat_channel_info(client, room_id),
             error = function(e) empty)
}

# Set the bot's Matrix avatar from the instance config, once, at startup.
# A bot's identity -- its avatar, like its display name -- is the bot
# layer's job, so it lives here in cerebro rather than in corteza's turn
# engine. (The display name is the one piece corteza keeps, because it
# rewrites it live to badge the current model.) Creds come from the same
# corteza Matrix config the poll loop uses; only the avatar file is read
# from cerebro's own config.
#
# Best-effort: any failure is logged and returns FALSE, so a bad path or a
# briefly unreachable homeserver never blocks the scheduler/poll loop.
# Idempotent across restarts: the file's md5 is recorded in the instance
# state.json and an unchanged file is not re-uploaded -- each upload mints
# a fresh media blob on the homeserver, so re-sending the same picture on
# every boot would orphan one per restart.
.apply_matrix_avatar <- function(avatar, instance_dir = NULL, client = NULL) {
    if (is.null(avatar) || !is.character(avatar) || length(avatar) != 1L ||
        is.na(avatar) || !nzchar(avatar)) {
        return(invisible(FALSE))
    }
    path <- path.expand(avatar)
    if (!file.exists(path)) {
        message("cerebro: matrix avatar not found at ", path, "; skipping")
        return(invisible(FALSE))
    }
    md5 <- unname(tools::md5sum(path))
    if (is.na(md5)) {
        message("cerebro: could not read matrix avatar ", path, "; skipping")
        return(invisible(FALSE))
    }
    if (identical(read_instance_state(instance_dir)$matrix_avatar_md5, md5)) {
        return(invisible(FALSE))
    }
    ok <- tryCatch({
        chat.api::chat_set_identity(client %||% .chat_client(), avatar = path)
        TRUE
    }, error = function(e) {
        message("cerebro: setting matrix avatar failed: ", conditionMessage(e))
        FALSE
    })
    if (isTRUE(ok)) {
        update_instance_state(matrix_avatar_md5 = md5,
                              instance_dir = instance_dir)
        message("cerebro: matrix avatar set from ", path)
    }
    invisible(ok)
}

.matrix_room_name_key <- function(name) {
    if (is.null(name) || !length(name)) {
        return("")
    }
    name <- as.character(name[[1L]])
    if (is.na(name)) {
        return("")
    }
    trimws(gsub("[[:space:]]+", " ", name, perl = TRUE))
}

.lookup_room_by_name <- function(name, client) {
    target <- .matrix_room_name_key(name)
    if (!nzchar(target)) {
        return(NULL)
    }
    rooms <- tryCatch(chat.api::chat_channels(client),
                      error = function(e) character())
    normalized_match <- NULL
    for (rid in rooms) {
        raw <- .channel_info(client, rid)$name
        if (is.null(raw)) {
            next
        }
        if (identical(raw, name)) {
            return(rid)
        }
        if (is.null(normalized_match) &&
            identical(.matrix_room_name_key(raw), target)) {
            normalized_match <- rid
        }
    }
    normalized_match
}

#' Ensure a Matrix space exists
#'
#' A space is a room that holds other rooms. One invite to the space
#' replaces an invite per room, which is the difference between a persona
#' that files a dozen topic rooms overnight and a readable morning.
#'
#' A space cannot be converted from an ordinary room afterwards --
#' \code{type} is fixed in \code{m.room.create} -- so a name that already
#' resolves to a non-space room is an error rather than something to
#' repair in place.
#'
#' Personas share a space: one creates it and the others find it by name.
#' Filing a room under a space is a state event on the space, which the
#' server's default power levels reserve for the creator, so the creator
#' lowers that one event to the member level right after creation. A
#' space created elsewhere needs the same grant from whoever owns it, or
#' every other persona's filing falls back to a per-room invite.
#'
#' @param name Non-empty space display name.
#' @param topic Character or NULL.
#' @param invite Character vector of Matrix user IDs, invited once at
#'   creation.
#' @param matrix_config Character or NULL. Persona-specific Matrix config.
#' @param instance_dir Character or NULL. State cache location.
#' @param dry_run Logical. Report the intended action without network writes.
#' @param client A chat.api client, or NULL to build one from
#'   \code{matrix_config}.
#' @return The space room ID, or the name during a dry-run creation.
#' @export
ensure_matrix_space <- function(name, topic = NULL, invite = character(),
                                matrix_config = NULL, instance_dir = NULL,
                                dry_run = FALSE, client = NULL) {
    if (!is.character(name) || length(name) != 1L || !nzchar(trimws(name))) {
        stop("name must be a non-empty character scalar", call. = FALSE)
    }
    name <- .matrix_room_name_key(name)
    client <- client %||% .chat_client(matrix_config)
    sid <- NULL
    # Cached apart from matrix_rooms. A space and a room can carry the
    # same display name, and one cache would hand back whichever was
    # written last -- filing rooms into a conversation, or posting
    # messages into a space.
    if (!is.null(instance_dir)) {
        cached <- (read_instance_state(instance_dir)$matrix_spaces %||% list())[[name]]
        if (!is.null(cached) && nzchar(cached)) {
            sid <- as.character(cached[[1L]])
        }
    }
    if (is.null(sid)) {
        # Ask the server, not just this instance's cache. Personas share a
        # space: whichever one creates it caches the id, and every other
        # one starts with an empty cache. Cache-only resolution has each
        # of them create its own space of the same name, which looks
        # exactly like it worked until two sidebars disagree.
        sid <- .lookup_room_by_name(name, client)
    }
    if (isTRUE(dry_run)) {
        if (is.null(sid)) {
            message("cerebro: would create Matrix space ", name)
        } else {
            message("cerebro: would reuse Matrix space ", name)
        }
        return(invisible(sid %||% name))
    }
    if (is.null(sid)) {
        sid <- chat.api::chat_channel_create(client, name = name,
            topic = topic, preset = "private_chat",
            invite = as.character(invite),
            creation_content = list(type = "m.space"))
        message("cerebro: created Matrix space ", name, " -> ", sid)
        .open_space_filing(client, sid, name)
    }
    if (!is.null(instance_dir)) {
        state <- read_instance_state(instance_dir)
        spaces <- state$matrix_spaces %||% list()
        spaces[[name]] <- sid
        state$matrix_spaces <- spaces
        write_instance_state(state, instance_dir)
    }
    sid
}

# Let every member of a new space file rooms under it.
#
# m.space.child is a state event, so the default power levels want 50
# for it, and from room version 12 the creator holds that power
# implicitly while every other member sits at 0. A second persona
# sharing the space then fails each filing and falls back to the
# per-room invite the space exists to remove. Only the creator can
# lower the level, which is why this runs at creation and not in
# .file_in_space(). Read-merge-write: a power_levels event replaces the
# whole content, and what the server set at creation stays as it was.
.open_space_filing <- function(client, space_id, name) {
    ok <- tryCatch({
        levels <- chat.api::chat_get_state(client, space_id,
            "m.room.power_levels") %||% list()
        # An empty object reads back as an empty unnamed list and would
        # go out as [] where the server expects {}. Absent means the same
        # default, so drop it.
        levels <- levels[lengths(levels) > 0L]
        events <- levels$events %||% list()
        events[["m.space.child"]] <- 0L
        levels$events <- events
        chat.api::chat_set_state(client, space_id, "m.room.power_levels",
                                 levels)
        TRUE
    }, error = function(e) {
        message("cerebro: could not open ", name,
                " to filing by its members: ", conditionMessage(e))
        FALSE
    })
    invisible(ok)
}

# A server to route a space child through, for m.space.child's `via`.
#
# Derived from the BOT'S OWN user id, not from the room id. Room ids
# stopped carrying a server: from room version 12 they are opaque and
# colon-free, so splitting one on ":" yields the whole id back. Writing
# that as a via produces a child event the server stores happily and
# every client ignores, because the via does not resolve to a server --
# a space that fills up and renders empty.
#
# The bot is in the room it is filing, so the bot's server is in the
# room, which is exactly what a via has to be. The room id is still used
# when it does carry a server, since that is a second true answer.
.mx_via <- function(user_id, room_id = NULL) {
    from_room <- if (!is.null(room_id)) {
        v <- sub("^[^:]*:", "", as.character(room_id)[[1L]])
        if (identical(v, as.character(room_id)[[1L]])) {
            ""
        } else {
            v
        }
    } else {
        ""
    }
    user_id <- as.character(user_id %||% "")[[1L]]
    from_user <- sub("^[^:]*:", "", user_id)
    if (identical(from_user, user_id)) {
        from_user <- ""
    }
    via <- unique(c(from_room, from_user))
    via[nzchar(via)]
}

# File a freshly created room under a space and open it to the space's
# members.
#
# Two state events do the filing (child on the space, parent on the room)
# and a third does the access: `restricted` join rules naming the space,
# which is what actually lets a space member in. Without it the room is
# listed in the sidebar and refuses the click.
#
# The join-rule write is the fragile one -- restricted rules need room
# version 9 or newer -- so its failure falls back to a direct invite. A
# room nobody can enter is a worse outcome than the invite this was meant
# to avoid.
.file_in_space <- function(client, space_id, room_id, name,
                           invite = character()) {
    via <- .mx_via(.self_id(client), room_id)
    if (!length(via)) {
        # No via means a child event no client will render. Refusing is
        # the whole point: the write would succeed and the space would
        # look filed and read empty.
        message("cerebro: no via server for ", name,
                "; not filing it under the space")
        via <- NULL
    }
    ok <- !is.null(via) && tryCatch({
        chat.api::chat_set_state(client, space_id, "m.space.child",
                                 list(via = as.list(via), suggested = TRUE),
                                 state_key = room_id)
        chat.api::chat_set_state(client, room_id, "m.space.parent",
                                 list(via = as.list(via), canonical = TRUE),
                                 state_key = space_id)
        chat.api::chat_set_state(client, room_id, "m.room.join_rules",
                                 list(join_rule = "restricted",
                                      allow = list(list(type = "m.room_membership",
                        room_id = space_id))))
        TRUE
    }, error = function(e) {
        message("cerebro: could not file ", name, " under the space: ",
                conditionMessage(e))
        FALSE
    })
    if (!ok && length(invite)) {
        tryCatch({
            for (u in as.character(invite)) {
                chat.api::chat_channel_invite(client, room_id, u)
            }
            message("cerebro: invited to ", name, " directly instead")
        }, error = function(e) {
            message("cerebro: could not invite to ", name, ": ",
                    conditionMessage(e))
        })
    }
    invisible(ok)
}

#' Ensure a persistent Matrix conversation room exists
#'
#' Reuses an exact display-name match or creates a private room, optionally
#' setting a topic and inviting users. When \code{name} has no match but
#' \code{legacy_name} does, the matched room is renamed to \code{name} and
#' reused. Topics beginning with a filesystem path
#' use corteza's existing \code{"<path> | <description>"} convention and
#' therefore scope that room's tools to the path.
#'
#' @param name Non-empty room display name.
#' @param topic Character or NULL.
#' @param invite Character vector of Matrix user IDs. Ignored for a newly
#'   created room when \code{space} is supplied: membership comes from the
#'   space instead, which is the whole point of using one.
#' @param matrix_config Character or NULL. Persona-specific Matrix config.
#' @param instance_dir Character or NULL. State cache location.
#' @param dry_run Logical. Report the intended action without network writes.
#' @param legacy_name Character or NULL. Prior display name to look up when
#'   \code{name} has no match; a hit is renamed to \code{name}.
#' @param space Character or NULL. A space room ID from
#'   \code{\link{ensure_matrix_space}}. A newly created room is filed under
#'   it and made joinable by its members rather than invited to
#'   individually.
#' @param client A chat.api client, or NULL to build one from
#'   \code{matrix_config}.
#' @return The room ID, or the room name during a dry-run creation.
#' @export
ensure_matrix_room <- function(name, topic = NULL, invite = character(),
                               matrix_config = NULL, instance_dir = NULL,
                               dry_run = FALSE, legacy_name = NULL,
                               space = NULL, client = NULL) {
    if (!is.character(name) || length(name) != 1L || !nzchar(trimws(name))) {
        stop("name must be a non-empty character scalar", call. = FALSE)
    }
    name <- .matrix_room_name_key(name)
    client <- client %||% .chat_client(matrix_config)
    rid <- NULL
    if (!is.null(instance_dir)) {
        cached <- (read_instance_state(instance_dir)$matrix_rooms %||% list())[[name]]
        if (!is.null(cached) && nzchar(cached)) {
            rid <- as.character(cached[[1L]])
        }
    }
    if (is.null(rid)) {
        rid <- .lookup_room_by_name(name, client)
    }
    legacy <- NULL
    if (is.null(rid) && is.character(legacy_name) &&
        length(legacy_name) == 1L && nzchar(trimws(legacy_name))) {
        legacy <- trimws(legacy_name)
        rid <- .lookup_room_by_name(legacy, client)
        if (is.null(rid)) {
            legacy <- NULL
        }
    }
    if (isTRUE(dry_run)) {
        if (!is.null(legacy)) {
            message("cerebro: would rename Matrix room ", legacy, " -> ", name)
        } else if (is.null(rid)) {
            message("cerebro: would create Matrix room ", name)
        } else {
            message("cerebro: would reuse Matrix room ", name)
        }
        return(invisible(rid %||% name))
    }
    if (is.null(rid)) {
        in_space <- is.character(space) && length(space) == 1L && nzchar(space)
        # Inviting AND filing under a space would put the invite back that
        # the space exists to remove, so a filed room is created with no
        # invite and .file_in_space() opens it to the space's members.
        rid <- chat.api::chat_channel_create(client, name = name,
            topic = topic, preset = "private_chat",
            invite = if (in_space) character() else as.character(invite))
        message("cerebro: created Matrix room ", name, " -> ", rid)
        if (in_space) {
            .file_in_space(client, space, rid, name, invite)
        }
    } else {
        if (!is.null(legacy)) {
            # Rename failure degrades to posting under the old name; the
            # rename is retried on the next sync.
            tryCatch({
                chat.api::chat_set_state(client, rid, "m.room.name",
                    list(name = name))
                message("cerebro: renamed Matrix room ", legacy, " -> ", name)
            }, error = function(e) {
                message("cerebro: could not rename Matrix room ", legacy,
                        ": ", conditionMessage(e))
            })
        }
        if (!is.null(topic) && nzchar(topic)) {
            current <- .channel_info(client, rid)$topic
            if (!identical(current, topic)) {
                chat.api::chat_set_state(client, rid, "m.room.topic",
                    list(topic = topic))
            }
        }
    }
    if (!is.null(instance_dir)) {
        state <- read_instance_state(instance_dir)
        rooms <- state$matrix_rooms %||% list()
        if (!is.null(legacy)) {
            rooms <- rooms[names(rooms) != legacy]
        }
        rooms[[name]] <- rid
        state$matrix_rooms <- rooms
        write_instance_state(state, instance_dir)
    }
    rid
}
