# Topic assignment for the fold sweep.
#
# At fold time the segment is classified against the existing topic
# rooms with a self-rated confidence. At or above the threshold it
# files silently; a new topic is created only when the proposal itself
# clears the bar, and is announced as a notice so it can be vetoed or
# renamed at leisure; everything else lands in the Unsorted catch-all,
# silently. Nothing here ever asks a question -- misfiled folds are
# corrected by renaming rooms, not by interrupting the user, and the
# threshold is the dial to turn when misfiles annoy.

# Rooms already stamped as topics: a named character vector mapping
# topic label -> room id. The stamp only marks a room AS a topic; the
# label is the room's current display name, because renaming the room
# is how a topic gets vetoed or corrected -- an enumeration keyed on
# the stamp's original `topic` field would keep filing into the old
# name forever. That field survives as provenance, and as the fallback
# label when the name cannot be read.
.topic_rooms <- function(client) {
    rooms <- chat.api::chat_channels(client)
    out <- character()
    for (rid in rooms) {
        st <- .lifecycle_state(client, rid)
        if (!identical(st$state, "topic")) {
            next
        }
        nm <- .room_label(client, rid)
        if (identical(nm, rid)) {
            nm <- if (is.character(st$topic) && length(st$topic) == 1L) {
                st$topic
            } else {
                ""
            }
        }
        if (nzchar(nm) && !(nm %in% names(out))) {
            out[[nm]] <- rid
        }
    }
    out
}

# A bounded excerpt of the archived transcript, the classifier's main
# signal. The vault is local, so a fold can read its own archive; a
# missing or unreadable file degrades to classifying on the title.
.segment_excerpt <- function(seg, max_chars = 2000L) {
    ref <- seg$state$vault
    if (!is.character(ref) || length(ref) != 1L || !nzchar(ref)) {
        return("")
    }
    root <- tryCatch(getExportedValue("pensar", "default_vault")(),
                     error = function(e) NULL)
    if (is.null(root) || !length(root) || !nzchar(root)) {
        return("")
    }
    path <- file.path(root, ref)
    if (!file.exists(path)) {
        return("")
    }
    txt <- tryCatch(
                    paste(readLines(path, n = 100L, warn = FALSE), collapse = "\n"),
                    error = function(e) ""
    )
    if (nchar(txt) > max_chars) {
        substr(txt, 1L, max_chars)
    } else {
        txt
    }
}

# Ask the model where a segment belongs. Returns list(topic, new,
# confidence) or NULL when a reply cannot be had or read -- the caller
# treats NULL as "file in Unsorted", so a provider outage degrades to
# the catch-all rather than an error, and never to a question.
.classify_segment <- function(title, excerpt, topics, model, provider) {
    if (!requireNamespace("llm.api", quietly = TRUE)) {
        return(NULL)
    }
    listing <- if (length(topics)) {
        paste("-", names(topics), collapse = "\n")
    } else {
        "(none yet)"
    }
    prompt <- paste(
                    "You are filing an archived conversation into a topic.",
                    "",
                    "Existing topics:",
                    listing,
                    "",
                    paste0("Conversation title: ", title),
                    "",
                    "Opening of the archived transcript:",
                    excerpt,
                    "",
                    paste("Pick the existing topic that fits best, or propose a new",
                          "topic name (2-4 words) only if none fits. Rate your",
                          "confidence that the conversation belongs in the chosen",
                          "topic, 0 to 1."),
                    paste0("Return ONLY JSON: ",
                           "{\"topic\": \"...\", \"new\": true|false, ",
                           "\"confidence\": 0.0}"),
                    sep = "\n")
    resp <- tryCatch(
                     llm.api::chat(prompt = prompt, model = model, provider = provider),
                     error = function(e) {
        message("cerebro: topic classify error: ", conditionMessage(e))
        NULL
    }
    )
    if (is.null(resp)) {
        return(NULL)
    }
    # A reply that is not one readable string -- missing content, a
    # nested list, character(0), NA -- is garbage, and garbage falls
    # through to Unsorted rather than erroring inside the regex.
    if (is.list(resp)) {
        reply <- resp$content %||% ""
    } else {
        reply <- resp
    }
    reply <- suppressWarnings(tryCatch(as.character(reply)[1L],
                                       error = function(e) NA_character_))
    if (!is.character(reply) || length(reply) != 1L || is.na(reply) ||
        !nzchar(reply)) {
        return(NULL)
    }
    m <- regmatches(reply, regexpr("\\{(?:[^{}]|\\{[^{}]*\\})*\\}", reply,
                                   perl = TRUE))
    if (!length(m)) {
        return(NULL)
    }
    obj <- tryCatch(jsonlite::fromJSON(m[1L], simplifyVector = FALSE),
                    error = function(e) NULL)
    if (is.null(obj)) {
        return(NULL)
    }
    topic <- .matrix_room_name_key(as.character(obj$topic %||% "")[1L])
    conf <- suppressWarnings(as.numeric(obj$confidence %||% NA_real_)[1L])
    list(topic = topic, new = isTRUE(obj$new), confidence = conf)
}

# Where a segment files, from a classification alone. Pure, so the
# no-questions policy is testable without a model. Returns list(topic,
# create); create means the topic room does not exist yet and the
# sweep should make and announce it.
.topic_decision <- function(cls, topics, threshold, unsorted) {
    fallthrough <- list(topic = unsorted, create = FALSE)
    if (is.null(cls)) {
        return(fallthrough)
    }
    conf <- cls$confidence
    # NA refuses, never passes: an unreadable confidence filed into a
    # real topic would be a misfile wearing a green tick.
    if (!is.numeric(conf) || length(conf) != 1L || is.na(conf) ||
        conf < threshold) {
        return(fallthrough)
    }
    topic <- cls$topic
    if (!is.character(topic) || length(topic) != 1L || !nzchar(topic) ||
        identical(topic, unsorted)) {
        return(fallthrough)
    }
    # A paragraph is not a room name.
    if (nchar(topic) > 60L) {
        return(fallthrough)
    }
    # Membership decides existing vs new: a model claiming an existing
    # name that is not there is proposing a new topic whether it knows
    # it or not.
    list(topic = topic, create = !(topic %in% names(topics)))
}
