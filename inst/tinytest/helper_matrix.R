# A Matrix-shaped chat.api client with no homeserver behind it.
#
# chat.api's Matrix adapter takes a testing seam for every verb cerebro
# calls (.channels, .info, .get_state, .state, .create, .history,
# .send), so a test replaces exactly the calls that would reach a
# server and keeps the adapter's own behaviour -- the oldest-first
# history page, the self flag, `...` reaching room creation -- which is
# what production runs through. The config carries a user id with a
# server part, so the via derivation and self detection work as live.
#
# Sourced by the test files that need it; tinytest runs each file with
# this directory as the working directory.
matrix_client <- function(..., user_id = "@bot:ex") {
    chat.api::chat_matrix(mx = list(server = "https://mx.example",
                                    token = "t", user_id = user_id,
                                    device_id = "D"),
                          save_cursor = FALSE, ...)
}

# One m.room.message event as the history seam hands them back.
matrix_event <- function(sender, ts, body = "", event_id = NULL,
                         msgtype = "m.text") {
    list(type = "m.room.message",
         event_id = event_id %||% paste0("$", sender, ts),
         sender = sender, origin_server_ts = ts,
         content = list(msgtype = msgtype, body = body))
}
