library(tinytest)

# The router's answer when there is no answer.
#
# `mybot · No durable topical thread` was a real room. The router had no
# structural way to say "there is no durable topic here", so it said it in
# prose, in the topic field, and the confidence gate waved it through --
# a confidence gate measures how sure the model is, not what it was sure
# of. `mybot · ` was a real room too: the same answer with the words left
# out, since an empty topic still made a non-empty prefixed name.

dt <- getFromNamespace(".thread_durable_topic", "cerebro")

# ---- ordinary topics survive intact ------------------------------------
# Listed first and generously, because everything below is a rejection
# rule and a rejection rule that is too eager is worse than the junk room
# it prevents: a real conversation silently stops being filed.
for (ok in c("Travel", "Coffee brewing", "Health and fitness",
             "Teaching long division", "Chess opening theory",
             "Urban planning and public transit funding",
             "Garden planning", "Statistics", "Owner profile",
             "Home Improvement", "Not buying new clothes", "Nordic skiing",
             "Nautical charts", "NA-East server latency")) {
    expect_equal(dt(ok), ok)
}

# ---- the refusals that actually happened -------------------------------
expect_equal(dt("No durable topical thread"), "")
expect_equal(dt(""), "")
expect_equal(dt("   "), "")

# "durable" is the system prompt's own word. No theme is ever called that,
# so its presence is the model quoting the question back at the answer.
expect_equal(dt("no durable topic"), "")
expect_equal(dt("Nothing durable here"), "")

# ---- the other ways of saying nothing ----------------------------------
for (nope in c("none", "None", "N/A", "n/a", "NA", "unknown", "Unclear")) {
    expect_equal(dt(nope), "")
}
expect_equal(dt("No clear theme"), "")
expect_equal(dt("no specific topic"), "")

# ---- a sentence is not a label -----------------------------------------
# The field is meant for a name. Anything this long is the model
# explaining itself in the slot reserved for the answer, and it would
# become a room title read left-to-right across a sidebar.
long <- paste("This conversation covers several unrelated matters and does",
              "not settle into one durable theme")
expect_true(nchar(long) > 60L)
expect_equal(dt(long), "")

# The boundary is a length, so pin both sides of it rather than trusting
# the constant to be read correctly.
expect_equal(dt(strrep("a", 60L)), strrep("a", 60L))
expect_equal(dt(strrep("a", 61L)), "")

# ---- shapes, not just values -------------------------------------------
# The router hands this whatever JSON parsed to. NULL is the missing key,
# and it used to fall back to "General" -- a label nothing chose, with a
# room to match.
expect_equal(dt(NULL), "")
expect_equal(dt(NA), "")
expect_equal(dt(character(0)), "")
# Whitespace is normalized, not merely trimmed, so a topic that differs
# from an existing room only by spacing resolves to that room instead of
# creating a second one.
expect_equal(dt("  Coffee   brewing "), "Coffee brewing")
