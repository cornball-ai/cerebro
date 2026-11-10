library(tinytest)

root <- tempfile("cerebro-agents-")

or_else <- getFromNamespace("%||%", "cerebro")
expect_identical(or_else(NULL, "fallback"), "fallback")
expect_identical(or_else(character(), "fallback"), "fallback")
expect_identical(or_else(0L, 1L), 0L)

paths <- cerebro::agent_paths("alice", root = root)
expect_equal(paths$instance_dir, file.path(root, "alice"))
expect_equal(paths$todo_current, file.path(root, "alice", "todo", "this_week"))
expect_equal(paths$todo_archive, file.path(root, "alice", "todo", "archive"))
expect_equal(paths$wiki, file.path(root, "alice", "wiki"))

expect_error(cerebro::agent_paths(""))
expect_error(cerebro::agent_paths("alice/bob"))

paths <- cerebro::instantiate_agent("alice", root = root, set_active = FALSE)
expect_true(dir.exists(paths$instance_dir))
expect_true(dir.exists(paths$todo_current))
expect_true(dir.exists(paths$todo_archive))
expect_true(dir.exists(paths$wiki_pages))
expect_true(dir.exists(paths$wiki_raw))

unlink(root, recursive = TRUE)
