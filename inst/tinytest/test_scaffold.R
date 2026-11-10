library(tinytest)

# Resolve root up front: the scaffold reports resolved paths, and under
# a symlinked tempdir (macOS's /var) the comparisons below build on root.
root <- tempfile("cerebro-scaffold-")
dir.create(root)
root <- normalizePath(root, winslash = "/")
instance <- file.path(root, "alice")

replace <- getFromNamespace(".scaffold_replace", "cerebro")
expect_identical(replace("{{name}}.service", "{{name}}", "alice"),
                 "alice.service")
expect_identical(replace("x={{name}}", "{{name}}", "alice"), "x=alice")
expect_identical(replace("{{name}}", "{{name}}", "alice"), "alice")

dry <- cerebro::scaffold_persona("alice", instance_dir = instance,
                                 vault = "tracked", timer = "*-*-* 03:30:00",
                                 dry_run = TRUE)
expect_false(dir.exists(instance))
expect_true(all(dry$action %in% c("would_create", "unchanged")))

report <- cerebro::scaffold_persona("alice", instance_dir = instance,
                                    vault = "tracked",
                                    timer = "*-*-* 03:30:00",
                                    timezone = "America/Chicago")
expect_true(dir.exists(instance))
expect_true(any(report$action == "created"))
expect_true(file.exists(file.path(instance, "config.yaml")))
expect_true(file.exists(file.path(instance, "alice.service")))
expect_true(file.exists(file.path(instance, "alice.timer")))
expect_true(file.exists(file.path(instance, ".corteza", "config.json")))
expect_true(file.exists(file.path(instance, ".claude", "settings.local.json")))
expect_true(file.exists(file.path(instance, "todo", "config.yaml")))
expect_true(file.exists(file.path(instance, "todo", "recurring.txt")))
expect_true(dir.exists(file.path(instance, "todo", "this_week")))
expect_true(dir.exists(file.path(instance, "todo", "archive")))
expect_true(dir.exists(file.path(instance, "vault", "raw")))
expect_true(dir.exists(file.path(instance, "vault", "wiki")))
expect_false(dir.exists(file.path(instance, "vault", "site")))
expect_false(file.exists(file.path(instance, "state.json")))
expect_false(file.exists(file.path(instance, "alice-restart.service")))

cfg <- yaml::read_yaml(file.path(instance, "config.yaml"))
expect_identical(cfg$instance, "alice")
expect_false(cfg$matrix$enabled)
expect_length(cfg$routines, 0L)

todo_cfg <- yaml::read_yaml(file.path(instance, "todo", "config.yaml"))
expect_identical(todo_cfg$tz, "America/Chicago")
expect_identical(todo_cfg$live_dir, file.path(instance, "todo", "this_week"))
expect_identical(todo_cfg$filename_fmt, "todo_%y%m%d_%s.md")

service <- readLines(file.path(instance, "alice.service"))
expect_true(any(grepl("CEREBRO_INSTANCE_DIR=", service, fixed = TRUE)))
expect_true(any(grepl("HACER_REPO=", service, fixed = TRUE)))
expect_true(any(grepl(instance, service, fixed = TRUE)))
timer <- readLines(file.path(instance, "alice.timer"))
expect_true(any(grepl("OnCalendar=*-*-* 03:30:00", timer, fixed = TRUE)))
expect_true(any(grepl("Unit=alice.service", timer, fixed = TRUE)))

ignore <- readLines(file.path(instance, ".gitignore"))
expect_true("/vault/site/" %in% ignore)
expect_false("/vault/" %in% ignore)
expect_equal(sum(ignore == "# >>> cerebro scaffold >>>"), 1L)

again <- cerebro::scaffold_persona("alice", instance_dir = instance,
                                   vault = "tracked",
                                   timer = "*-*-* 03:30:00",
                                   timezone = "America/Chicago")
expect_true(all(again$action == "unchanged"))

config_path <- file.path(instance, "config.yaml")
writeLines(c(readLines(config_path), "# local"), config_path)
conflict <- cerebro::scaffold_persona("alice", instance_dir = instance,
                                      vault = "tracked",
                                      timer = "*-*-* 03:30:00",
                                      timezone = "America/Chicago")
expect_identical(conflict$action[conflict$path == config_path], "conflict")
expect_true("# local" %in% readLines(config_path))

overwritten <- cerebro::scaffold_persona("alice", instance_dir = instance,
    vault = "tracked", timer = "*-*-* 03:30:00",
    timezone = "America/Chicago", overwrite = TRUE)
expect_identical(overwritten$action[overwritten$path == config_path],
                 "overwritten")
expect_false("# local" %in% readLines(config_path))

local_instance <- file.path(root, "local")
writeLines("custom-entry", file.path(root, "seed-ignore"))
dir.create(local_instance, recursive = TRUE)
file.copy(file.path(root, "seed-ignore"),
          file.path(local_instance, ".gitignore"))
cerebro::scaffold_persona("local", instance_dir = local_instance,
                          vault = "local")
local_ignore <- readLines(file.path(local_instance, ".gitignore"))
expect_true("custom-entry" %in% local_ignore)
expect_true("/vault/" %in% local_ignore)
expect_equal(sum(local_ignore == "# >>> cerebro scaffold >>>"), 1L)

none_instance <- file.path(root, "none")
cerebro::scaffold_persona("none", instance_dir = none_instance, vault = "none")
expect_false(dir.exists(file.path(none_instance, "vault")))
none_ignore <- readLines(file.path(none_instance, ".gitignore"))
expect_false("/vault/" %in% none_ignore)
expect_false("/vault/site/" %in% none_ignore)

bad_instance <- file.path(root, "bad")
dir.create(bad_instance)
writeLines("# >>> cerebro scaffold >>>", file.path(bad_instance, ".gitignore"))
expect_error(cerebro::scaffold_persona("bad", instance_dir = bad_instance,
                                       vault = "none"), pattern = "malformed cerebro scaffold block")
expect_false(file.exists(file.path(bad_instance, "config.yaml")))

expect_error(cerebro::scaffold_persona(""))
expect_error(cerebro::scaffold_persona("../alice"))
expect_error(cerebro::scaffold_persona("alice", vault = "invalid"))
expect_error(cerebro::scaffold_persona("alice", timer = ""))

# An instance dir under a symlink scaffolds the same way whether or not
# it exists yet. The creating call used to embed the raw path while
# every later call resolved it, so the service file conflicted with
# itself from the second call on.
target <- file.path(root, "linked-target")
dir.create(target)
linked <- file.path(root, "linked")
if (isTRUE(suppressWarnings(file.symlink(target, linked)))) {
    bob <- file.path(linked, "bob")
    first <- cerebro::scaffold_persona("bob", instance_dir = bob,
                                       vault = "none")
    expect_true(all(startsWith(first$path, file.path(target, "bob"))))
    second <- cerebro::scaffold_persona("bob", instance_dir = bob,
                                        vault = "none")
    expect_true(all(second$action == "unchanged"))
    service <- readLines(file.path(target, "bob", "bob.service"))
    expect_true(any(grepl(file.path(target, "bob"), service, fixed = TRUE)))
}

unlink(root, recursive = TRUE)
