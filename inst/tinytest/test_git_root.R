library(tinytest)

# Which repository a working directory belongs to.
#
# `rev-parse --show-toplevel` inside a linked worktree returns the
# WORKTREE. Filing a conversation by that path gives a permanent Matrix
# room to a directory that exists in order to be deleted --
# ~/glinty/.claude/worktrees/gallery-loop was one of those, and it
# outlived the branch it was made for.
#
# Following the worktree back to its repo rather than skipping it means
# the conversation is still threaded, under the repo it was actually
# about.

gr <- getFromNamespace(".thread_git_root", "cerebro")

if (!nzchar(Sys.which("git"))) {
    exit_file("git not installed")
}

root <- file.path(tempdir(), paste0("cerebro-gitroot-", Sys.getpid(), "-",
                                    sample.int(1e6, 1L)))
main <- file.path(root, "repo")
dir.create(main, recursive = TRUE)

git <- function(dir, ...) {
    suppressWarnings(system2("git", c("-C", shQuote(dir), ...),
                             stdout = FALSE, stderr = FALSE))
}
git(main, "init", "-q")
git(main, "config", "user.email", "t@example.invalid")
git(main, "config", "user.name", "t")
writeLines("x", file.path(main, "f.txt"))
git(main, "add", "f.txt")
git(main, "commit", "-q", "-m", "initial")

norm <- function(p) normalizePath(p, mustWork = FALSE)

# ---- the ordinary case is untouched ------------------------------------
# In the main worktree the common git dir IS this repo's .git, so the
# toplevel comes back unchanged. Asserted first: everything below is a
# redirect, and a redirect that fires on ordinary repos would file every
# conversation under the wrong room.
expect_equal(gr(main), norm(main))

# A subdirectory resolves to the repo, not to itself.
sub <- file.path(main, "R")
dir.create(sub)
expect_equal(gr(sub), norm(main))

# ---- a linked worktree resolves to its repo ----------------------------
wt <- file.path(main, ".claude", "worktrees", "feature-x")
dir.create(dirname(wt), recursive = TRUE)
added <- git(main, "worktree", "add", "-q", "-b", "feature-x", shQuote(wt))

if (dir.exists(wt)) {
    # The thing this exists to prevent: the raw toplevel IS the worktree,
    # so without the redirect that disposable path becomes a room name.
    raw <- suppressWarnings(system2("git",
        c("-C", shQuote(wt), "rev-parse", "--show-toplevel"),
        stdout = TRUE, stderr = FALSE))
    expect_equal(norm(raw[[1L]]), norm(wt))

    # And what it returns instead.
    expect_equal(gr(wt), norm(main))

    # From inside a subdirectory of the worktree too, which is where a
    # session's cwd usually is rather than at its top.
    wtsub <- file.path(wt, "R")
    dir.create(wtsub, showWarnings = FALSE)
    expect_equal(gr(wtsub), norm(main))
} else {
    message("worktree add unavailable; skipped the redirect assertions")
}

# ---- not a repo at all -------------------------------------------------
# Empty, not an error and not the directory itself: the caller reads a
# non-empty string as "this session belongs to a repo".
plain <- file.path(root, "not-a-repo")
dir.create(plain)
expect_equal(gr(plain), "")

unlink(root, recursive = TRUE)
