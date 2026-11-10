library(tinytest)

# Setting the bot's Matrix avatar from the instance config: the
# best-effort guard paths, the md5 idempotence that keeps a restart from
# re-uploading an unchanged picture, and the one upload that does happen,
# through chat.api's identity verb on a Matrix adapter with the upload
# seamed out (helper_matrix.R). The guard paths reach no network: they
# return first, and the one path that would build a client is pointed at
# an absent creds file so it fails before any homeserver call.

source("helper_matrix.R")
apply_avatar <- cerebro:::.apply_matrix_avatar

dir <- tempfile("cerebro-avatar-")
dir.create(dir)

old_cfg <- Sys.getenv("CORTEZA_MATRIX_CONFIG", unset = NA)
Sys.setenv(CORTEZA_MATRIX_CONFIG = file.path(dir, "absent.json"))

# --- Nothing configured: a no-op, no state written ---
expect_false(apply_avatar(NULL, dir))
expect_false(apply_avatar("", dir))
expect_false(apply_avatar(NA_character_, dir))
expect_false(apply_avatar(c("a", "b"), dir))

# --- A configured path that does not exist ---
expect_false(apply_avatar(file.path(dir, "nope.png"), dir))

# --- A real file, but the creds config is absent: the attempt fails,
# returns FALSE, and leaves the state unstamped so the next boot retries.
f <- file.path(dir, "face.png")
writeBin(as.raw(c(1, 2, 3, 4)), f)
expect_false(apply_avatar(f, dir))
expect_null(cerebro::read_instance_state(dir)$matrix_avatar_md5)

# --- Idempotent skip: a stamped, unchanged file is not re-sent. The skip
# returns before reading creds, so the absent config is never touched.
md5 <- unname(tools::md5sum(f))
cerebro::update_instance_state(matrix_avatar_md5 = md5, instance_dir = dir)
expect_false(apply_avatar(f, dir))
expect_identical(cerebro::read_instance_state(dir)$matrix_avatar_md5, md5)

# --- A changed file is no longer covered by the stamp, so it is tried
# again (and fails cleanly here), leaving the old stamp untouched.
writeBin(as.raw(c(5, 6, 7, 8)), f)
expect_false(apply_avatar(f, dir))
expect_identical(cerebro::read_instance_state(dir)$matrix_avatar_md5, md5)

# --- The upload itself, with a client supplied: one identity call
# carrying the file, then the md5 stamp; the same file again is skipped
# without a second call.
if (requireNamespace("mx.client", quietly = TRUE)) local({
    seen <- list()
    cl <- matrix_client(.avatar = function(mx, path) {
        seen[[length(seen) + 1L]] <<- path
        "mxc://ex/abc"
    })
    g <- file.path(dir, "new.png")
    writeBin(as.raw(c(9, 9, 9)), g)
    expect_true(suppressMessages(apply_avatar(g, dir, client = cl)))
    expect_identical(seen, list(g))
    expect_identical(cerebro::read_instance_state(dir)$matrix_avatar_md5,
                     unname(tools::md5sum(g)))
    expect_false(apply_avatar(g, dir, client = cl))
    expect_identical(length(seen), 1L)
})

if (is.na(old_cfg)) {
    Sys.unsetenv("CORTEZA_MATRIX_CONFIG")
} else {
    Sys.setenv(CORTEZA_MATRIX_CONFIG = old_cfg)
}
unlink(dir, recursive = TRUE)
