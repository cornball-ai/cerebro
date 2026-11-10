library(tinytest)

resolve <- getFromNamespace(".resolve_path", "cerebro")
relative <- getFromNamespace(".thread_relative", "cerebro")

root <- tempfile("cerebro-paths-")
dir.create(root)
real <- normalizePath(root, winslash = "/")

# An existing path resolves as normalizePath() does.
expect_identical(resolve(root), real)

# A path that does not exist yet resolves through its existing ancestor,
# so it reads the same before and after it is created.
before <- resolve(file.path(root, "new", "page.md"))
expect_identical(before, file.path(real, "new", "page.md"))
dir.create(file.path(root, "new"))
writeLines("x", file.path(root, "new", "page.md"))
expect_identical(resolve(file.path(root, "new", "page.md")), before)

# Tilde and relative paths come back absolute.
expect_identical(resolve("~"), normalizePath("~", winslash = "/"))
expect_true(startsWith(resolve("no-such-dir/x.md"),
                       normalizePath(getwd(), winslash = "/")))

# Through a symlink, the vault and a page under it resolve to the same
# real directory, so a page that is not written yet is still inside the
# vault. normalizePath() alone resolves the existing vault and not the
# missing page, the mismatch a symlinked tempdir (macOS's /var) exposes.
link <- file.path(tempdir(), paste0("link-", basename(root)))
if (isTRUE(suppressWarnings(file.symlink(real, link)))) {
    expect_identical(resolve(file.path(link, "wiki", "coffee.md")),
                     file.path(real, "wiki", "coffee.md"))
    expect_identical(relative(file.path(link, "wiki", "coffee.md"), link),
                     "wiki/coffee.md")
    expect_identical(relative(file.path(link, "new", "page.md"), real),
                     "new/page.md")
    unlink(link)
}

# A path outside the vault is still refused.
other <- tempfile("cerebro-other-")
expect_error(relative(file.path(other, "x.md"), root),
             "outside the target vault")

unlink(root, recursive = TRUE)
