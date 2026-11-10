library(tinytest)

# R checks an Imports version floor when a namespace loads only for the
# packages the NAMESPACE imports from: loadNamespace() passes the floor
# as versionCheck to each import directive, while a `pkg::` call loads
# the namespace with no check at all. Every Imports entry that carries
# a floor therefore has an import directive, so a stale copy earlier on
# the library path fails at loadNamespace("cerebro") instead of being
# used. Depends floors are checked at load regardless.
desc <- read.dcf(system.file("DESCRIPTION", package = "cerebro"), "Imports")
entries <- trimws(strsplit(desc[1L, "Imports"], ",")[[1L]])
floored <- sub("\\s*\\(.*$", "", entries[grepl("(", entries, fixed = TRUE)])
expect_true(length(floored) > 0L)
imported <- names(getNamespaceImports("cerebro"))
for (p in floored) expect_true(p %in% imported, info = p)
