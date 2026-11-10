# cerebro runtime image: this checkout of cerebro and everything it loads,
# on rocker/r2u. Build from the repository root:
#
#   podman build -t cerebro .
#
# CRAN packages install as r2u apt binaries; packages CRAN does not carry
# yet come as source from the repository named in DESCRIPTION's
# Additional_repositories. Pin BASE to a digest for a reproducible build.
ARG BASE=docker.io/rocker/r2u:noble
FROM ${BASE}

ENV DEBIAN_FRONTEND=noninteractive

# git and gh: vault commits and the git-reading briefing sources.
# littler: the `r` front end that instance scripts and the default
# command use.
RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates git gh littler \
    && rm -rf /var/lib/apt/lists/*

# Dependencies come from DESCRIPTION, so the list cannot drift from the
# package. Copying DESCRIPTION alone keeps this layer cached until a
# dependency changes. Suggests are included: they are the optional
# integrations an instance turns on in config.yaml. EXTRA_PACKAGES adds
# packages nothing declares; mx.crypto gives Matrix end-to-end encryption.
#
# The first pass goes through bspm, which takes the apt binary of any
# package CRAN carries. The second pass replaces each package whose
# Additional_repositories version is newer than the one that installed:
# those are the development versions DESCRIPTION's floors ask for.
ARG EXTRA_PACKAGES="mx.crypto"
COPY DESCRIPTION /opt/src/cerebro/DESCRIPTION
RUN Rscript -e ' \
    d <- read.dcf("/opt/src/cerebro/DESCRIPTION"); \
    field <- function(f) if (f %in% colnames(d)) d[1L, f] else ""; \
    deps <- unlist(strsplit(c(field("Depends"), field("Imports"), field("Suggests")), ",")); \
    deps <- trimws(sub("\\(.*", "", deps)); \
    extra <- strsplit(trimws(Sys.getenv("EXTRA_PACKAGES")), "[[:space:]]+")[[1L]]; \
    base <- rownames(installed.packages(priority = "base")); \
    pkgs <- setdiff(unique(c(deps, extra)), c("", "R", base)); \
    extra_repos <- trimws(strsplit(field("Additional_repositories"), ",")[[1L]]); \
    repos <- c(extra_repos, getOption("repos")); \
    message("installing: ", paste(pkgs, collapse = " ")); \
    install.packages(pkgs, repos = repos); \
    ap <- available.packages(repos = extra_repos); \
    ip <- installed.packages(); \
    both <- intersect(rownames(ap), rownames(ip)); \
    newer <- both[package_version(ap[both, "Version"]) > package_version(ip[both, "Version"])]; \
    if (length(newer)) { \
        message("from ", paste(extra_repos, collapse = " "), ": ", paste(newer, collapse = " ")); \
        bspm::disable(); \
        install.packages(newer, repos = repos, type = "source") \
    }; \
    miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]; \
    if (length(miss)) stop("not installed: ", paste(miss, collapse = " ")) \
    ' \
    && rm -rf /var/lib/apt/lists/* /tmp/Rtmp*

COPY . /opt/src/cerebro
RUN R CMD INSTALL --no-docs /opt/src/cerebro \
    && Rscript -e 'library(cerebro); cat("cerebro", as.character(packageVersion("cerebro")), "\n")'

# r2u enables bspm in Rprofile.site so install.packages() goes through apt
# over D-Bus. Nothing installs at runtime, and without D-Bus the hook
# warns on every R start; take it out.
RUN sed -i --follow-symlinks 's/^suppressMessages(bspm::enable())/# runtime image: bspm disabled/' /usr/lib/R/etc/Rprofile.site \
    && ! Rscript -e 'invisible(1)' 2>&1 | grep -q 'D-Bus'

# The image holds no persona. Mount an instance directory at /instance
# and the credentials it needs, and run as a non-root user:
#
#   podman run --userns keep-id -v ~/mybot:/instance cerebro
#
# Secrets stay outside the image and outside the instance repository.
WORKDIR /instance
CMD ["r", "-e", "cerebro::run('/instance')"]
