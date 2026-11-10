% tinyrox says don't edit this manually, but it can't stop you!
\name{scaffold_persona}
\alias{scaffold_persona}
\title{Scaffold a cerebro persona repository}
\usage{
scaffold_persona(
  name,
  instance_dir = file.path("~", name),
  vault = c("local", "tracked", "none"),
  timezone = Sys.timezone(),
  timer = NULL,
  overwrite = FALSE,
  dry_run = FALSE
)
}
\arguments{
\item{name}{Character. Persona identifier used in config and service names.
Must contain only letters, numbers, underscores, and hyphens.}

\item{instance_dir}{Character. Persona repository or project directory.}

\item{vault}{Character. Vault policy: \code{"local"} creates a vault ignored
by Git, \code{"tracked"} creates a vault whose source may be tracked while
its rendered site is ignored, and \code{"none"} creates no vault.}

\item{timezone}{Character. IANA timezone written to \code{todo/config.yaml}.}

\item{timer}{Character or NULL. Optional systemd OnCalendar value for a
generated \code{<name>.timer} targeting \code{<name>.service}. The default
service is long-running, so use this only when the instance overrides that
service to be one-shot or normally inactive.}

\item{overwrite}{Logical. Replace existing scaffold files that differ from
the current templates.}

\item{dry_run}{Logical. Report planned actions without writing.}
}
\value{
A data frame with one row per managed path and its action, invisibly.
}
\description{
Creates the generic structural files and directories used by a cerebro
persona. Existing files are preserved by default: files that differ from
the current template are reported as conflicts unless \code{overwrite} is
true. The managed cerebro block in \code{.gitignore} is updated in place
while preserving all content outside that block.
}
\details{
The generated config is intentionally inactive: Matrix is disabled and no
routines, credentials, provider, model, rooms, or persona content are
supplied. Those remain explicit instance-owned choices.

}
