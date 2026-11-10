# cerebro

**Persistent AI assistants in R.** Scheduled routines, conversational agents,
and Markdown knowledge vaults, built from R packages you can also use on their
own.

cerebro runs an assistant from a directory you control. Its configuration,
prompts, and routines are ordinary files. One R process runs the scheduler
and, when enabled, the Matrix chat loop.

## What it does

- **Scheduled work:** run R functions or shell commands on named schedules or
  five-field cron expressions. Routine state persists between restarts.
- **Conversation:** use corteza's agent runtime and optional Matrix integration
  alongside scheduled work.
- **Briefings:** combine configured evidence sources into a report, or prepare
  a draft and deliver that same draft later.
- **Knowledge and continuity:** file conversations into source-linked Markdown
  vaults and repository or topic threads; check vault consistency.
- **Assistant scaffolding:** create or update an assistant's directory structure
  while preserving files you've edited.

## Install

Install the development version from GitHub. The cornball.ai repository
supplies development dependencies that may be newer than their CRAN versions.
The current dependency stack requires R 4.4 or later.

```r
repos <- c(
    "https://cornball-ai.github.io/drat",
    "https://cloud.r-project.org"
)
install.packages("remotes", repos = repos)
remotes::install_github("cornball-ai/cerebro", repos = repos, upgrade = "never")
```

Check the component versions with:

```r
cerebro::status()
```

This is development software. See [NEWS.md](NEWS.md) for changes and
[GitHub issues](https://github.com/cornball-ai/cerebro/issues) for bugs and
feature requests.

## Create an assistant

Try the scaffold in a temporary directory:

```r
assistant_dir <- file.path(tempdir(), "myassistant")
cerebro::scaffold_persona(
    "myassistant", instance_dir = assistant_dir, vault = "local", dry_run = TRUE
)
cerebro::scaffold_persona(
    "myassistant", instance_dir = assistant_dir, vault = "local"
)
```

For a persistent assistant, pass a directory you own as `instance_dir`.
The same scaffold can add an assistant layer to an existing project.

The generated files include `config.yaml`, agent instructions, a system
prompt, corteza settings, optional vault directories, and a user service
template. Matrix starts disabled and the routine list starts empty. You choose
the assistant's identity, model, provider, credentials, and schedules.

Existing files that differ from the templates are reported as conflicts and
preserved by default. `vault = "local"` keeps vault contents out of Git;
`"tracked"` allows vault source to be tracked, and `"none"` omits the vault.

## Schedule work

For example, an assistant's `config.yaml` can schedule an hourly package-status
report:

```yaml
instance: myassistant

matrix:
  enabled: false

routines:
  - name: package status
    schedule: hourly
    capability: r
    expr: "print(cerebro::status())"
```

Start the scheduler in R:

```r
cerebro::run(assistant_dir)
```

`run()` reads `config.yaml`, registers the routines, restores their scheduling
state, and runs until interrupted. It also loads `routines.R` when that file
exists. Supported schedules include `"hourly"`, `"daily 09:00"`,
`"Mon-Fri 09:00"`, and cron expressions such as `"*/30 * * * *"`.

Routines can also be registered directly:

```r
cerebro::add_routine("package status", "hourly", function() {
    print(cerebro::status())
})
cerebro::list_routines()
```

Put those registrations in `routines.R` to reload them on startup. The scheduler
runs inside the R process; use a process supervisor when you need it to survive
logouts or restart after failure. The scaffold supplies a systemd user-service
template. It generates a timer only when you provide an explicit `timer` value.

## Add Matrix and knowledge workflows

Enable `matrix.enabled` after configuring corteza's external Matrix credentials.
The scheduler and chat bot then share one event loop. The instance configuration
can select the provider, model, system prompt, and tool filter. Credentials stay
outside the assistant repository.

Built-in routine capabilities cover briefings, deferred briefing delivery,
todo synchronization and reconciliation, Matrix archiving, conversation
lifecycle maintenance, vault checks, R expressions, and shell commands.

Conversation helpers can organize coding sessions by repository and general
conversations by topic. The [conversation lifecycle guide](vignettes/lifecycle.md)
describes how active segments, vault archives, and persistent topic threads fit
together. Configure destinations and participants explicitly for each instance.

## The R toolchain

| Package | Role |
| --- | --- |
| [corteza](https://github.com/cornball-ai/corteza) | Agent runtime, chat loop, and tool execution |
| [llm.api](https://github.com/cornball-ai/llm.api) | Connections to model providers |
| [saber](https://github.com/cornball-ai/saber) | Package discovery, code analysis, and project context |
| [pensar](https://github.com/cornball-ai/pensar) | Markdown knowledge vaults and their links |
| [hacer](https://github.com/cornball-ai/hacer) | Optional plain-text task planning |

cerebro combines these packages with its scheduler, assistant scaffolding,
briefing routines, and conversation workflows. Each component remains usable
on its own. `DESCRIPTION` records the actual required and optional dependencies.

R functions are also callable tools for agents that can run an R process:

```sh
Rscript --vanilla -e 'print(saber::pkg_exports("hacer"))'
Rscript --vanilla -e 'cat(saber::pkg_help("run_monday", "hacer"), sep = "\n")'
```

This supports on-demand discovery of a package's functions and documentation.
corteza also offers an MCP server when a client needs access to a persistent R
session.

## Operating an instance

The package owns shared runtime behavior. The assistant directory owns its
configuration, prompts, routines, and knowledge. Logs, live state, credentials,
and generated media should stay outside package source control.

After upgrading cerebro or a dependency loaded by a running assistant, restart
that assistant so it loads a consistent set of installed packages. Reinstalling
a package does not refresh an existing R process.

## License

[Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0).
