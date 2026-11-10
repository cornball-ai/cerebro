# cerebro development guide

cerebro is an R package first. It is also the source of truth for the
runtime, shared behavior, and canonical shape of the assistant personas built
on it.

The package owns the machinery. Each persona repository owns its identity,
configuration values, prompts, task content, and knowledge. Do not copy shared
runtime logic into persona repositories.

## Architecture

cerebro has three responsibilities:

1. **Runtime.** `run()` loads an instance's `config.yaml`, registers routines,
   and drives the scheduler and optional Matrix loop.
2. **Shared behavior.** Reusable operations such as `sync_todo()` live in
   exported, documented, tested R functions.
3. **Scaffolding.** cerebro defines and generates the structural files that
   make a repository a persona instance.

Persona repositories remain standalone repositories. They are thin instances
of cerebro, not source directories embedded in this package and not entries
relocated under `~/.corteza/instances/`.

There are two supported repository shapes:

- A pure persona repository, where the repository exists to host the
  assistant.
- A project with an attached persona layer, where the project remains primary
  and cerebro manages only the small set of persona files.

The generator must support both without taking ownership of unrelated project
files.

## Canonical persona shape

| Path | Purpose | Tracking rule |
|---|---|---|
| `config.yaml` | cerebro routines, briefings, and Matrix settings | Track |
| `CLAUDE.md` and `AGENTS.md` | Persona identity and agent instructions | Track |
| `<name>.service` and optional `.timer` | User-scoped service definitions | Track |
| `prompts/*.md` | Persona-owned system prompts | Track |
| `.corteza/config.json` | corteza instance settings | Track |
| `.claude/settings.local.json` | Claude Code permissions for the instance | Track |
| `scripts/` | Persona-specific entry points and tooling | Track |
| `todo/` metadata | hacer configuration and recurring-task definitions | Track |
| `todo/this_week/` | Live planning state | Ignore unless the instance explicitly owns it as source |
| `vault/` | pensar knowledge-base source | Per-instance policy |
| `vault/site/` | Rendered knowledge-base output | Ignore; export under `~/Sync` |
| `state.json`, logs, caches | Runtime state | Ignore |
| Generated media and reports | Runtime output | Ignore or store under `~/Sync` |

Secrets never belong in a persona repository or in cerebro. Matrix credentials
remain in the user's external corteza configuration.

At the cerebro package root, `AGENTS.md` is intentionally a symlink to this
file so Claude and Codex receive one canonical development guide. Generated
personas receive separate starter files because their identity and operational
instructions may diverge.

`vault/` is the canonical persona-facing name because it matches pensar and
the existing persona repositories. The current `agent_paths()` API exposes
legacy `wiki` path names. Any code migration must preserve compatibility until
downstream instances have moved; run `saber::blast_radius()` before changing
that exported contract.

Vault source tracking is intentionally configurable by persona. The scaffold
must not assume that knowledge-base source is public or Git-tracked. Rendered
sites are always generated output.

## Scaffold contract

The scaffold API is user-invoked. Package installation and attachment must
never create persona directories or files.

The generator should:

- Create a new persona layer from templates shipped under `inst/`.
- Add missing structural files to an existing standalone or attached persona.
- Be idempotent.
- Preserve persona-authored content by default.
- Report conflicts instead of silently replacing modified files.
- Keep provider, model, schedules, rooms, and vault tracking as explicit
  per-instance choices.
- Generate the long-running service. Generate an optional `<name>.timer` only
  when the caller supplies a concrete systemd `OnCalendar` value; never invent
  a schedule. The timer targets `<name>.service`, so it is appropriate only
  when the instance overrides that service to be one-shot or normally inactive.
- Avoid generating secrets, live todos, runtime state, rendered sites, or
  media.

Template defaults define structure, not identity. Persona-specific prose and
configuration values stay in the persona repository.

## R package constraints

All package changes must remain CRAN-conformant:

- R code lives in `R/`, is documented with tinyrox into `man/`, and is tested
  with tinytest under `inst/tinytest/`.
- Declare real runtime dependencies honestly in `DESCRIPTION`.
- Do not bundle persona configs, vault content, todos, prompts, credentials,
  logs, or media in the package.
- Development-only files belong in `.Rbuildignore`.
- Do not create files outside explicit user-invoked functions.
- Prefer base R and deterministic behavior.

Before changing R code, inspect the package with saber rather than inferring
its API:

```bash
r -e 'str(saber::symbols("."))'
```

Before renaming, moving, or changing the signature of an exported function,
run:

```bash
r -e 'saber::blast_radius("function_name", ".")'
```

Use the standard package workflow after implementation:

```bash
r -e 'rformat::rformat_dir("R", control_braces = "multi", expand_if = TRUE); tinyrox::document(); tinypkgr::install(); tinytest::test_package("cerebro")'
```

Use `tinypkgr::check()` for the package check.

## Development workflow

- Work on a branch; do not develop directly on `main`.
- Keep one responsibility per file.
- Do not mix unrelated cosmetic edits into substantive changes.
- Do not add `Co-Authored-By` trailers or force-push.
- Let the user review README and other public documentation before committing.
- Use incremental commits during review and squash when merging the PR.
- Restart long-running persona services after reinstalling cerebro or one of
  its loaded dependencies.

Development plans and task notes live outside the repository in
`~/tasks/cerebro/`. The active implementation plan is
`~/tasks/cerebro/persona-cleanup-plan.md`. They are development artifacts,
not package content.
