<!-- nullkomma:agents:start -->
## nullkomma

This project's development environment comes from [nullkomma](https://github.com/dataheld/nullkomma).
Run commands inside its dev shell:
`direnv exec . <command>`, or `nix develop --command <command>` without direnv.
`nk` lists the project's workflows;
`nk <name> [args]` runs one.
Agents can also use them as MCP tools, from `.mcp.json`.
Both run in the working tree;
under `process-compose up`, tasks that rerun on changes may run in a copy,
named in their logs.

Check your work locally, from fastest to strictest:

1. While you work, run the narrowest task that covers the change.
1. Before you report a change as done, run `nk all`, then `nk flake-check`,
   and fix what fails.
   `nk all` formats, builds and runs every check in the working tree.
   `nk flake-check` runs all checks sandboxed, as CI does,
   and sees only files that Git tracks: `git add` new files first.

Skills for nullkomma itself are in `.agents/skills/nk-*`;
if they are missing, run `nk generate-files`.
<!-- nullkomma:agents:end -->
