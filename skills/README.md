# Agent skills for LAGOtrials

This directory holds [agent skills](https://github.com/aws/agent-toolkit-for-aws/tree/main/skills)
for using the LAGOtrials R package with AI coding agents. A skill is a
self-contained directory with a `SKILL.md` of curated instructions and triggers
that an agent loads on demand.

## Available skills

- [`lagotrials`](lagotrials/SKILL.md) covers how to run LAGO optimizations
  with this package: the core `lago_optimization()` workflow, goal modes, cost
  functions, the outcome model options including center level data,
  confidence sets and how to read them, sensitivity sweeps and budget searches,
  the app, the MCP server, common errors, and the result object.

To let an agent call the package as tools instead of writing R, use the MCP
server in [`python/`](../python/README.md#mcp-server). The skill and the
server work together: the skill explains the method and its caveats, and the
server runs it.

## Installing a skill

Copy the skill directory into your agent's skills path.

Claude Code:
```bash
# project-scoped
mkdir -p .claude/skills && cp -r skills/lagotrials .claude/skills/
# or user-scoped
mkdir -p ~/.claude/skills && cp -r skills/lagotrials ~/.claude/skills/
```

Other agents (Codex, Cursor, Kiro, ...) support skill directories too; place the
`lagotrials` directory in the location that agent reads skills from.

Once installed, the agent picks up the skill automatically when a task matches
the triggers in the skill's `description`.
