# Agent skills for LAGOtrials

This directory holds [agent skills](https://github.com/aws/agent-toolkit-for-aws/tree/main/skills)
for using the LAGOtrials R package with AI coding agents. A skill is a
self-contained directory with a `SKILL.md` of curated instructions and triggers
that an agent loads on demand.

## Available skills

- [`lagotrials`](lagotrials/SKILL.md) — how to run LAGO optimizations with this
  package: the core `lago_optimization()` workflow, goal modes, cost functions,
  the outcome model options, confidence sets, common errors, and the result
  object.

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
