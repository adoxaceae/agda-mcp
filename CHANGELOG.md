# Changelog

All notable changes to agda-mcp will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2026-02-14

### Added
- New `agda_get_goal_type_implicits` tool to display goal types with implicit arguments visible (uses `Instantiated` rewrite mode)
- New `agda_get_context_implicits` tool to display context with implicit arguments visible (uses `Instantiated` rewrite mode)
- Better detection and reporting of unsolved implicit metavariables in goal summaries

### Changed
- Goal summaries now distinguish between visible goals and unsolved implicit metavariables
- Format.hs: Enhanced `formatAllGoalsWarnings` to report count of unsolved implicit goals

### Fixed
- Improved visibility of implicit arguments in type-checking contexts

## [0.1.0.0] - Initial Release

### Added
- MCP server implementation for Agda interaction
- Support for loading Agda files with library configuration
- Goal inspection and manipulation tools
- Context and type inference tools
- Case splitting and refinement operations
- Auto proof search capabilities
- Multi-agent session support with isolated REPL instances
- File edit operations (give, refine, case split, solve all)
- Resource endpoints for goals and file context
- Concise and Full response formats
- Project root detection for .agda-lib files
