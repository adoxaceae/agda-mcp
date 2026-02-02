---
description: How to use Agda MCP for agentic proof development
---

This workflow describes the incremental, agentic process for developing Agda proofs using the `agda-mcp` server.

## 1. Environment Preparation

Before starting, ensure you have a `libraries` file for your project. This avoids global configuration issues and allows for multi-project isolation.

1.  Identify all `.agda-lib` dependencies for your project.
2.  Create a file named `libraries` in your project root containing absolute paths to these files.
    - Example:
      ```text
      /home/user/projects/agda-stdlib/standard-library.agda-lib
      /home/user/projects/other-lib/other-lib.agda-lib
      ```

## 2. Server Lifecycle

// turbo
1. Start the `agda-mcp` server from the `agda-mcp` project directory:
   ```bash
   nix develop -c cabal run agda-mcp
   ```
   *Note: Ensure no other process is using port 3000.*

## 3. Incremental Development Loop

Follow these steps for each proof or module:

1. **Load File**: Initialize the session by loading your target file.
   - Use the `agda_load` tool.
   - **Important**: Pass the `libraryFile` argument pointing to the `libraries` file created in Step 1.
   - Specify a `sessionId` if you are working on multiple projects simultaneously.

2. **Define Signatures**: Write your type signatures and lemma statements first.
   - Use Agda holes `?` for implementation details.
   - Reload the file (`agda_load`) to verify the high-level strategy.

3. **Explore Goals**: List and inspect holes to understand what needs to be proved.
   - Use `agda_get_goals` to see all active holes.
   - Use `agda_goal_type_context` for a specific hole to see available variables and the required type.

4. **Iterative Refinement**: Fill holes one by one.
   - Use `agda_refine` to introduce constructors or function applications.
   - Use `agda_case_split` when pattern matching is needed.
   - Use `agda_give` to provide a complete solution for a hole.

5. **Verify and Repeat**: After each major refinement, reload the file.
   - The MCP server will automatically apply file edits.
   - Check standard output/error for type-checking status.

## 4. Final Validation

// turbo
1. Once all holes are filled, run a final type-check using the command line to ensure total correctness:
   ```bash
   nix develop -c agda <filename>
   ```
   *Inspect the output directly for errors.*
