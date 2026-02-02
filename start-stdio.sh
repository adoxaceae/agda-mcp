#!/usr/bin/env bash
export AGDA_DIR=/home/bsaul/.agda
BINARY="/home/bsaul/projects/agda-mcp/dist-newstyle/build/x86_64-linux/ghc-9.10.3/agda-mcp-0.1.0.0/x/agda-mcp/build/agda-mcp/agda-mcp"
# Run inside nix develop but using the pre-built binary
exec nix develop /home/bsaul/projects/agda-mcp -c "$BINARY" --stdio
