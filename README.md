# Wax Compiler Toolchain

Wax is a compiler toolchain for a Rust-like syntax targeting WebAssembly.

## Capabilities

*   **Formatting:** Ensures consistent code style.
*   **Type Checking:** Verifies the semantic correctness of Wax code.
*   **Syntax Conversion:** Supports conversion between Wax, WebAssembly Text (WAT), and eventually WebAssembly Binary.

## CLI Interface

**Usage:** `wax -- [options] [INPUT]`

### Positional Arguments

*   `[INPUT]`: Source file to convert/format. Optional. If omitted, the tool reads from `stdin`.

### Options

*   `-f`, `--format`: Output format (Default: Auto/`wasm`). Values: `wat`, `wasm`, `wax`.
*   `-i`, `--input-format`: Input format (Default: Auto/`wax`). Values: `wat`, `wasm`, `wax`.
*   `-o`, `--output`: Output file (Default: `stdout`).

## Current Supported Pipelines

The CLI currently supports the following conversion flows:

1.  `wat` -> `wat` (Formatting / Round-trip)
2.  `wat` -> `wax` (Decompilation / Desugaring)
3.  `wax` -> `wax` (Formatting / Checking)

**Note:** `wasm` (binary) input/output is not yet implemented.
