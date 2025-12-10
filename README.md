# Wax Compiler Toolchain

Wax is a compiler toolchain for a Rust-like syntax targeting WebAssembly.

## Installation

**Requirements:** [Opam](https://opam.ocaml.org/) (2.1+) and OCaml 5.0+.

```sh
# Install dependencies
opam install . --deps-only

# Build
dune build

# Run tests
dune runtest

# Install
opam install .
```

## Capabilities

*   **Formatting:** Ensures consistent code style.
*   **Type Checking:** Verifies the semantic correctness of Wax code.
*   **Syntax Conversion:** Supports conversion between Wax, WebAssembly Text (WAT), and eventually WebAssembly Binary.

## Documentation
Full documentation is available at [vouillon.github.io/wax/](https://vouillon.github.io/wax/).
You can also build the documentation locally using `mdbook build docs`.

## CLI Interface


**Usage:** `wax [OPTION]… [INPUT]

### Positional Arguments

*   `[INPUT]`: Source file to convert/format. Optional. If omitted, the tool reads from `stdin`.

### Options

*   `-f`, `--format`, `--output-format`: Output format (Default: Auto/`wasm`). Values: `wat`, `wasm`, `wax`.
*   `-i`, `--input-format`: Input format (Default: Auto/`wax`). Values: `wat`, `wasm`, `wax`.
*   `-o`, `--output`: Output file (Default: `stdout`).
*   `-v`, `--validate`: Perform validation (type checking for Wax, well-formedness for Wasm Text). Validation is disabled by default.
*   `-s`, `--strict-validate`: Perform strict reference validation (for Wasm Text). This overrides the default relaxed reference validation behavior.
*   `--color`: Color output: `always`, `never`, or `auto` (default). `auto` colors only if output is a TTY.

## Example

![Example](/assets/example.png)
