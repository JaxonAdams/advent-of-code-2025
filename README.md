# Advent of Code 2025

```
         |
        -+-
         A
        /=\               /\  /\    ___  _ __  _ __ __    __
      i/ O \i            /  \/  \  / _ \| '__|| '__|\ \  / /
      /=====\           / /\  /\ \|  __/| |   | |    \ \/ /
      /  i  \           \ \ \/ / / \___/|_|   |_|     \  /
    i/ O * O \i                                       / /
    /=========\        __  __                        /_/    _
    /  *   *  \        \ \/ /        /\  /\    __ _  ____  | |
  i/ O   i   O \i       \  /   __   /  \/  \  / _` |/ ___\ |_|
  /=============\       /  \  |__| / /\  /\ \| (_| |\___ \  _
  /  O   i   O  \      /_/\_\      \ \ \/ / / \__,_|\____/ |_|
i/ *   O   O   * \i
/=================\
       |___|
```

My solutions to [Advent of Code 2025](https://adventofcode.com/2025) implemented in Clojure.

## About

This repository contains my daily solutions to the 2025 Advent of Code challenges. Each solution emphasizes functional programming principles, leveraging Clojure's expressive syntax.

## Project Structure

```
.
├── src/
│   └── aoc/
│       ├── problems/     # Daily challenge solutions
│       └── utils/        # Shared utilities
├── test/
│   └── aoc/
│       └── problems/     # Unit tests
├── input/                # Puzzle inputs (gitignored)
└── deps.edn              # Project dependencies
```

## Prerequisites

- [Clojure CLI tools](https://clojure.org/guides/install_clojure) (version 1.12.1 or higher)
- Java 11 or higher

## Running Solutions

These solutions were developed using REPL-driven development with an editor integration, and this is the intended way to run each solution as well.

For editor integration (NeoVim/Conjure, VSCode/Calva, etc.):

```bash
clj -M:nrepl
```

This starts an nREPL server on port 7888 with CIDER middleware.

## Solution Approach

Each day's solution follows a consistent pattern:

1. **Input parsing** - Transform raw text into workable data structures
2. **Core logic** - Pure functions (where possible) implementing the puzzle algorithm
3. **Result computation** - Compose functions to produce the final answer

Solutions prioritize:
- **Readability** - Clear function names and logical decomposition
- **Functional style** - Immutable data, pure functions, sequence operations
- **Testability** - Small, composable functions with example test cases

## Development Tools

This project uses:
- **clj-kondo** - Linting and static analysis
- **nREPL** - Interactive development environment

## Input Files

Puzzle inputs are stored in `input/` and are gitignored. To run solutions, add your personal input files:

```
input/
└── day01/
    └── problem-1-input.txt
```

## License

Solutions are provided as-is for educational purposes. Advent of Code is created by [Eric Wastl](http://was.tl/). README ASCII art retreived from [asciiart.eu](https://www.asciiart.eu/holiday-and-events/christmas/trees).
