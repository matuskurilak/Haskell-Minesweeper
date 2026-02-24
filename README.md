# Minesweeper

A command-line Minesweeper game written in Haskell, featuring both a human-playable interface and an AI solver capable of winning **80–90% of games** on standard boards.

This project was developed as a school assignment at [KPI FEI TUKE](https://kpi.fei.tuke.sk/). The base project scaffold (board logic, UI, and cabal setup) was provided by the faculty. The core contributions — `HumanSolver.hs` and `ComputerSolver.hs` — were implemented independently.

---

## Features

- Interactive terminal-based gameplay for human players
- AI computer solver with a multi-stage decision pipeline
- Benchmark mode to measure solver win rate over many random games
- Clean Haskell codebase using Cabal for builds and dependency management

---

## AI Solver Design

The computer solver (`ComputerSolver.hs`) uses a layered pipeline of strategies, falling back to the next stage only when the current one cannot determine a safe move:

### 1. Rule-based deduction
Two classic Minesweeper rules are applied to every open tile:
- **Rule A (Mark):** If the number of remaining unflagged mines around a tile equals the number of closed neighbours, all closed neighbours are mines — flag them.
- **Rule B (Open):** If the number of flagged neighbours already equals the tile's number, all remaining closed neighbours are safe — open them.

### 2. Constraint Satisfaction (CSP)
When simple rules aren't enough, the solver builds a set of constraints from all open tiles — each constraint encodes *"these N cells contain exactly M mines"*. It then analyzes pairs of constraints using subset reduction:
- If constraint A is a subset of constraint B and their mine counts are equal, the cells in B but not A are all safe.
- If constraint A is a subset of constraint B and the mine count difference equals the number of differing cells, those cells are all mines.

### 3. Probabilistic fallback
When no deterministic move exists, the solver computes a mine probability for each closed tile using two signals:
- **Local probability:** derived from adjacent open tiles (how many of the tile's closed neighbours are accounted for as mines).
- **Global probability:** the baseline ratio of remaining mines to remaining closed tiles, used for tiles with no open neighbours.

Local probabilities from multiple neighbours are combined using the formula `1 - ∏(1 - pᵢ)` (independent probability union). The tile with the **lowest combined mine probability** is chosen, with ties broken by preferring tiles with more open neighbours (more information available).

### 4. First move strategy
The very first move is always placed at the center of the board, maximising the chance of opening a large connected area early.

---

## Project Structure


```
├── Minesweeper/
│   ├── Board.hs           — Core board types, state, and game logic (provided)
│   ├── UI.hs              — Terminal rendering and input handling (provided)
│   ├── HumanSolver.hs     — Human player input handling
│   └── ComputerSolver.hs  — AI solver (independently implemented)
├── Main.hs                — Entry point and benchmark runner
├── tuke-minesweeper.cabal
├── Makefile
├── LICENSE
├── README.md (this file)
└── .gitignore
```
---

## Getting Started

### Prerequisites

- [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler)
- [Cabal](https://www.haskell.org/cabal/)

### Build

```bash
cabal update
cabal build
```

### Play

Board size is optional and defaults to `small`. Available sizes: `small`, `medium`, `large`.

```bash
cabal run minesweeper                  # small board (default)
cabal run minesweeper small
cabal run minesweeper medium
cabal run minesweeper large
```

### Run the AI Benchmark

Runs N random games and reports the solver's win rate. Board size and number of runs are both optional.

```bash
cabal run minesweeper benchmark        # small board, 100 runs
cabal run minesweeper benchmark 50     # small board, 50 runs
cabal run minesweeper medium benchmark 200
cabal run minesweeper large benchmark 30
```

---

## License

This project is released under the MIT License. The copyright is held by **KPI FEI TUKE** (2021), as the base scaffold was provided by the faculty. See [LICENSE](LICENSE) for the full text.