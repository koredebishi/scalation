# PhD Publication Pipeline — Minimal Publishable Units

Seven papers sliced from the comprehensive-exam material (q1–q3), the committee's
follow-up questions, and the engine work. Each file holds: paper outline, working
tree (where coding is involved), abstract, motivation, methodology, implementation
diagram, and target venues.

**Recommended order: 1 → 2 → 4(formulation) → 3 → 5 → 6 → 7.**
Rationale: #1 stakes the framing claim with zero implementation risk; #2 is the core
dissertation novelty every later paper cites; #4's formulation version costs only
writing and claims the HPDES territory early; #3 is a cheap empirical win that
doubles as #2's motivation; #5 builds the link-agent substrate #4's implementation
needs; #6 is Study 2 retargeted; #7 is the committee-mandated benchmark and the
architecture's existence proof — do it last.

| # | File | Paper | Type | New work |
|---|------|-------|------|----------|
| 1 | [01-vv-age-of-agentic-ai.md](01-vv-age-of-agentic-ai.md) | V&V in the Age of Agentic AI | Position | Writing only |
| 2 | [02-runtime-invariant-monitor.md](02-runtime-invariant-monitor.md) | RIM: Runtime Invariant Monitor | Implementation | 5 invariants + quarantine test |
| 3 | [03-goodhart-calibration.md](03-goodhart-calibration.md) | When Calibration Lies (Goodhart) | Short empirical | Two-arm bounded SPSA |
| 4 | [04-hpdes-rollback-across-resolution.md](04-hpdes-rollback-across-resolution.md) | HPDES: Rollback Across Resolution | Formulation | Writing + cost model |
| 5 | [05-links-as-agents.md](05-links-as-agents.md) | Links as Agents | Implementation | LinkState + signals + routing |
| 6 | [06-evacuation-scenario-matrix.md](06-evacuation-scenario-matrix.md) | I-210/SR-134 Evacuation Matrix | Domain study | 27-cell matrix + fixes |
| 7 | [07-agentic-vs-sumo-benchmark.md](07-agentic-vs-sumo-benchmark.md) | Agentic vs SUMO Benchmark | Benchmark | Minimal agent loop |

**Engineering dependency:** Papers 4 (HPDES) and 5 (links-as-agents) build on the
graph-network paradigm — `context/graph-network-architecture.md` (the RoadGraph is
Burghout's "common module" and the substrate LinkState attaches to).

**Contribution disjointness** (so no reviewer calls two of them the same paper):
framing (1) / monitor (2) / calibration pathology (3) / synchronization (4) /
state estimation (5) / domain study (6) / workflow benchmark (7).

**Deadline notes (verify all before planning):** SIGSIM-PADS ~Jan; ANNSIM ~Jan;
WSC ~Apr; TRB traditionally Aug 1; ITSC early in the year. Check each venue's
arXiv-preprint policy before posting.
