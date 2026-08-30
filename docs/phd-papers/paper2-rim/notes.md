# Paper 2 — working notes

## Source material

- **`data/Oral-Exam-Written-Directory/Comprehensive-Exam-Written/Question1ref/oral_equation.pdf`**
  — the oral-slide equation deck: p.1 invariant predicates, p.2 the n₀=10
  quarantine rule (Law & Kelton relative precision), p.3 the RIM-constrained
  objective, p.9 shockwave formula, p.16 causality+feasibility gates,
  p.23 committee comments with slide numbers. REUSE THE NOTATION.

- `data/Oral-Exam-Written-Directory/Comprehensive-Exam-Written/q1.tex` — Q1.2
  (invariants), Q1.3 (verdicts), committee follow-up on quarantine specification
- `docs/phd-papers/02-runtime-invariant-monitor.md` — original brief (working
  tree, standardization section, venue table)
- `context/graph-network-architecture.md` — spec for the RoadGraph substrate

## Engineering episodes to use as evidence (all real, 2026-07)

- **I/O-bound trace run:** unconditional per-step prints made the reference run
  I/O-bound — killed at 75 min with a 226 MB log. This is WHY snapshots, not
  streams. (§7)
- **Deterministic fingerprint:** fitness 121.530785 + coroutine counts
  (59757, 59753, 59750) reproduced exactly across runs → exact-match regression
  works. (§7, and supports the conformance-suite idea)
- **Thread-dump diagnosis:** during a 2.5 h drain, dump showed 1,223 cars in
  stop-and-go and zero stuck merge waiters — external observability separating
  livelock from genuine jam. (Good §7 or §8 anecdote.)
- **The measures caught my own error:** first TrafficMeasuresTest scenario
  assumed free flow at 2 s headways; Edie speed reported 6.55 m/s because IDM's
  T=3 s makes that spacing congested. The instruments refused to lie. (§8 or
  intro hook.)

## Blockers / ordering

- §9 results need the runtime regression fixed first (runs at minutes, not hours).
- `monitor/` package is the remaining build; everything it senses already exists
  (RoadGraph counters, Edie, waveSpeed, vCount).
- Resolve the four VERIFY bib entries (edie1963traffic, wardrop1952road,
  blochwitz2011fmi, chen2001pems) before submission.

## Standing warnings

- Original prose only; exam text is source, not copy.
- The verdict path must stay LLM-free in every diagram and sentence.
