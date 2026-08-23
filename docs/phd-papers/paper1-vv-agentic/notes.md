# Paper 1 — working notes

## Source material (mine these, never paste-plagiarize — rewrite fresh)

- `data/Oral-Exam-Written-Directory/Comprehensive-Exam-Written/q1.tex`
  — Q1.1 (classical assumptions), Q1.3 (failure factors), Q1.5 (pillars + roles)
- `docs/phd-papers/01-vv-age-of-agentic-ai.md` — the original brief (abstract,
  motivation, methodology drafts live there; this folder supersedes it for writing)
- Committee follow-up comments (the four open problems are THEIR questions —
  saying so in the paper is a strength)

## Standing warnings

- **Do not claim classical V&V "breaks."** Claim: principles valid, mechanisms
  don't scale. (Brief's "My only concern" note — keep it taped to the monitor.)
- No hallucinated citations. Every entry in refs.bib is copied from the exam
  bibs or is our own paper. Add new ones only after reading them.
- All prose original — the exam text is source material, not copy material.

## Ideas parking lot

- The "watcher independence" argument (monitor deterministic because modeler is
  stochastic) is the paper's sharpest sentence — make it a pull-quote.
- The 2026-07 engineering episodes are usable as one-line evidence: the
  I/O-bound trace run (why snapshots-not-streams), the deterministic fingerprint
  (121.530785 — why exact-match regression is even possible).
- Possible reviewer: "isn't this just MLOps for simulation?" Answer in §7:
  DES has a property MLOps lacks — physics gives deterministic oracles.
