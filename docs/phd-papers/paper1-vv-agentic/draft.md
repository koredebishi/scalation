# V&V in the Age of Agentic AI: Who Guards the Validity of Discrete-Event Simulation?

**Target:** WSC 2027 (foundations track) · backup ANNSIM 2027 · arXiv preprint first
**Type:** Position paper — writing only, no new code
**Bib:** `refs.bib` (cite as `[@key]`; becomes `\cite{key}` in LaTeX later)
**Status:** ☐ skeleton → ☐ full prose → ☐ internal review → ☐ LaTeX port → ☐ submitted

---

## Abstract (working version — tighten last)

Classical verification and validation for discrete-event simulation assumes a
human-organized modeling process: objectives are specified in advance, changes are
deliberate, and validity judgments are made at identifiable review points
[@sargent2010verification; @balci1998vv; @law1991simulation]. When an LLM agent
autonomously generates and refines simulation experiments, part of the human
researcher's traditional role as guarantor of experimental validity is transferred
to the agent — and the *mechanisms* for applying V&V stop scaling, even though the
principles stand. Manual tracing, the most powerful debugging tool the classical
literature offers, does not survive an agent that produces scenarios faster than
any researcher can read them. This paper argues that V&V for DES must be restated
in machine-checkable form: Sargent's three validity types become three evidence
pillars an architecture can record — formal, empirical, and methodological. We
locate the four known agentic failure factors [@koohestani2025agentguard] in the
concrete layers of a DES pipeline, draw the boundary between what an agent may
decide and what must remain with the researcher, and close with the open problems:
termination logic, statistical quarantine criteria, audit-trail navigation at
scale, and state pruning.

> **Framing guard (do not lose this):** never claim "classical V&V breaks."
> The defensible claim: *the principles remain valid, but the mechanisms for
> applying them no longer scale when experiments are generated autonomously.*
> Reviewers who know Sargent/Balci/Law will kill the stronger claim.

---

## 1. Introduction

**Claim.** The human researcher has always been the invisible guarantor of DES
validity; agent-paced experimentation removes that guarantor without replacing it.

**Evidence.** Personal narrative (real, from Q1): manual tracing of the ScalaTion
agenda worked *because I set the inputs myself*; the methodological reasoning
(why Shifted Erlang tau, why Gipps→IDM) lived only in my head. At machine pace,
that reasoning is lost unless the architecture records it.

**Sources.** q1.tex §Q1.1; engineering doctrine that head-knowledge must become
machine-checkable infrastructure (industry framing, not academic citation).

**TODO:** open with the manual-tracing story, then the transfer-of-role sentence,
then the paper's one-line thesis.

## 2. What classical V&V assumes

**Claim.** Each core classical practice makes a hidden assumption about the pace
and visibility of the modeling process — state each precisely.

**Evidence.** Table: practice → hidden assumption → what agent pace does to it.
(Objectives in advance; deliberate changes; review points; manual tracing.)

**Sources.** [@sargent2010verification; @sargent2013introduction;
@sargent2020verification; @balci1997verification; @balci1998vv; @law1991simulation].

**TODO:** the table is the section. 4–5 rows, one paragraph per row.

## 3. The four agentic failure factors, located in DES

**Claim.** Stochasticity, hallucination, emergent unintended behavior, and
pipeline vulnerabilities [@koohestani2025agentguard] are not abstract — each lands
in a specific DES layer (knowledge, agent, scenario DSL, engine, evaluation,
feedback), with a concrete traffic failure for each.

**Evidence.** Wrong-context parameter retrieval; DSL translation error; invalid
arrival pattern producing an artificial queue that the feedback loop then rewards
(the real Poisson same-tick artifact from our work).

**Sources.** [@koohestani2025agentguard; @kwon2026aivv; @srinivasan2025dura;
@rabanser2026towards]; q1.tex §Q1.3; own experience [@bishi2026idm].

## 4. The three pillars, machine-checkable

**Claim.** Sargent's conceptual / operational / computerized-model validity
translate to formal, empirical, and methodological *evidence* that a system can
record — because the reasoning that lived in the researcher's head now has
nowhere else to live.

**Evidence.** Pillar table + where each is produced. Formal = invariants over the
executed trace (implemented: conservation counters, gap/kinematic checks in
ScalaTion — cite Paper 2). Empirical = fit vs PeMS + sensitivity sweeps
[@bishi2026idm]. Methodological = retrieval trace + hypothesis lineage +
parameter-change log.

**Sources.** [@sargent2010verification]; q1.tex §Q1.5; Paper 2 (companion).

## 5. The three-role decision boundary

**Claim.** Gate (deterministic) / agent (LLM) / human — the gate alone decides
admissibility; the agent diagnoses and proposes but cannot overrule a red
invariant; the human alone declares a discovery. Because the modeler is now
stochastic, the monitor must remain deterministic: a watcher only adds assurance
if its failure modes are independent of the watched.

**Evidence.** Self-reflection is a productivity feature, not a safety property
[@rabanser2026towards]; contrast with adjudication approaches [@kwon2026aivv;
@srinivasan2025dura].

**Sources.** q1.tex §Q1.5; committee follow-up comments.

## 6. Open problems (the committee's own questions)

1. **Stopping criterion** — when does the loop decide "good enough"? Explicit
   objective + termination logic.
2. **Quarantine specification** — how many replications distinguish artifact
   from emergence; which statistical test. (Answered in Paper 2 — here it is
   *posed* as the community's problem.)
3. **Audit trail at scale** — navigating hypothesis lineage after 1,000
   iterations.
4. **State growth** — pruning branching scenario trees.

## 7. A research agenda for DES V&V under agency

**Claim.** The DES community is behind the agentic-systems literature on its own
core competency; state what to build next (the pillars as community
infrastructure, conformance suites, the deterministic-gate pattern).

**Sources.** [@zhou2025autonomous; @yao2022react] for how patterns win by being
small and named; [@lopez2018sumo] for where the user base is.

---

## Figures

1. The V&V transfer problem (two-column diagram — exists as ASCII in the brief,
   redraw for print).
2. Three pillars + three-role boundary (one figure, reuse
   `assets/AutoTrafficSim.png` from the exam if it fits).

## Porting plan

Draft in this file with `[@keys]` → when sections stabilize, paste into the WSC
kit (or `acmart` for backup venues), find-replace `[@key]` → `\cite{key}`, drop
in `refs.bib` unchanged.
