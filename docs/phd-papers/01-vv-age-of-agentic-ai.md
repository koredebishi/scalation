# Paper 1 — V&V in the Age of Agentic AI: Who Guards the Validity of Discrete-Event Simulation?

**Type:** Position paper (writing only — no new code required)
**Source material:** q1.tex (Q1.1, Q1.3, Q1.5), committee follow-up comments
**Dissertation chapter:** Framing chapter for AutoTrafficSim V&V

---

## Paper Outline

1. **Introduction** — the human researcher as the traditional guarantor of experimental
   validity; what changes when an LLM agent proposes, revises, and justifies experiments
   faster than a researcher can inspect them.
2. **What classical V&V assumes** — Sargent, Balci, Law: objectives specified in advance,
   deliberate changes, validity judgments at identifiable review points, manual tracing
   as the debugging workhorse. Why none of these assumptions survive agent-paced
   experimentation.
3. **The four agentic failure factors mapped to DES** — stochasticity, hallucination,
   emergent unintended behavior, new pipeline vulnerabilities (AgentGuard), each located
   in a concrete DES layer (knowledge, agent, scenario DSL, engine, evaluation, feedback).
4. **The three pillars, machine-checkable** — translating Sargent's conceptual /
   operational / computerized-model validity into formal, empirical, and methodological
   evidence that an architecture can record, because the methodological reasoning that
   used to live in the researcher's head now has nowhere to live unless the system
   writes it down.
5. **The human/agent decision boundary** — what the agent decides on its own (reject,
   quarantine, revise) and what it must never decide (whether a surprising trace is a
   discovery).
6. **Open problems** (from the committee's own questions):
   - Stopping criterion — when does the feedback loop decide "good enough"? Explicit
     objective function and termination logic.
   - Quarantine specification — how many replications distinguish artifact from
     emergence, and what statistical test makes the call.
   - Audit trail at scale — how a researcher navigates a hypothesis lineage after
     1,000 iterations.
   - State growth — pruning strategy for branching scenario trees.
7. **A research agenda for DES V&V under agency** — what the simulation community
   should build next, and why DES is currently behind the agentic-systems literature
   on its own core competency.

## Working Tree

Writing only. No code. Figures reuse the AutoTrafficSim architecture diagram
(`assets/AutoTrafficSim.png` from the exam document).

---

## Abstract

Classical verification and validation for discrete-event simulation assumes a
human-organized modeling process: objectives are specified in advance, changes are
deliberate, and validity judgments are made at identifiable review points. When an LLM
agent autonomously generates and refines simulation experiments, part of the human
researcher's traditional role as guarantor of experimental validity is transferred to
the agent, and every one of those assumptions breaks. Manual tracing, the most powerful
debugging tool the classical literature offers, does not survive an agent that produces
scenarios faster than any researcher can read them. This paper argues that V&V for DES
must be restated in machine-checkable form. We translate Sargent's three validity types
into three evidence pillars an architecture can record — formal evidence that a trace
preserves physical and event-ordering invariants, empirical evidence that conclusions
hold across sensitivity sweeps against external ground truth, and methodological
evidence that captures the retrieval sources and hypothesis lineage behind every
scenario. We locate the four known agentic failure factors in the concrete layers of a
DES pipeline, draw the boundary between what an agent may decide and what must remain
with the researcher, and close with the open problems: termination logic, statistical
quarantine criteria, audit-trail navigation at scale, and state pruning.

## Motivation

The engineering world reached this conclusion before the simulation world wrote it
down. Practitioners building agentic coding systems now state plainly that domain
knowledge which lives in people's heads must be converted into machine-checkable
infrastructure — lint rules, tests, review criteria, project-law files — because
agents operate at a pace where head-knowledge cannot gate quality (Cherny, 2026,
industry commentary; use as framing evidence of mainstream doctrine, not as an
academic citation). The claim of this paper is that simulation V&V must undergo the
same conversion, and that the DES community has not yet answered for its own methods.

When I was building my models in ScalaTion I could do manual tracing: I would open the
logs, watch the animation, and walk the time-ordered agenda entry by entry whenever
something looked off, because I was the one who set the inputs. That working style does
not survive an autonomous agent. The formal and empirical questions I asked left traces
in logs and metrics; the methodological questions — why we chose the Shifted Erlang tau
for the arrival process, why we moved from Gipps to IDM — often lived only in my head.
Once an agent generates scenarios at machine speed, that reasoning disappears unless the
architecture records it. The agentic-systems literature (AgentGuard, AIVV, DURA-CPS) has
started building assurance layers for LLM agents in general. The DES community, whose
entire discipline rests on V&V, has not yet answered for its own methods. This paper is
that answer, stated as a position and a research agenda.

## Methodology

This is a position paper. The method is argument by translation and by located failure:

1. Take the classical V&V canon (Sargent 2010/2013, Balci 1997/1998, Law) and state
   precisely which assumption each core practice makes about the pace and visibility of
   the modeling process.
2. Take the four agentic failure factors from AgentGuard and place each one in a
   specific layer of a DES experiment pipeline, with a concrete traffic-simulation
   failure example for each (wrong-context parameter retrieval, DSL translation error,
   invalid arrival pattern producing an artificial queue the feedback loop then rewards).
3. Restate the three validity types as machine-checkable evidence pillars and show
   where each pillar is produced in an agentic architecture.
4. Derive the open problems directly from what the translation cannot yet deliver —
   these are stated as research questions, not solved.

No empirical results are claimed. The companion implementation paper (Paper 2, Runtime
Invariant Monitor) carries the experimental burden.

## Implementation Diagram

```
            THE V&V TRANSFER PROBLEM

  Classical DES V&V                Agent-paced DES
  ------------------               -----------------------------
  researcher sets inputs           agent proposes scenario
  researcher traces agenda   -->   trace volume >> human reading speed
  review points, meetings          continuous revision loop
  reasoning in the head            reasoning must be RECORDED or lost

            THREE PILLARS, MACHINE-CHECKABLE

  +----------------+   +----------------+   +---------------------+
  | FORMAL         |   | EMPIRICAL      |   | METHODOLOGICAL      |
  | DSL contract   |   | fit vs PeMS    |   | retrieval trace     |
  | invariants on  |   | sensitivity    |   | hypothesis lineage  |
  | executed trace |   | sweeps, seeds  |   | parameter-change log|
  +-------+--------+   +-------+--------+   +----------+----------+
          |                    |                       |
          +--------------------+-----------------------+
                               |
                    +----------v-----------+
                    | HUMAN BOUNDARY:      |
                    | agent may reject /   |
                    | quarantine / revise; |
                    | only the researcher  |
                    | declares a discovery |
                    +----------------------+
```

## Target Venues

| Venue | Fit | Notes |
|---|---|---|
| **WSC 2027** (foundations / philosophy of simulation track) | Best fit | Position papers are at home here; deadline typically ~April 2027 |
| ANNSIM 2027 | Good | Society venue, committee-friendly |
| *SIMULATION* (SCS journal) | Good | If expanded with fuller literature treatment |
| Journal of Simulation (JOS) | Alternative | Slower, but archival |
| arXiv preprint first | Recommended | Stakes the framing claim early; verify venue preprint policy |
