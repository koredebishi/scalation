# PADS 2027 — Candidate Papers

**Working document. Session date: 2026-08-26.**
Purpose: three candidate ideas, what's verified, what still needs checking, and the
list of directions already ruled out so we don't re-tread them.

---

## Venue facts

| | |
|---|---|
| Conference | ACM SIGSIM-PADS 2027, June 9–11, Hyatt Regency Atlanta |
| Co-located | ACM FCRC 2027 |
| **First call** | **Oct 30, 2026** |
| **Second call** | **Jan 22, 2027** |
| Review model | Two-phase — first-call reviews come with improvement suggestions and feed a second-call resubmission |
| Journal | TOMACS special issue available |

**Regular tracks:** Simulation Algorithms and Infrastructure · Modeling Methodology · Applications

**Special tracks 2027:** AI and Simulation *(new)* · Model reduction *(new)* · Cybersecurity · Digital Twins · Simulation-based Optimization

**Dropped from 2026:** Visualization · Computational Demography · Computational Epidemiology · Automation in M&S

The two new tracks have **no published sub-bullets** — only names. Everything else in the
call has a detailed topic list. Track chairs not yet listed; worth watching before Oct 30.

**Applications track is deliberately excluded.** It is the only track where the Eaton model
would be the *subject*. Everywhere else it must be demoted to a workload. That is the
constraint we are choosing to pay.

---

## The advisor test

Every idea below has to answer three questions:

1. Is it a **method**, not a study or a piece of engineering?
2. Is it **novel** — has someone already done it?
3. **Who does it help** outside this repo?

The generalization filter: *if a fix only works for my Scala repo, it isn't a pain worth publishing.*

---

# Idea 1 — RIM (the method paper)

**Track:** Modeling Methodology → *Experiment design, V&V, calibration, UQ*
**Or:** AI and Simulation, if the LLM-judge arm is included (see below)
**Status:** strongest. Most of it is already specified in `02-runtime-invariant-monitor.md`.

### One sentence

> A three-way procedure for judging a simulation trace — physically invalid, statistically
> unstable, or genuinely emergent — with a stated decision rule at each boundary.

### Goal

Let the computer decide whether a strange result is a bug or a discovery.

### Motivation

Both sides of this have already happened here:

- The **same-tick arrival shockwave** looked like real traffic behaviour. It took weeks of
  log-reading to expose it as a bug in the arrival generator.
- The **lane-vs-corridor divergence** looked equally strange and turned out to be the
  finding of the ANNSIM paper.

Two odd-looking results, opposite verdicts, and the only thing that separated them was a
human reading logs for weeks. That doesn't scale.

The obvious fix makes it worse. A pass/fail checker throws away the second kind, because
real findings look strange too. A binary monitor would have deleted our own result.

### Research question

Can *physically impossible*, *random noise*, and *real but surprising* be separated
automatically — and what is the decision rule at each boundary?

### The contribution, stated so it reads as method

The five invariant checks are **not** the contribution. Every simulator has collision
detection. The contribution is that **checking and judging are different operations**, and
every existing gate conflates them. A pass/fail monitor doesn't merely miss findings — it
systematically deletes the surprising ones, which are the only ones worth having.

### Methodology

1. **Define the three verdicts in domain-free terms first**, then instantiate for traffic.
   This is what keeps it methodology rather than a traffic paper.
   - **Broken** — a physical or logical law was violated. Reject.
   - **Unstable** — laws hold, behaviour disappears under seed / time-step / boundary change. Park.
   - **Stable** — laws hold and behaviour survives all of it. Escalate to the human.

2. **Build three labelled buckets of traces.** This is the whole experiment.
   - **Known broken** — injected faults: same-tick arrivals, sub-length spacing,
     negative speed, non-adjacent lane jump. We know these are bad because we put them there.
   - **Known real** — textbook-documented behaviour: stop-and-go waves, capacity drop at a
     merge, bottleneck breakdown. Only use ones that can be cited.
   - **Known noise** — behaviour that appears in one seed and not the others.

3. **Run the two-way monitor first and show it fails.** Count how many *known real* traces
   pass/fail rejects. **That number is the paper.** If it's near zero there is no paper.

4. **Run the three-way monitor and score it** — catch rate and latency on known-broken,
   false-park rate on known-real, correct-park rate on known-noise.

5. **State the parking test as an actual rule.** n = 10 seeds (settled — see below), halve
   the time step, perturb boundary conditions.

6. **Report cost.** Wall clock with the monitor on vs off.

### Settled

**Replication count is 10.** Standard in traffic microsimulation. Extra replications only
shrink Monte Carlo noise, which is not the dominant error term — model and behavioural
uncertainty swamp it. Running 100 buys a tighter interval around a number that is already
wrong for other reasons.

Write this argument into the paper: PADS reviewers are simulation-methodology people, not
traffic people, and it needs one citation and one sentence.

It also *strengthens* the parking test — if a behaviour only appears after 100 seeds it
isn't a robust finding. Fixing n at 10 turns a convention into a design choice.

### Eaton's role

Use Eaton as the hard test case. Fire closure and contraflow produce genuinely strange
traces. US-101 mostly produces traffic. Eaton stresses "bug or discovery" harder.

### The track move

Add one arm where an LLM judges traces from output charts — AutoCred's method — and count
how many **known-real** phenomena it throws away.

That single arm moves this paper from Methodology into **AI and Simulation**, which is new
and thin, and it attacks a method PADS already published using an argument we own.

### Open risks

| risk | status |
|---|---|
| **Three-valued runtime verification already exists** (Bauer, Leucker, Schallhart: true/false/inconclusive). Their third value means "trace not long enough to decide." Ours means "physics fine, may be noise." Different — but a reviewer will ask. | **UNVERIFIED. Check first — one day.** |
| Merge gap-acceptance bug: vehicles are inserted with no gap check, so the baseline model produces broken traces by design. Either fix it first or use it openly as a known-broken case. | Decide before building — it changes what the baseline is. |
| "You built an assertion checker." | Answered by the framing above. Lead with the verdict logic, never with the five checks. |

---

# Idea 2 — Cross-formalism agreement (the AI paper)

**Track:** AI and Simulation, or Modeling Methodology → *Modeling formalisms and DSLs*
**Status:** promising, one verification outstanding.

### One sentence

> When a model is machine-generated and no reference exists, generate it twice in two
> different formalisms and use disagreement as the error signal.

### Goal

Check a machine-written simulation model when there is nothing to check it against.

### Motivation

Everyone building LLMs that write simulation models has an answer key. They generate the
"true" model first, then measure whether the LLM recovers it. That works for chemical
reaction networks because a database of them exists.

**There is no such database for traffic.** Or evacuation. Or most things anyone actually
wants to model. The entire field's evaluation method only works where someone already did
the work by hand.

### Research question

Does disagreement between two formalisms catch real errors in machine-generated models,
and how close does it get to having an answer key?

### The honest limit — state it this way or get killed in review

**Agreement proves nothing.** Two models from the same LLM can share a misunderstanding and
agree while both are wrong. The claim is one-directional:

> Disagreement reliably detects errors. Agreement does not certify correctness.

### The ρ formulation — this is what makes it a real contribution

Turn that limit into a quantity instead of a caveat.

Let `E_A` be the event that the model generated in formalism A is wrong, `E_B` likewise for
B. Define the **correlated-error probability**:

```
ρ = P(E_A ∩ E_B)
```

Disagreement is only a useful signal when ρ is small relative to `P(E_A ∪ E_B)`. If both
versions come from the same model with the same misunderstanding, ρ approaches
`P(E_A)` and the method detects nothing.

So the research question sharpens from "does this work" to:

> **When does formalism diversity reduce ρ enough to make disagreement informative?**

That is measurable, and it is the reason to compare *different worldviews* rather than two
samples from the same one. Process-interaction and event-scheduling force different
reasoning about the same system, so they should fail differently. Two `process` models from
two prompts should fail the same way.

**Baseline to beat:** same-formalism self-consistency — generate the `process` version
twice and compare. If cross-formalism doesn't beat that, formalism diversity buys nothing
and the paper has no claim.

### What to measure

| measure | what it tells you |
|---|---|
| error-detection recall | of the injected faults, how many does disagreement catch |
| **false-reassurance rate** | how often both versions are wrong *and* agree — this is ρ, measured |
| diversity benefit | cross-formalism recall minus same-formalism self-consistency recall |

### Which formalisms — verified against the repo

**The solid pair:**

| formalism | package | state |
|---|---|---|
| process-interaction | `simulation/process` | main engine, complete |
| event-scheduling | `simulation/event` | complete — `Model`, `Event`, `Entity`, `CausalLink`, `EventNode` |

These are the two classical DES worldviews. Equally expressive by textbook result, but the
code looks nothing alike — a vehicle in `process` is an actor that flows and yields; in
`event` it is a data record touched by scheduled routines. That difference is the point:
an LLM makes **different mistakes in each**. Correlated errors are what would ruin this,
and differing worldviews are the defence.

**Possible third:** `simulation/agent` — already has `Link`, `Junction`, `Gate`,
`EdgeAgents`, so it is traffic-shaped out of the box. But roughly half its files are
`.scalaa` (disabled). Needs work first.

**Do not use — this confound would be fatal:**

- `simulation/activity` is **Petri nets**, not activity-scanning.
- `simulation/queueingnet` (Jackson, M/M/c) and `simulation/state` (Markov chains) are
  **analytic, not simulation** — they cannot express car-following at all.

The comparison only works between formalisms of **equal expressive power**. Compare a
microscopic process model against a Jackson network and disagreement means nothing — the
gap is the abstraction level, not an error. A reviewer will find this.

Two formalisms is enough. "Does A agree with B" does not need a third.

### Testing it

Inject known errors into a generated model and measure whether cross-formalism
disagreement catches them. Same known-fault design as RIM — the two papers share machinery
without sharing a claim.

### Open risk

| risk | status |
|---|---|
| Has anyone done cross-*representation* consistency for generated **models** rather than generated text? Self-consistency decoding exists in LLM work; this is not that. | **UNVERIFIED. One search.** |

---

# Idea 3 — Structural sensitivity across layers (backup / third)

**Track:** Modeling Methodology
**Status:** good idea, needs the reframe, one prior-art check outstanding.

### Goal

Find out where model accuracy actually comes from, so people know where to spend their time.

### Motivation

A traffic model is a stack of choices. Every one is defensible. Nobody has time to get them
all right.

| layer | choices available in this repo |
|---|---|
| arrival process | Poisson, Erlang-2, shifted Erlang, NH-Poisson |
| car-following law | IDM, Gipps, Krauss |
| lane-change law | MOBIL, none |
| integrator | eight |
| time step | free |

Standard practice picks one from each row, tunes the parameters, reports the fit. **Nobody
asks which row deserves the effort.**

We have direct evidence the field is spending badly — see the ANNSIM result below.

### The reframe that makes it a method paper, not a study

As a study — "we ran 192 configs, here is a table" — this gets called cheap, correctly.

As a method it is this:

> Sensitivity analysis assumes the factors sit on a number line. Model structures are not
> numbers. There is no distribution over "car-following laws," so Sobol indices do not
> apply, and nobody has said what replaces them.

That gap is not traffic-specific — same problem in epidemiology (which contact model),
climate (which parameterization), systems biology (which kinetics).

### Research question

How do you measure the influence of a modeling choice when the choice is not a number?

And: does the answer change depending on what is being simulated?

### Why it cannot fail flat

Three possible outcomes, all publishable:

1. **Ranking is stable** → a guideline. Low ceiling, but real.
2. **Ranking is regime-dependent** → the good outcome. Effort allocation must follow the
   regime; there is no universal advice.
3. **The layers don't separate** — interactions swamp main effects → arguably the most
   interesting result, and it says the whole one-per-row practice is unsound.

### Prior evidence in hand — ANNSIM 2026

*"Beyond Corridor Averages: Lane-Level Validation of Microscopic Freeway Simulation with
Data-Driven Arrivals"* — ANNSIM 2026, May 4–7, Orlando. **Already published — cannot be
resubmitted.** Its findings are the motivation for this idea, not its content.

1. Integrator choice barely matters — eight solvers, ballistic through RK4, **under 1%**
   variation in fit. The paper itself notes this is consistent with prior comparative work.
2. Arrival process matters a lot — shifted Erlang-2 cuts flow error **~28%** vs Poisson by
   forcing a realistic minimum headway. Holds across all eight integrators.
3. Lane-level detail diverges from corridor aggregates — five mainline detectors, four-lane
   segment, two ramp stations.

*(Finding 3 is committed to RIM as the "escalate" case study. Do not double-dip.)*

### The version that isn't cheap

Eight integrators moved the answer 1%. One arrival change moved it 28%. **The car-following
law has not been tested.** If IDM vs Gipps vs Krauss also lands small next to the arrival
process, the claim becomes:

> Microscopic behavioural detail matters less than input timing.

That tells the field to stop polishing car-following models and fix their inputs. That has
consequences, and nobody calls it cheap.

Honest risk: car-following law probably *does* matter in stop-and-go. Which is exactly why
the question is regime-dependence rather than a single global ranking.

### The data-driven counter, and why it doesn't land

**The attack:** with real data you don't choose an arrival distribution, you replay the
data. SUMO has had this for years — DFROUTER (insertions from loop counts), routeSampler
(demand from turn counts), Calibrator (forces flow to match measurements).

**Edge 1 — the data is aggregated, so the choice moves rather than vanishing. Strongest.**
PeMS gives 120 vehicles in 5 minutes. It does *not* give the headway sequence inside that
interval. Poisson permits zero headway; shifted Erlang-2 forces a minimum. Same count,
different microscopic behaviour. **That is the 28%.** No amount of detector data resolves
it — detectors count, they don't record sequence. (Note: SUMO's docs cover the counts
thoroughly and are silent on within-interval headway.)

**Edge 2 — the Calibrator is evidence *for* the point.** Why does SUMO need a tool that
forces flow to match measurements? Because insertion drifts. DLR built an override because
the input process is load-bearing.

**Edge 3 — trajectory data is short and rare.** NGSIM is 45 minutes. Drone datasets cover a
few hundred metres. Nobody has a corridor for a full day.

**Edge 4 — what-if kills replay, and this is where Eaton returns.** Evacuation, closure,
contraflow: historical arrivals do not apply. A digital twin replays today; it cannot
replay a fire.

### Open risk

| risk | status |
|---|---|
| Punzo / Ciuffo / Montanino do variance-based SA **within** a layer (IDM's parameters). Barton / Nelson / Song work on **input model uncertainty** — finding 2 is an input-uncertainty result and that literature may already claim input models dominate. Bayesian model averaging and multi-model ensembles cover part of the "which model" question. | **UNVERIFIED. Check before pitching.** Novelty is the *cross-layer* comparison, not the finding. |
| Headway-distribution literature is old — Cowan's M3, Buckley, displaced exponentials. "Minimum headway matters" is not new alone. | Know the distinction before the advisor asks. |

---

# How the three fit together

Both papers 1 and 2 answer the same underlying question — **how do you tell a bad model
from a good one when you have no ground truth?**

- **RIM** answers it from **physics** — invariants that hold regardless of data.
- **Cross-formalism** answers it from **redundancy** — two independent expressions of the
  same system.
- **Structural sensitivity** is a different question: not *is this model right* but *which
  part of it decides whether it's right*.

That's a coherent story rather than three unrelated ideas, which is what an advisor is
listening for.

Eaton carries paper 1 (as the hard test case) and returns in paper 3 (Edge 4).

---

# Two further candidates, and where the math actually is

A separate five-idea ranking was brought in with formal statements attached. Three of the
five were the ideas above wearing notation; two were new. This section records the verdict
so the ground isn't re-covered.

## What the notation added

| idea | notation offered | verdict |
|---|---|---|
| Cross-Formalism | correlated-error probability ρ | **Genuine improvement — adopted above.** Turns the honest limit into a measurable quantity. |
| Artifact–Stability Triage | threshold rule on admissibility `I(s)` and robustness `R(φ) = P(φ persists under perturbation)` | **Precision, not novelty.** This is how to *state* RIM in the paper. It adds no contribution. |
| Cross-Layer Sensitivity | `Var(Y) = ΣV_j + ΣV_jk + …` | **Careful — this hides the problem.** See below. |

### Why the Sobol decomposition on idea 3 is a trap

Writing the functional ANOVA looks like it formalises the layer-ranking question. It
doesn't. To compute `V_j` you need a **probability measure over the layer choices**.

What is the measure over {IDM, Gipps, Krauss}? Put a uniform prior on it and the variance
shares depend entirely on which three car-following models happen to be implemented. Add a
fourth and the ranking moves. That is a property of the library, not of the world.

**Confronting that is the paper.** Decorating it with ANOVA notation assumes it away, and a
reviewer who knows sensitivity analysis will see it immediately.

## New candidate A — Adaptive Evidence Acquisition for V&V

**Problem:** deciding which V&V test to run next is done by habit and checklist.

**Formulation offered:** choose the next test by expected information gain per unit cost,

```
T* = argmax_T  [ H(H | E_t) − E[H(H | E_t, e_T)] ] / C(T)
```

subject to a correctness constraint,

```
min_π E[ Σ_t C(T_t) ]     s.t.  P( V̂_π ≠ V* ) ≤ δ
```

> *What is the cheapest sequence of V&V evidence that preserves the correctness of the
> final validation decision?*

**The problem is real and appears unoccupied** — earlier searching found nothing on
adaptive V&V test selection. Sargent's framework is a checklist, not a policy.

**But the math is not new:**

- Information gain per unit cost is **Bayesian experimental design** — Lindley, 1956.
- The constrained sequential form is **Chernoff's sequential design of experiments**, 1959.
  It is Wald's SPRT generalised to multiple tests.
- Cost-normalised acquisition is standard practice (expected improvement per second,
  Snoek et al. 2012).

So the contribution is *pointing sixty-year-old machinery at V&V*. That is the same shape
as "apply constrained Bayesian optimisation to calibration," which is on the ruled-out list.
Defensible, but not thrilling, and the advisor test will find it.

**Two blockers the formulation hides:**

1. **There is no evidence model.** Information gain needs `P(H | E)` — a posterior over "is
   my model valid" — plus a likelihood for each test outcome. What is the probability of a
   given KS result *given the model is valid*? Nobody has that. Without it the acquisition
   function is uncomputable. **Building that evidence model is the actual research.**
2. **`V*` requires ground truth on validity.** You cannot measure `P(V̂ ≠ V*)` without
   knowing the correct verdict. That means labelled cases — known-broken, known-noise,
   known-real.

**That second blocker is decisive for sequencing.** The labels it needs are exactly RIM's
three-bucket design.

> **This is RIM's sequel, not RIM's rival.** RIM defines the verdicts and the tests that
> separate them. Adaptive acquisition chooses which test to run next. Run them in that
> order and the second gets stronger, because the first hands it the ground truth.

## New candidate B — Invariant-Guided Active Debugging (minimal reproducer)

**Formulation offered:** first-failure time `t* = min{ t : I_k(s_t) = 0 }`, then

```
x* = argmin_x c(x)     s.t.  I_k(S(x)) = 0
```

— shrink the scenario while keeping the failure reproducible.

**This is delta debugging.** Zeller & Hildebrandt, 2002, *Simplifying and Isolating
Failure-Inducing Input* — the ddmin algorithm. It is also what property-based testing does
routinely when it shrinks a counterexample (QuickCheck, Hypothesis, C-Reduce).

The simulation-specific twist is that the predicate is a runtime invariant over a
trajectory rather than a crash. That is a variation on a 24-year-old technique, not a new
problem.

**Verdict: weakest of the five.** Keep it as a *feature* of RIM — when a trace is rejected,
shrink it before handing it back — not as a paper.

## Revised ranking after this comparison

| | idea | why |
|---|---|---|
| **1** | **RIM / triage** | buildable now, both case studies are ours, and it supplies the labels the others need |
| **2** | **Cross-formalism, with ρ** | the ρ formulation makes it sharper than it was |
| **3** | **Adaptive evidence acquisition** | good problem, old math, needs an evidence model nobody has — and needs RIM first |
| **4** | **Cross-layer sensitivity** | only if the measure problem is confronted rather than decorated |
| **5** | Minimal reproducer | delta debugging; fold into RIM as a feature |

The instinct behind the five-idea table is right: "LLM helps with V&V" is weak, and formal
statement sharpens it. It picked the wrong winner because the notation on candidate A looks
impressive and its 1959 provenance is not visible from the formula.

---

# Repo inventory — what actually exists

**Assets**

- **Eight simulation paradigms in one library** — `process`, `event`, `activity`,
  `agent`, `state`, `tableau`, `monte_carlo`, `queueingnet`. Most libraries have one.
  Underused; this is what makes idea 2 ours.
- **Deep optimization suite** — SPSA, SPSA_Mo, GA, DifferentialEvolution, NelderMead,
  TabuSearch, GridSearch, `MetaModelTR` (surrogate trust-region), `StoppingRule`,
  `BoundsConstraint`.
- `Dynamics.scala` — IDM, Gipps, Krauss, MOBIL. All layers swappable → idea 3 is cheap to run.
- Eight integrators, multiple arrival processes.
- Eaton dual corridor (I-210 WB + SR-134 WB), US-101 (CalRoute101_3).
- Sapelo2. Compute is not a constraint.

**Gaps**

- `Monitor.scala` is Miller's **trace logger**, not an invariant checker. RIM is new code —
  but `VTransport.move()` and `vList` are clean hook points.
- **No `RoadGraph`.** The composition/evolution story has no code behind it.
- `CalibrateCalRoute101.scala.bak` — calibration is switched off.
- **No parallel anything.** `Checkpoint.scalaa` disabled. Coroutine's virtual threads are
  for coroutines, not PDES. This closes the entire synchronization track.
- Runtime: current branch ~2h; an earlier branch ran ~15 min. **Run idea 3 on the 15-min
  branch.** Not a blocker, just a branch choice.

---

# Ruled out — do not re-tread

| direction | why it's dead |
|---|---|
| **Finetuning an LLM to write simulation models** | Six groups in sixteen months: Kreikemeyer et al. (Mar 2025, CRN → formal DSL), ChronoLLM (Aug 2025), EnvTrace (Nov 2025, trace-alignment eval), SimLLM (Jan 2026, SFT+DPO for SimPy DES), DEVS spec-driven gen+eval (Mar 2026), RLVP (Jul 2026). We would be seventh, latest, with least compute. |
| **RL with a physics-verifier reward** | RLVP, arXiv 2607.10474, July 2026. Hybrid verifier: hard program-validity checks + continuous physics rewards on PDE residual. Exactly the idea. |
| **LLM credibility scoring from output** | AutoCred. And our own counter-evidence — a model can pass every aggregate metric with physically absurd parameters. Keep the counter-argument, drop the method. |
| **Differentiable simulation** | Andelfinger's territory. Named twice in the call. He is on the steering and program committees. |
| **Model reduction** (micro→meso as formal reduction) | Feret sets the bar (PADS'25). LWR is a hydrodynamic limit, not a lumping of IDM — it isn't an equivalence relation. Not buildable to deadline. |
| **Eaton structural-mutation paper** (rules for a road changing mid-run) | **Falsified.** SUMO has documented semantics (wait, then teleport). Real contraflow is staged — crews clear the road first, so "40 cars on a reversing link" is an instantaneous-flip artefact of our model, not a real phenomenon. Ronchi + *The verification of wildland–urban interface fire evacuation models* (2023) own the no-data validation problem. And "optimizer exploits the simulator" is Karl Sims, 1994 — creatures harvested Euler drift and shrank until their feet passed through the floor for free energy. |
| **Model-families reframe of the ANNSIM result** | ANNSIM 2026 was May 4–7. Already published. Resubmitting the same result is self-plagiarism, not reframing. |
| Synchronization / scheduling / load balancing | No parallel engine. |
| Emerging hardware | Nothing to build on. |
| DDDAS / digital twins | Months of plumbing for a crowded track. |
| Cybersecurity | No fit. |

---

# Next actions

**Verification — one day each, do these before committing**

1. **Idea 1:** three-valued runtime verification (Bauer / Leucker / Schallhart). Write down
   why "unstable" ≠ "inconclusive."
2. **Idea 2:** has cross-representation consistency been done for generated *models*?
3. **Idea 3:** Barton / Nelson / Song on input model uncertainty; Punzo / Ciuffo on
   within-layer SA; Bayesian model averaging.

**Decisions outstanding**

- Merge gap-acceptance bug — fix first, or use as a known-broken case?
- Oct 30 or Jan 22 for idea 1?
- Which branch for idea 3 (the 15-minute one).

**Watch**

- PADS 2027 committee page — who chairs *AI and Simulation* tells us what the track means.

---

# Related documents

| file | holds |
|---|---|
| `02-runtime-invariant-monitor.md` | Idea 1's full outline, abstract, working tree, venue table |
| `paper2-rim/draft.md` | RIM draft prose |
| `03-goodhart-calibration.md` | SPSA basis withdrawn — the Goodhart argument needs a new demonstration |
| `04-hpdes-rollback-across-resolution.md` | Blocked on a parallel engine |
| `06-evacuation-scenario-matrix.md` | Eaton as Applications — TRB, not PADS |
| `README.md` | Earlier CFP walkthrough and the five superseded ideas |