Based on the **PADS 2023–2025 pattern** we harvested—not on AutoTrafficSim—the five ideas I would take seriously are these.

The common template is:

[
\boxed{\text{fixed simulation choice} \rightarrow \text{AI-controlled choice} \rightarrow \text{preserved property} \rightarrow \text{tradeoff}}
]

That matches what PADS has recently rewarded: multi-fidelity simulation, surrogate/PDES coupling, malleable microscopic simulation, learned calibration, formal model generation, etc. The 2025 best-paper shortlist itself contains multi-fidelity simulation, surrogate-assisted multi-agent learning, and LLM-to-formal-DSL model generation.

## 1. AI-Directed Adaptive Fidelity for Evacuation Simulation

This is my **#1 overall**.

### Fixed thing today

Every region in Eaton receives essentially the same microscopic treatment.

### Make it intelligent

Let an AI policy decide dynamically:

[
r_i(t)\in{\text{coarse},\text{microscopic}}
]

for road region/link (i).

The policy sees things like:

[
s_i(t)=
(q_i,k_i,\dot q_i,\text{queue growth},
\text{merge activity},\text{fire proximity})
]

and allocates expensive microscopic simulation only where interactions matter.

For example:

* uncongested I-210 → coarse;
* developing interchange queue → micro;
* ramp spillback emerges → promote region;
* congestion clears → demote.

### The actual research question

> **Can AI learn where microscopic fidelity is required while preserving evacuation decisions?**

Optimization:

[
\min_\pi C_{\text{simulation}}
]

subject to

[
D(Y_{\pi},Y_{\text{full-micro}})\le\epsilon.
]

But I would go beyond ordinary output error and test:

[
\operatorname{Decision}(Y_\pi)
==============================

\operatorname{Decision}(Y_{\text{micro}}).
]

So maybe individual trajectories differ slightly, but the conclusion about contraflow or evacuation exposure does not.

### Why this screams PADS

PADS 2025 had **MFNetSim: A Multi-Fidelity Network Simulation Framework**, **Directing PDES and Surrogate Models in Loosely Coupled Hybrid Simulations**, and **Autonomic Partition-Aware Malleable Microscopic Traffic Simulation**.

You would be combining those intellectual patterns but making **fidelity selection learned and state-dependent**.

### Formalism that falls out

This is where your V&V intuition becomes interesting:

[
\text{state fidelity}
\rightarrow
\text{trajectory fidelity}
\rightarrow
\text{outcome fidelity}
\rightarrow
\boxed{\text{decision fidelity}}
]

Potential title:

**Learning Where Fidelity Matters: Adaptive Microscopic Resolution for Emergency Traffic Simulation**

**Novelty upside:** very high
**PADS fit:** exceptional
**Engineering:** high

---

## 2. AI-Directed Rare-Event Simulation for Evacuation Failures

This is probably the **best balance of novelty and feasibility** with Eaton.

Ordinary Monte Carlo wastes runs.

Suppose 99% of plausible evacuation scenarios are boring, while what we care about is:

* gridlock;
* fire overtaking queued traffic;
* contraflow crossover failure;
* diversion creating a secondary bottleneck;
* ramp spillback blocking the mainline.

### Fixed thing today

The scenario distribution (p(x)) determines which cases we simulate.

### Make it intelligent

Learn a proposal distribution:

[
q_\theta(x)
]

that concentrates simulation on rare, dangerous outcomes:

[
J(S(x))>\gamma.
]

The AI learns:

> What kind of plausible evacuation conditions are most likely to create a critical failure?

This could use:

* reinforcement learning;
* cross-entropy method + learned proposal;
* normalizing flow;
* diffusion model;
* Bayesian adaptive importance sampling.

No LLM required.

### Crucial simulation problem

You can't simply generate adversarial nonsense.

You need:

[
x\in\mathcal X_{\text{plausible}}
]

and, if estimating probabilities, likelihood correction:

[
P_{p}(A)
========

E_{q}
\left[
\frac{p(x)}{q(x)}
\mathbf1{A}
\right].
]

Now it becomes proper **AI-assisted rare-event simulation**, not scenario generation.

### Eaton advantage

Your model already gives a meaningful interconnected I-210/SR-134 world, with empirical demand support and time-dependent routing through the freeway connector.

You can ask:

> How many simulation runs are needed to discover the first critical evacuation failure?

Compare:

random Monte Carlo
vs Latin hypercube
vs conventional importance sampling
vs Bayesian optimization
vs learned proposal policy.

### Tradeoff

[
\boxed{
\text{rare-event discovery efficiency}
\leftrightarrow
\text{distributional realism}
}
]

### Formalism that falls out

What does it mean for an AI-generated stress scenario to be:

**possible**, **plausible**, **likely**, or merely **adversarial**?

That is a legitimate simulation-methodology question.

Potential title:

**Learning to Find the Tail: AI-Guided Rare-Event Simulation for Wildfire Evacuation**

**Novelty upside:** high
**PADS fit:** very high
**Engineering:** moderate

---

## 3. Neural Speculative Execution for Microscopic Simulation

This one is more algorithmic and perhaps more interesting than it first sounds.

### Fixed thing today

Eaton calculates detailed vehicle dynamics every time.

Let the true transition be:

[
s_{t+1}=F(s_t).
]

### Introduce a learned cheap transition

[
\hat s_{t+1}=\hat F_\theta(s_t).
]

Then train a gating policy:

[
g_\phi(s_t)
\in
{\text{surrogate},\text{exact}}.
]

Examples:

free-flow link → surrogate
stable platoon → surrogate
merge → exact
lane change → exact
queue front → exact
closure interaction → exact.

So the neural model **speculates** about mundane dynamics.

The exact simulator handles difficult interactions.

### Why I like this for PADS

PADS already likes both **speculation** and **surrogate simulation**.

2024's best paper was **SpecSims**, a speculative tree-based simulation-cloning framework.

2025 included **Directing PDES and Surrogate Models in Loosely Coupled Hybrid Simulations**.

2023 even had **DeepSim: A Transformer Based Model for Fast Simulation and Exploring Computer System Design Space**.

The analogical leap is:

> **speculative neural execution for DES/microscopic traffic.**

### Key difficulty

When do you trust the surrogate?

Could use epistemic uncertainty:

[
u_\theta(s_t)
]

and exact execution whenever

[
u_\theta(s_t)>\tau.
]

But the important measure should not merely be prediction MAE.

Measure:

[
\text{speedup}
]

versus

[
\text{physical violations}
]

versus

[
\text{decision error}.
]

### Formalism

Define safe replacement of exact transitions.

Something like:

[
\hat F
\stackrel{\epsilon,I}{\approx}F
]

meaning the approximation is bounded by error (\epsilon) while maintaining invariant set (I).

Potential title:

**Neural Speculation for Microscopic Traffic Simulation: Learning When Exact Execution Is Necessary**

**Novelty upside:** very high
**PADS fit:** exceptional
**Engineering:** high

---

## 4. Event-Triggered AI Control of Evacuation Operations

This is the most application-facing of the five.

I would **not** do:

> PPO controls traffic lights.

Too ordinary.

Instead, make the interesting problem **when to intervene**.

### Actions

[
a_t\in
{
\text{wait},
\text{activate partial contraflow},
\text{extend contraflow},
\text{reroute},
\text{close route},
\text{release control}
}.
]

But actions have:

* setup delays;
* operational costs;
* reversal costs;
* safety constraints.

Then the agent solves:

[
\max_\pi E\left[
-\alpha E_{\text{vehicles}}
-\beta T_{\text{clearance}}
-\gamma C_{\text{switching}}
\right].
]

### The non-obvious research question

> **When should an evacuation simulator trigger an expensive structural intervention under evolving uncertainty?**

That's much better than static contraflow comparison.

The AI watches queues, demand, closures/fire evolution and makes event-triggered decisions.

### Where a new validation problem appears

This could create something genuinely interesting:

**policy-induced simulation distribution shift.**

You validate a model under historically observed traffic.

Then the learned controller generates states humans never historically produced:

[
d_{\pi}(s)\neq d_{\text{historical}}(s).
]

Your simulator is now being used deepest in precisely the regions where its empirical validation is weakest.

So:

> **Can a simulation validated under observational traffic be trusted under AI-induced counterfactual states?**

That's a serious methodological result, if handled carefully.

### Tradeoff

[
\boxed{
\text{intervention benefit}
\leftrightarrow
\text{control complexity / switching / validation support}
}
]

Potential title:

**Event-Triggered AI Control for Wildfire Evacuation: When Should Contraflow Be Activated?**

**Novelty upside:** moderate-high
**PADS fit:** strong
**Engineering:** moderate

---

## 5. AI Allocation of Simulation Budget

This sounds simpler, but I think it could become very elegant.

Simulation studies usually treat replication somewhat mechanically:

[
N=30
]

runs everywhere.

But that's wasteful.

Some scenarios clearly produce stable results after 3 runs.

Others lie near a decision boundary and need 100.

### Fixed thing today

A predetermined simulation budget.

### Let AI allocate it

At each step the policy chooses:

[
a_t\in
{
\text{new scenario},
\text{another seed},
\text{higher fidelity},
\text{smaller }\Delta t,
\text{stop}
}.
]

Given total budget (B):

[
\sum_t C(a_t)\le B.
]

Objective:

[
\min
P(\text{wrong scientific decision})
]

under that budget.

Example:

Scenario A:
after 4 replications, contraflow advantage is obviously huge → stop.

Scenario B:
effect is near zero → spend 30 more replications.

Scenario C:
variance is weird → use smaller timestep.

Scenario D:
uncertainty localizes at a merge → promote fidelity.

### Why this is interesting

This is essentially **test-time compute scaling translated into simulation**:

[
\text{LLM world: allocate reasoning tokens}
]

becomes

[
\boxed{\text{simulation world: allocate experiment compute}}
]

And unlike an LLM metaphor, it addresses a real simulation-cost problem.

### Formalism

Here we could define:

**simulation sufficiency**.

Given evidence (E_t), when is additional simulation unlikely to change the decision?

[
P(D_{t+B}\neq D_t\mid E_t)<\delta.
]

Then stop.

This may become a new formal stopping criterion for AI-directed simulation studies.

Potential title:

**Simulation-Time Scaling: Learning How to Spend an Experiment Budget**

**Novelty upside:** high
**PADS fit:** high
**Engineering:** low-moderate

---

# My actual ranking for your situation

Given that Eaton is already substantially implemented, I would separate **best paper idea** from **best feasible paper**.

| Rank  | Idea                                     |  PADS | Novelty | Build cost |   Eaton reuse |
| ----- | ---------------------------------------- | ----: | ------: | ---------: | ------------: |
| **1** | Adaptive fidelity / simulation attention | ★★★★★ |   ★★★★★ |       High |          High |
| **2** | AI rare-event evacuation simulation      | ★★★★★ |   ★★★★½ |     Medium | **Very high** |
| **3** | Neural speculative execution             | ★★★★★ |   ★★★★★ |       High |          High |
| **4** | Event-triggered evacuation control       |  ★★★★ |    ★★★½ |     Medium | **Very high** |
| **5** | Adaptive simulation-budget allocation    | ★★★★½ |   ★★★★½ | Low–Medium | **Very high** |

If the deadline is the practical constraint, I would explore **#2 and #5 first**.

If the goal is the strongest **PADS simulation-methodology contribution**, I would explore **#1 and #3 first**.

And if you want the idea that most naturally creates the **new V&V/formalism issue you were sensing**, I think **#1 is strongest**:

> once AI determines **which simulation fidelity is executed where and when**, classical model validity is no longer enough—we have to define what properties an adaptive execution policy must preserve.

That V&V question emerges from the new simulation mechanism instead of being pasted onto it.
================================================================================================================
Opus Ideation dump. 

PADS 2027 — June 9–11, Atlanta, co-located with ACM FCRC 2027. First call Oct 30, 2026 (~9 weeks out), second call Jan 22, 2027. Two-phase review: submit to the first call, get reviews with improvement suggestions, resubmit to the second. TOMACS special issue available.

Two structural facts before the walkthrough:

- 2027 changed its special tracks. Gone: Visualization, Computational Demography, Computational Epidemiology, and "Automation in M&S." New: AI and Simulation and Model reduction. Kept: Cybersecurity, Digital Twins, Simulation-based Optimization.
- The two new tracks have no sub-bullets published yet. Only names. Everything else in the call has a detailed topic list. That gap is the single most useful thing on the page.

---
Regular Track 1 — Simulation Algorithms and Infrastructure

┌──────────────────────────────────┬─────────────────────────────────────────────────────────┐
│            Sub-bullet            │                       Do you fit?                       │
├──────────────────────────────────┼─────────────────────────────────────────────────────────┤
│ Synchronization, scheduling,     │ No, not today. This is PADS's crown jewel and it means  │
│ memory management, load          │ parallel/distributed. ScalaTion is sequential. Paper 4  │
│ balancing, runtime environments, │ aims here but is design-space only, no implementation.  │
│  fault tolerance                 │ Entering costs you a parallel engine.                   │
├──────────────────────────────────┼─────────────────────────────────────────────────────────┤
│ Simulation on emerging hardware  │ No. No GPU/FPGA work. Sapelo2 access isn't the same     │
│ platforms                        │ thing.                                                  │
├──────────────────────────────────┼─────────────────────────────────────────────────────────┤
│ Data-driven simulation systems:  │ Yes, and you're not using it. You have PeMS detector    │
│ Digital Twins, DDDAS, symbiotic  │ data, a calibrated corridor, and a decision context.    │
│ simulation                       │ Precedent: CQSim+ (PADS'25).                            │
├──────────────────────────────────┼─────────────────────────────────────────────────────────┤
│ Simulation engines and           │                                                         │
│ techniques for differentiable    │ Occupied. See below.                                    │
│ simulation and RL                │                                                         │
├──────────────────────────────────┼─────────────────────────────────────────────────────────┤
│ Performance evaluation of        │ Yes, cheaply. Your 15–20 min → 2h regression is a real  │
│ simulation systems               │ measurement artifact. Short-paper material at best.     │
└──────────────────────────────────┴─────────────────────────────────────────────────────────┘

On differentiable simulation — I checked before recommending it, because the call names it twice. It is Philipp Andelfinger's territory: Differentiable Agent-Based Simulation (PADS'21), Towards Differentiable Agent-Based Simulation (TOMACS 2022), Slight Stochastic Shifts Suffice (PADS'25). He is on the PADS steering committee and program committee. And Differentiable Hybrid Traffic Simulation (arXiv 2210.08046) already solved the discrete-continuous conversion for lane changes — the exact thing I was going to tell you was open. Do not walk in there. You would be competing with your own referee on his home ground.

AI fit for this track: real, but only in the DDDAS/symbiotic bullet — state estimation and data assimilation from partial sensors. The rest is either closed to you or closed by Andelfinger.

---
Regular Track 2 — Modeling Methodology

Sub-bullet: Modeling formalisms and DSLs
Do you fit?: Partly. Your scenario DSL. Precedent is Kreikemeyer/Uhrmacher's LLM→formal DSL
(PADS'25 best-paper shortlist) — which means the program chair's own group is here.
────────────────────────────────────────
Sub-bullet: Model-based approaches for reuse, composition, evolution
Do you fit?: Yes. Your current branch is literally trafficmodelabstraction. RoadGraph is a
composition story. Under-argued so far.
────────────────────────────────────────
Sub-bullet: Experiment design, V&V, calibration, UQ, simulation-based inference, conceptual
modeling
Do you fit?: This is your home. Papers 1, 2, 3, 7 all live here. Note "simulation-based inference"
— that's neural posterior estimation, and it's a more respectable AI framing for your
calibration work than "LLM guardrails."
────────────────────────────────────────
Sub-bullet: Model families and ensembles
Do you fit?: Yes, and nobody noticed. Your ANNSIM structural-sensitivity paper is a model-families
paper — IDM/Gipps/Krauss as a family, integrators as a family. You already have the result and
filed it under the wrong heading.
────────────────────────────────────────
Sub-bullet: Explorative modeling
Do you fit?: Yes. Paper 6's scenario matrix.

AI fit for this track: strong and legitimate. But be aware your V&V-of-agentic-AI angle competes directly with Uhrmacher's Rostock group, who are prolific here and set the taste.

---
Regular Track 3 — Applications

Includes "Emergency response and crisis preparedness" and "Transportation and urban planning" — a perfect fit for Eaton, which is exactly why you ruled it out. Noting the cost: this is the only track where your Eaton model is the subject rather than the workload. Everywhere else it has to be demoted to a benchmark. That's a constraint on every idea we generate, and it's the right constraint — just be deliberate about paying it.

---
Special Track — AI and Simulation (new, undefined)

No published sub-bullets. That cuts both ways.

Upside: a new track needs submissions, and early papers define its scope. Lower competition than the regular tracks in year one.

Downside: no stated scope means reviewers apply their own priors, and the PADS prior is hard — AI must serve simulation principles, not be an application of simulation. Every idea in your README fails that test, and so did both of mine.

Action item: the track chair isn't listed yet. When the committee page fills in, who chairs this track tells you what it means. Worth watching before the Oct 30 deadline.

---
Special Track — Model reduction (new)

The most interesting line in the call for you, and you have never looked at it.

What it means at PADS: Feret's Model Reduction of Infinite Rule-Based Models (PADS'25) is the exemplar — formal, exact reductions, symmetry and bisimulation arguments, proofs. That's the intellectual standard.

Your fit: micro→meso→macro is the traffic model reduction problem, and you already own the literature through paper 4 (Burghout's consistency requirements, virtual links). Reduction asks a sharper question than your paper 4 does, though: not "how do I synchronize the two resolutions" but "when is the reduced model provably a faithful stand-in?"

AI fit here is the least crowded thing in the call. The formal object is an approximate bisimulation with an error bound. Learning bisimulation metrics is an established technique in RL (Ferns et al.) that has never been pointed at DES model reduction. That gives you a learned relation with a bound, not a learned policy — which survives the PADS prior in a way that adaptive-fidelity and learned-scheduling do not.

I'm flagging it as the direction with the best shape. I am not pitching it yet — I've been wrong twice and it needs a real lit check first.

---
Special Tracks — Digital Twins / Simulation-based Optimization / Cybersecurity

- Digital Twins: needs live telemetry and a real physical counterpart. You could get there with a live PeMS feed, but it's a lot of plumbing for a track that's crowded.
- Simulation-based Optimization: bullets are compute budget allocation, differentiable simulation and gradient estimation, novel calibration, Bayesian optimization. Your README idea #5 lives here. Note differentiable simulation appears here again — same Andelfinger problem.
- Cybersecurity: no fit. Skip.

---
Answering your step 4 — where AI doesn't fit, what else

Three non-AI routes are open to you:

1. Model families and ensembles (Track 2). You have the result already. It needs reframing, not new work.
2. Model reduction (new special track). Formal, provable, no AI required — AI optional as a second contribution.
3. Performance evaluation (Track 1). Cheap, honest, low ceiling.

And the strategic move regardless of topic: submit something to Oct 30. The two-phase review means a first-call submission buys you PADS referee feedback and a second shot on Jan 22. Nine weeks is tight for new work but not for reframing work you already have.

---
Two questions before I go deeper:

1. Which sections do you want drilled — my read is Model reduction and Model families/ensembles, with DDDAS/symbiotic as third.
2. Are you aiming at Oct 30 or Jan 22? It changes whether we're reframing existing results or building something.

Sources: PADS 2027 · PADS 2027 CFP · PADS 2026 CFP (full topic list)

✻ Churned for 3m 31s
new task? /clear to
