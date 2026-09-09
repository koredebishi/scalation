# Final Research Blueprint

## Automatic Micro-to-Meso Abstraction and Single-Agenda Mixed-Resolution Traffic Simulation in Scalation

### Status

This document supersedes the earlier two-paper blueprint.

The earlier plan correctly separated model reduction from coupling, but it inherited an incorrect assumption that the existing microscopic Scalation simulator was a conventional discrete-time simulator that would need to be synchronized with a separate discrete-event mesoscopic engine. The source code shows otherwise.

The project should therefore be simplified around **one Scalation runtime, one event agenda, one vehicle identity, and two traffic resolutions**.

The final project has three contributions:

\[
\boxed{\text{C1. Generate the mesoscopic abstraction}}
\]

\[
\boxed{\text{C2. Execute micro and meso on one Scalation agenda}}
\]

\[
\boxed{\text{C3. Formally check that the generated semantics are the semantics being executed}}
\]

Everything else is evaluation or future work.

---

# 1. Research starting point

We already have an executable microscopic traffic simulator in Scalation.

The original research question was:

> Can this existing microscopic traffic model be automatically transformed into a computationally cheaper mesoscopic representation, while retaining enough agreement with the microscopic reference to be useful?

The long-term motivation is city-scale simulation:

```text
               LARGE TRAFFIC NETWORK

       MESO     MESO     MESO     MESO

                   +--------+
                   | MICRO  |
                   | region |
                   +--------+

       MESO     MESO     MESO     MESO
```

Most of the network can eventually operate at a cheaper mesoscopic resolution while selected regions retain microscopic detail.

But **Los Angeles is not the first experiment**.

The first scientific question is much simpler:

\[
\boxed{
M_{\mu}
\approx
M_m
\quad ?
}
\]

If the mesoscopic representation does not reproduce useful traffic behavior from the microscopic reference, the rest of the project has no value.

---

# 2. Correction to the earlier architecture: the micro model is already process-interaction DES

The previous report describes the microscopic model as discrete-time and the mesoscopic model as discrete-event.

That is not the correct characterization of the current Scalation implementation.

The existing traffic example explicitly identifies itself as a **Process-Interaction Simulation**.

A `Vehicle` extends `SimActor`, and `SimActor` is ordered according to activation time on the director's time-based agenda. Scheduling sets a future activation time and places the actor on that agenda.

The process-model `Model.reschedule` places the actor back onto the agenda.

Inside `VTransport`, microscopic motion is implemented by repeatedly:

1. evaluating the microscopic dynamics,
2. scheduling the vehicle after the reaction/update interval,
3. yielding control to the director.

The inspected `Vehicle` implementation currently defines a default reaction/update interval of 0.5 s.

Therefore:

\[
\boxed{
M_\mu
=
\text{process-interaction DES with frequent microscopic updates}
}
\]

The mesoscopic model should instead be:

\[
\boxed{
M_m
=
\text{process-interaction DES with coarse link/node events}
}
\]

This eliminates the need for two independent simulation clocks.

---

# 3. Final runtime architecture

Both resolutions should execute under the existing Scalation director.

```text
                      SCALATION MODEL
                            |
                     Global event agenda
                            |
                     Simulation clock
                            |
                        Vehicle actor
                            |
              +-------------+-------------+
              |                           |
              v                           v
         VTransport                  MesoTransport
          MICRO                         MESO
              |                           |
       Gipps / IDM etc.            Link/queue logic
       position updates            ready/release events
       lane interaction            capacity/storage
       activation every τ          sparse activations
              |                           |
              +-------------+-------------+
                            |
                       same Vehicle
                       same ID
                       same route
                       same clock
```

This is the central architectural simplification.

A vehicle does not transfer between two simulators.

It changes **movement semantics**.

For a microscopic link:

\[
Vehicle_i
\xrightarrow{VTransport}
Vehicle_i
\]

with frequent activation.

For a mesoscopic link:

\[
Vehicle_i
\xrightarrow{MesoTransport}
Vehicle_i
\]

with much fewer activations.

The same actor continues through the network.

---

# 4. What the mesoscopic model actually is

We should freeze this now.

The target is:

\[
\boxed{
\text{vehicle-based, event-driven, link/queue mesoscopic traffic}
}
\]

This is preferable to an aggregate CTM/LWR representation for this project because individual vehicle identity is retained.

The vehicle continues to carry:

\[
(id,\ route,\ vehicleType,\ldots)
\]

but the model stops maintaining continuous microscopic state such as:

\[
(x,\ v,\ a,\ leader,\ lane\ interaction)
\]

while the vehicle is inside a mesoscopic link.

The basic mesoscopic link \(e\) contains:

\[
L_e
\]

length,

\[
v_e^f
\]

free-flow speed,

\[
C_e
\]

discharge capacity,

\[
K_e
\]

storage capacity,

and a queue.

Aimsun's mesoscopic simulator similarly remains vehicle-based and event-based, with downstream congestion capable of affecting vehicle section-exit timing.

This is conceptually much closer to what we need than a cell-density representation.

---

# 5. Minimal mesoscopic semantics

Do not begin with a complicated traffic model.

For vehicle \(i\) entering link \(e\) at:

\[
t_i^{in},
\]

define a free-flow traversal time:

\[
\tau_e^0
=
\frac{L_e}{v_e^f}.
\]

Then define an **earliest-ready time**:

\[
t_i^{ready}
=
t_i^{in}
+
\tau_e^0.
\]

The important word is **ready**, not exit.

At \(t_i^{ready}\), the vehicle becomes eligible to leave the link.

Actual release depends on:

- discharge capacity,
- FIFO ordering,
- downstream storage,
- node/merge priority,
- boundary insertion feasibility.

This distinction solves an important event-semantics problem discussed below.

The initial meso implementation therefore needs only:

```text
MesoLink
MesoNode
MesoQueue

Enter
Ready
Release
Block
SpaceAvailable
```

plus:

```text
TravelTime
Capacity
Storage
Priority
RouteChoice
```

No signalized arterial is required initially.

The existing freeway/ramp model is enough.

---

# 6. Residual event-semantics problem

Using one agenda removes **clock synchronization**, but it does not remove every event-ordering problem.

The colleague is correct on this point.

Suppose at time \(t\) we calculate:

\[
t_i^{ready}=t+17.34.
\]

Between \(t\) and \(t+17.34\), the downstream link might become full.

If `17.34` were treated as an irrevocable **exit time**, the event would now be invalid.

Aimsun's mesoscopic node model explicitly updates affected exit times when downstream congestion changes, illustrating why downstream state cannot simply be ignored after scheduling.

Our first implementation should use a simpler discipline.

## Ready-event discipline

Schedule only the vehicle's **earliest physical readiness**:

\[
t_i^{ready}.
\]

At that time:

```text
vehicle becomes READY
        |
        v
check node/downstream
        |
   +----+----+
   |         |
   v         v
RELEASE    BLOCK
```

If downstream capacity exists:

\[
Ready(i,e)
\rightarrow
Release(i,e,e').
\]

If downstream is full:

\[
Ready(i,e)
\rightarrow
Blocked(i,e).
\]

The blocked vehicle leaves the active agenda and remains in the exit queue.

When downstream space is released, that downstream event wakes the upstream queue head.

This is consistent with Scalation's general process-interaction pattern: queued entities can be explicitly rescheduled when the relevant condition changes.

This avoids fixed-step polling and avoids having to cancel a precommitted false exit.

---

# 7. Same-timestamp event ordering

One agenda also requires deterministic semantics for equal timestamps.

We should define a total ordering as part of the specification.

For example:

\[
(eventTime,\ eventPhase,\ movementPriority,\ sequenceNo).
\]

Initial event phases should be:

1. **SpaceRelease** — downstream occupancy decreases.
2. **Release/Transfer** — waiting vehicles may use newly available space.
3. **Ready/Admission** — new vehicles become eligible.
4. deterministic sequence number as final tie-break.

For merges, movement priority is explicit.

For equal-priority movements, use deterministic FIFO/sequence order.

The precise ordering can change during design if traffic semantics demand it, but **once frozen it must live in MesoSpec and both backends must implement the same rule**.

There must not be one tie-breaking rule in Maude and another implicit rule in Scala's priority queue.

---

# 8. Boundary transfer is \(\beta\), not a global \(\gamma\)

The previous report models meso→micro transfer as:

\[
\gamma:S_m\rightarrow\mathcal P(S_\mu).
\]

That is appropriate for general aggregate-state reconstruction.

It is unnecessarily broad for our vehicle-based architecture.

Vehicle identity is never destroyed.

For a fixed meso→micro spatial boundary, define instead:

\[
\beta_{m\rightarrow\mu}
(v,\ boundary,\ S_\mu^{down}).
\]

Its job is to determine:

\[
(lane,\ insertionTime,\ insertionSpeed).
\]

The boundary loader asks:

1. Is there physical space?
2. Which downstream lane is route-compatible?
3. What is the earliest safe insertion time?
4. What initial speed is feasible relative to the microscopic leader?

Aimsun follows the same general strategy: it checks whether space exists and then applies microscopic car-following logic to calculate the earliest admissible arrival time.

If no insertion is currently possible:

\[
vehicle\rightarrow BoundaryQueue.
\]

When microscopic space becomes available, the head vehicle is awakened.

No polling loop.

Micro→meso is simpler:

\[
\beta_{\mu\rightarrow m}
\]

preserves:

- vehicle ID,
- route,
- type,
- current simulation time,

and admits the vehicle into the mesoscopic link.

Aimsun likewise uses the time a vehicle exits the microscopic region as its mesoscopic entrance time.

---

# 9. The common intermediate representations

There should be exactly two small semantic representations.

## 9.1 MicroIR

`MicroIR` describes what matters about the existing microscopic model.

```text
Network
    Segment
    Lane
    Node
    Connectivity
    Ramp

Demand
    Source
    ArrivalProcess
    Route

VehicleDynamics
    Gipps / IDM / ...
    parameters
    update interval

LaneBehavior
    lane change
    merge behavior

Observables
    flow
    speed
    density
    travel time
```

The source already exposes these concepts through classes such as `Vehicle`, `VTransport`, `Route`, `Junction`, `Ramp`, `VSource`, and the dynamics implementations.

For Paper 1 we do **not** claim that arbitrary Scala source can automatically be understood.

MicroIR may initially be produced through:

- static extraction where straightforward,
- Scalation API inspection,
- limited annotations.

That keeps the research focused on model reduction.

---

## 9.2 MesoSpec

The LLM does not generate arbitrary Scala source.

It generates:

\[
MesoSpec.
\]

Example conceptual structure:

```text
Link L1
    source = MicroSegment1
    travelTime = ...
    capacity = ...
    storage = ...

Node N1
    inputs = [L1, Ramp1]
    output = L2
    priority = ...

Movement
    routeCondition = ...
```

MesoSpec is the **single source of truth**.

From it:

\[
MesoSpec
\rightarrow
ScalaTion
\]

and

\[
MesoSpec
\rightarrow
Maude.
\]

---

# 10. Target vocabulary and structural synthesis

The earlier vocabulary contained named primitives such as `Merge` and `Diverge`.

The colleague correctly notes that if `Merge` itself is already a named primitive, asking the LLM to reproduce a ramp merge mostly tests wiring rather than meaningful composition.

Therefore the v1 target vocabulary should be more generic:

```text
Link
Node
Movement
Queue
TravelTime
Capacity
Storage
Priority
RouteChoice
Enter
Ready
Release
Block
```

Do **not** include:

```text
Merge
Diverge
```

as special semantic operators.

A merge is represented by:

\[
\text{multiple incoming movements}
+
\text{shared downstream storage}
+
\text{priority}.
\]

A diverge is:

\[
\text{route-conditioned outgoing movement selection}.
\]

Now a ramp topology actually requires composition.

We should call the held-out test:

\[
\boxed{\text{structural composition test}}
\]

not “mechanism holdout.”

That is the accurate claim.

---

# 11. Role of the LLM

The LLM receives:

\[
MicroIR
\]

and the target MesoSpec grammar.

It proposes:

\[
MicroIR
\xrightarrow{LLM}
MesoSpec.
\]

The LLM determines things such as:

- which microscopic segments should collapse into mesoscopic links;
- which microscopic behaviors become link-level timing/capacity;
- how ramp connectivity becomes node/movement structure;
- which target primitives must be composed;
- which source parameters or observables determine target parameters.

The LLM does **not**:

- calculate simulation time,
- control the agenda,
- numerically execute the model,
- decide whether its own output is correct.

The target vocabulary constrains the problem without reducing it to a hard-coded mapping such as:

```text
Gipps -> Rule 7
MOBIL -> Rule 4
Ramp -> Rule 9
```

That would be rule selection rather than synthesis.

---

# 12. Strong non-LLM baseline

We need one serious baseline.

Use:

\[
\boxed{\text{Trace-Fitted Deterministic Meso}}
\]

Its structure is constructed conventionally from the network topology.

Its parameters can use full access to **training** microscopic traces:

- per-link travel-time distributions,
- discharge rates,
- capacities,
- storage/jam estimates.

The baseline can optimize its numeric parameters using a conventional optimizer if necessary.

It gets the same design/training information available to the proposed method.

It does not see held-out test scenarios.

This baseline should be strong enough that we would genuinely use it ourselves if the LLM approach did not exist.

If it beats the LLM, report that.

---

# 13. Gate 3: the LLM must earn its place

This is one of the most important falsifiability conditions.

Suppose:

\[
MesoSpec_{\text{deterministic}}
\]

can be derived from MicroIR with simple conventional mappings and performs as well as:

\[
MesoSpec_{\text{LLM}}.
\]

Then the honest result is:

> An LLM is unnecessary for this transformation class.

Do not force the AI story.

Possible value the LLM may provide is:

- structural composition,
- reduced human effort,
- adaptation when source mechanisms differ,
- semantic mapping not captured by a fixed translator.

But this must come from results.

For that reason, begin logging from the **first project commit**:

- human hours,
- number of manual corrections to generated MesoSpec,
- number of generation iterations,
- number of hand edits required to obtain executable models.

Effort remains a secondary result, not the primary scientific claim, but it cannot be reconstructed later.

---

# 14. Role of Maude

Maude should not be used as a glorified schema checker.

Basic checks such as:

- required property exists,
- capacity \(>0\),
- IDs are valid,
- route reference exists,

belong in Scala/schema validation.

Maude is useful because rewrite theories and model checking let us inspect possible event-state evolutions.

Its role here is:

\[
\boxed{
\text{behavioral checking of generated event semantics}
}
\]

Candidate properties include:

### P1 — Time monotonicity

\[
t_{next}\ge t_{current}.
\]

### P2 — Causal order

\[
Enter(i,e)
\prec
Ready(i,e)
\prec
Release(i,e).
\]

### P3 — Storage safety

\[
0\le occupancy(e)\le K_e.
\]

### P4 — Mode exclusivity

\[
\neg(Micro(i)\land Meso(i)).
\]

### P5 — No zero-time transition cycle

There must not be an infinite rewrite sequence at one simulation time.

### P6 — Bounded deadlock freedom

On small admissible networks, the event semantics should not reach an unintended state in which nothing can progress.

More ambitious liveness properties can be added only if necessary; fairness-sensitive liveness requires additional assumptions.

We do not need that complexity to start.

---

# 15. Translator-agreement test — mandatory

This is an important addition from the colleague and should be treated as **load-bearing**.

MesoSpec has two translators:

\[
T_S:MesoSpec\rightarrow ScalaTion
\]

and

\[
T_M:MesoSpec\rightarrow Maude.
\]

If:

\[
T_S
\]

and

\[
T_M
\]

implement different semantics, Maude can verify the wrong model while the Scala simulation silently runs another one.

Therefore before using any Maude result we require a **translator conformance test**.

## Small deterministic instances

Use examples such as:

```text
1 link, 2 vehicles
2 links, 3 vehicles
merge, 3–5 vehicles
storage-blocked downstream link
micro -> meso -> micro
```

Remove uncontrolled randomness.

If randomness is needed, use an explicit fixed random-input tape consumed by both implementations.

Define a canonical projected trace:

\[
\pi(\tau)=
[
(t,\ event,\ vehicle,\ from,\ to,\ occupancy,\ mode)
].
\]

Then require:

\[
\boxed{
\pi(
Trace_{Maude}(MesoSpec)
)
=
\pi(
Trace_{Scalation}(MesoSpec)
)
}
\]

on the bounded conformance suite.

Implementation-specific bookkeeping is removed by \(\pi\).

Semantic events must agree.

If translator agreement fails:

\[
\boxed{\text{no verification claim is permitted}}
\]

until it is fixed.

---

# 16. Mutation study — mandatory

The property list alone does not establish that Maude contributes anything.

We therefore keep the mutation study.

The mutation catalog is frozen **before the checker is tuned against it**.

Initial mutations:

### M1 — Past event

Generate:

\[
t_{next}<t_{current}.
\]

Expected: violate time monotonicity.

### M2 — Causal inversion

Generate:

\[
Release(i,e)
\]

before `Enter`/`Ready`.

Expected: violate event causal order.

### M3 — Storage overflow

Allow admission when:

\[
occupancy(e)=K_e.
\]

Expected: violate storage invariant.

### M4 — Zero-time cycle

Generate transitions such as:

\[
A@t\rightarrow B@t\rightarrow A@t.
\]

Expected: reveal non-progress/zero-time cycle.

### M5 — Lost wake-up

A blocked vehicle is never reactivated after downstream capacity becomes available.

Expected: reach bounded deadlock/progress failure.

### M6 — Merge-order violation

At equal timestamps, a lower-priority movement releases before the specified higher-priority movement.

Expected: violate the declared node-order semantics.

Run:

\[
\text{Maude checker}
\]

against the mutants.

Also run randomized Scalation executions with runtime assertions.

Report:

- mutation detected by Maude;
- mutation detected by randomized runtime testing;
- counterexample length;
- verification/testing cost.

The purpose is not to make Maude look good.

The purpose is to determine whether exhaustive bounded semantic analysis catches failures that ordinary execution can miss.

---

# 17. Evaluation protocol

Keep this simple.

Use three scenario sets:

\[
X_{design},
\qquad
X_{dev},
\qquad
X_{test}.
\]

## Design

Used to establish:

- meso semantics,
- IR,
- vocabulary,
- compiler architecture.

## Development

Used while debugging:

- LLM prompts,
- MesoSpec generation,
- Maude checks,
- parameter procedures.

## Test

Sealed until the pipeline is frozen.

No method is changed after test results are inspected.

---

# 18. Freeze the metrics before the first final evaluation

We do not need TOST or arbitrary “equivalence margins” for the first paper.

We should not yet use the word **equivalent**.

The claim is:

> The generated mesoscopic model approximates the microscopic reference.

But we should freeze the metric set and identify the primary metrics before the final evaluation.

The existing Scalation reporting infrastructure already measures flow and speed and includes flow/speed sMAPE and RMSE reporting.

Therefore:

## Primary fidelity metric

\[
\boxed{
\text{mean detector-level flow sMAPE}
}
\]

across the evaluated detector/time observations.

This maintains continuity with the current traffic evaluation pipeline.

## Primary computational metric

\[
\boxed{
R_E=
\frac{N_{events}^{micro}}
     {N_{events}^{meso/hybrid}}
}
\]

or the corresponding event reduction percentage.

Event count is more fundamental than wall-clock runtime because it directly measures what the abstraction removes and is less dependent on machine/compiler noise.

## Required secondary fidelity metrics

- speed sMAPE;
- corridor travel-time error;
- maximum queue-length error;
- congestion onset/dissipation difference.

## Secondary computational metrics

- wall-clock runtime;
- memory;
- events per simulated vehicle;
- synchronization/boundary overhead, where applicable.

A model cannot be described as useful merely because flow sMAPE is small if, for example, it eliminates a congestion episode present in the microscopic reference.

The secondary traffic measures act as physical guardrails.

---

# 19. Structural composition test

Do not build a new signalized arterial just to manufacture a difficult holdout.

Use the existing network mechanisms.

Develop examples around simpler mainline structures.

Then hold out a topology requiring new composition, such as the ramp/node interaction.

Because `Merge` is **not** a primitive in the final vocabulary, the generator must construct the behavior from:

\[
Link
+
Movement
+
Queue
+
Priority
+
Storage.
\]

Call this:

\[
\boxed{\text{structural composition}}
\]

not mechanism holdout.

The question is:

> Can the generator compose the available semantic primitives into a network structure it was not explicitly shown as an example?

That is sufficient for Paper 1.

---

# 20. Hybrid/mixed-resolution experiment

Keep the useful part of the earlier sandwich experiment, but reinterpret it under the one-agenda architecture.

Reference:

\[
\mu_A
\rightarrow
\mu_B
\rightarrow
\mu_C.
\]

Mixed resolution:

\[
\mu_A
\rightarrow
m_B
\rightarrow
\mu_C.
\]

Compare downstream \(C\).

The old report correctly recognized that standalone aggregate agreement does not automatically imply valid boundary behavior.

## Boundary-only control

Use an effectively zero-length/zero-dynamics meso transfer:

\[
\mu
\rightarrow
m
\rightarrow
\mu.
\]

That isolates the loading/unloading effect.

Then use a genuine mesoscopic region.

Measure at the boundary:

- insertion speed;
- insertion headway;
- lane choice;
- blocked insertion duration.

Measure downstream:

- flow;
- speed;
- travel time;
- queue behavior.

Use repeated stochastic runs.

No single-trajectory comparison.

---

# 21. Performance experiment

The fundamental computational hypothesis is:

\[
\boxed{
\text{meso removes microscopic activations}
}
\]

so measure it directly.

Run several resolution fractions:

\[
r_m
\in
\{
0,\ 0.25,\ 0.50,\ 0.75,\ 1.0
\}.
\]

For each:

\[
N_{events}(r_m),
\]

\[
T(r_m),
\]

and traffic error:

\[
D(r_m).
\]

The ideal result is a curve:

\[
\boxed{
\text{more meso}
\rightarrow
\text{fewer events}
\rightarrow
\text{lower runtime}
\rightarrow
\text{some fidelity loss}
}
\]

This is the scalability story.

Not LA yet.

---

# 22. Final three contributions

The paper should contain **three contributions, not six**.

## C1 — Automatic mesoscopic abstraction

A semantic pipeline that takes an existing Scalation microscopic traffic model and produces a vehicle-based event-driven MesoSpec:

\[
MicroIR
\xrightarrow{LLM}
MesoSpec.
\]

The LLM composes a known low-level mesoscopic vocabulary rather than selecting prewritten reduction rules.

---

## C2 — Single-agenda mixed-resolution execution

Micro and meso execute as different movement semantics under the same Scalation process-interaction runtime:

\[
VTransport
\leftrightarrow
MesoTransport.
\]

The vehicle, route, simulation clock and event director remain shared.

This makes model resolution a property of the network segment rather than a property of an entirely separate simulator.

---

## C3 — Verified specification-to-execution semantics

The same MesoSpec is translated into:

\[
ScalaTion
\]

and:

\[
Maude.
\]

Translator-agreement tests establish that both implementations produce the same bounded semantic traces.

Maude then checks generated event semantics, and a predeclared mutation study tests whether those checks find meaningful scheduling/state faults.

Thus formal verification is tied to the actual generated executable model rather than to an independently handwritten formal surrogate.

---

# 23. Experiments required for the paper

Only these are required.

### E1 — Does the basic meso abstraction work?

Micro versus properly parameterized meso.

Measure fidelity.

This is the first stop/go gate.

### E2 — Can the model be generated?

Trace-fit/deterministic baseline versus LLM versus LLM+Maude.

### E3 — Do the two MesoSpec translators agree?

Bounded canonical trace agreement.

### E4 — Does Maude detect semantic defects?

Frozen mutation study.

### E5 — Can resolutions coexist?

\[
micro
\rightarrow
meso
\rightarrow
micro.
\]

### E6 — Is abstraction computationally worthwhile?

Event count reduction and wall-clock speedup.

That is enough.

---

# 24. Stop/go gates

## Gate 1 — Meso usefulness

Manually instantiate the target meso semantics first.

If:

\[
M_m
\]

cannot reproduce reasonable:

- flow,
- speed,
- travel time,
- congestion/queue behavior,

then stop.

Fix the abstraction.

Do not work on LLM generation yet.

---

## Gate 2 — Mixed-resolution viability

Before large LLM/Maude work, demonstrate:

\[
VTransport
\rightarrow
MesoTransport
\rightarrow
VTransport
\]

for the same vehicle in the same simulation director.

If this cannot be made robust:

\[
\boxed{\text{fix it immediately}}
\]

because the large-network vision depends on it.

---

## Gate 3 — LLM value

Compare the generated pipeline against the deterministic/trace-fitted alternative.

If a simple conventional translator solves the problem just as well:

\[
\boxed{\text{drop or weaken the LLM claim}}
\]

rather than manufacture complexity.

The research should survive that outcome.

---

# 25. Development plan

## Week 1 — Meso semantics

Implement:

```text
MesoTransport
MesoLink
MesoNode
Ready
Release
Block
storage/capacity
```

Manually instantiate it on the existing corridor.

Run micro versus meso.

This determines whether the research is viable.

---

## Week 2 — One-agenda mixed resolution

Demonstrate:

```text
MICRO -> MESO -> MICRO
```

with the same `Vehicle`.

Implement:

\[
\beta_{\mu m}
\]

and:

\[
\beta_{m\mu}.
\]

Implement blocked insertion without polling.

---

## Week 3 — MicroIR and MesoSpec

Freeze v0.1 of both representations.

Implement deterministic:

\[
MesoSpec\rightarrow ScalaTion.
\]

---

## Week 4 — Generation

Implement:

\[
MicroIR
\xrightarrow{LLM}
MesoSpec.
\]

Start LLM-versus-deterministic comparisons.

---

## Week 5 — Maude translation and agreement

Implement:

\[
MesoSpec\rightarrow Maude.
\]

Before formal claims, implement translator-agreement tests.

---

## Week 6 — Formal checks and mutation study

Freeze the mutation catalog before tuning the checker.

Run Maude and randomized-runtime detection experiments.

---

## Week 7 — Final traffic/performance evaluation

Freeze:

- primary metrics,
- secondary metrics,
- test scenarios.

Run all methods once the pipeline is locked.

Measure event counts and runtimes.

---

## Week 8 — Paper

PADS 2027 explicitly includes simulation algorithms/infrastructure, modeling methodology, AI and Simulation, transportation applications, and a dedicated Model Reduction special track.

The first-call deadline is October 30, 2026; the second call is January 22, 2027.

Do not sacrifice the experiment merely to hit the first call.

---

# 26. What is explicitly out of scope

For this paper:

- adaptive runtime selection of micro/meso regions;
- reinforcement learning;
- LLM fine-tuning;
- arbitrary Scala program understanding;
- signalized-city synthesis;
- CTM/LWR as the primary meso representation;
- whole LA;
- LA County;
- formal proof of every Scalation behavior;
- full reconstruction of mesoscopic states into arbitrary microscopic configurations;
- decision-preservation guarantees;
- multiple mesoscopic baselines;
- claims of exact micro–meso equivalence.

These can be later projects.

---

# 27. Relation to existing hybrid simulators

We must state this clearly in the paper.

Aimsun already supports micro, meso and hybrid traffic simulation and already handles vehicle transfer across micro/meso boundaries.

Therefore:

> “We implemented micro–meso hybrid traffic simulation”

is not a contribution.

The contribution is instead:

\[
\boxed{
\text{existing executable micro}
\rightarrow
\text{automatically generated lower-resolution model}
}
\]

and:

\[
\boxed{
\text{both resolutions share the same generic DES execution kernel}.
}
\]

We should also avoid claiming novelty simply because Maude appears in traffic; Maude's rewriting-logic and model-checking capabilities are established.

Any “first” claim should wait for a dedicated related-work search.

---

# 28. Long-term LA vision

If the corridor results succeed:

\[
\text{corridor}
\rightarrow
\text{freeway network}
\rightarrow
\text{regional network}
\rightarrow
\text{LA}.
\]

Eventually each segment can carry a resolution:

\[
r_e\in\{\mu,m\}.
\]

Later work can make that resolution dynamic:

\[
r_e(t)\in\{\mu,m\}.
\]

Then an LA-scale simulation might use mesoscopic resolution almost everywhere while retaining microscopic resolution around:

- bottlenecks,
- incidents,
- control interventions,
- areas requiring lane-level behavior.

But dynamic switching is not required to establish the present contribution.

---

# 29. Final research question

The final technical question is:

> **Can an existing process-interaction microscopic traffic simulator be automatically abstracted into a vehicle-based mesoscopic representation that executes within the same discrete-event runtime, retains useful traffic behavior, and substantially reduces event-processing cost?**

The AI/formal-method question is:

> **Can an LLM generate that abstraction from the semantic structure of the existing model while Maude constrains and checks the resulting event semantics?**

---

# 30. Final paper pitch

> We develop a method for deriving a vehicle-based mesoscopic traffic model from an existing microscopic Scalation simulator. Rather than coupling independent simulation engines, microscopic and mesoscopic links execute under the same process-interaction event scheduler and preserve vehicle identity across resolution boundaries. A common generated specification is compiled to both Scalation and Maude; bounded trace agreement checks ensure that the verified semantics are the semantics being executed, while mutation experiments evaluate whether formal checking detects event-ordering and queue-state defects. We evaluate the resulting models by traffic fidelity, mixed-resolution boundary behavior, event-count reduction, and execution time against a strong trace-fitted mesoscopic baseline.

---

# 31. The practical criterion

At the end, this project still comes down to the simple statement that started the discussion:

\[
\boxed{
\text{reasonable micro}
\approx
\text{useful meso}
}
\]

and simultaneously:

\[
\boxed{
N_{events}^{meso}
\ll
N_{events}^{micro}.
}
\]

If those statements are false, LLMs and Maude do not matter.

If they are true, then we have the foundation for:

\[
\boxed{
\text{automatically generated, mixed-resolution, city-scale traffic simulation}.
}
\]

That is the project to start.

---

# Final implementation invariant

The remaining note about **blocked-set bookkeeping** is important enough to treat as an implementation invariant:

\[
BlockedUpstream(e)
=
\{u \mid u \text{ has a head vehicle waiting for space on } e\}
\]

That relationship should live in **MesoSpec**, not only in Scalation code, so both translators expose the same wake-up dependency. A downstream `SpaceRelease(e)` must deterministically identify and reconsider the appropriate blocked upstream movement(s). That makes M5 a real semantic property rather than a Scala-only runtime bug.
===================================================================================================================================================



A Starting point maybe:
I checked the actual repository. **Do not convert `simulation/event` into the meso implementation.** It has useful ideas, but using it directly would undo the single-agenda architecture we just locked.

Your meso should live under **`simulation/process`**, beside `VTransport`, and use the existing `Vehicle`, `SimActor`, `Model`, agenda, and blocking/wakeup machinery.

### Why I am confident about this

`simulation/event.Model` owns its **own future-event list** of `Event` objects and advances its own clock by dequeuing those events.  Its `Entity` is also a separate event-simulation entity type, not your traffic `Vehicle`.

So if you built the meso using that package, you would get:

```text
process.Model agenda              event.Model eventList
       |                                  |
    Vehicle                            Entity
       |                                  |
 microscopic                         mesoscopic
```

That gives us **two schedulers and two entity representations again**.

Don't do that.

Your process package already has the machinery we need. `WaitQueue.waitIn()` can suspend a `SimActor` indefinitely until something reschedules it, and `Resource.release()` already demonstrates the exact wake-up pattern we designed: dequeue a waiting actor and `schedule(0.0)` when capacity becomes available.

---

# The meso tree I would build

Start here:

```text
scalation/
└── simulation/
    └── process/
        │
        ├── Model.scala                 EXISTING
        ├── SimActor.scala              EXISTING
        ├── Vehicle.scala               EXISTING
        │
        ├── Transport.scala             EXISTING
        ├── VTransport.scala            MICRO
        │
        ├── WaitQueue.scala             EXISTING
        ├── Resource.scala              EXISTING PATTERN
        │
        └── meso/
            │
            ├── MesoTransport.scala     <-- START HERE
            │
            ├── MesoNode.scala          <-- second
            │
            ├── MesoMovement.scala      <-- ramp/merge later
            │
            ├── MesoSpec.scala          <-- NOT YET
            │
            └── example_1/
                ├── MesoOneLinkTest.scala
                ├── MesoLinearTest.scala
                └── MixedMicroMesoTest.scala
```

I would **not create all of those today**.

For Gate 1, start with only:

```text
process/
└── meso/
    ├── MesoTransport.scala
    └── example_1/
        └── MesoOneLinkTest.scala
```

That's it.

---

# Class tree

This is the relationship we want:

```text
                    Component
                        |
                        v
                    Transport
                   /         \
                  /           \
                 v             v
          VTransport      MesoTransport
             MICRO            MESO
```

`VTransport` already extends `Transport`.

I recommend:

```scala
class MesoTransport(...) extends Transport(...)
```

as well.

That is much better than deriving the meso model from `simulation.event.Event`.

---

# Vehicle tree

There should be **no `MesoVehicle`**.

```text
                   SimActor
                      |
                      v
                   Vehicle
                      |
          +-----------+-----------+
          |                       |
          v                       v
   VTransport.move()       MesoTransport.move()
       MICRO                     MESO
          |                       |
    update every τ         sparse activation
    Gipps / IDM            Ready / Release
    x,v,a,leader           queue / capacity
```

The same `Vehicle` object survives the transition.

Your current `Vehicle` already extends `SimActor`.

That is exactly what we want.

---

# What `MesoTransport` should do

The first version should be conceptually this:

```text
Vehicle enters MesoTransport
          |
          v
occupancy++
enqueue vehicle
          |
          v
calculate free-flow time

tau0 = length / freeFlowSpeed
          |
          v
actor.schedule(tau0)
actor.yieldToDirector()
          |
          v
       READY
          |
          v
Can vehicle leave?
      /       \
    YES        NO
     |          |
     v          v
  RELEASE      BLOCK
     |          |
     |        yield
     |          |
     |     wait for wakeup
     |          |
     +----------+
          |
          v
occupancy--
continue vehicle.act()
```

The critical distinction remains:

$$
t^{ready}
=
t^{enter}
+
\frac{L}{v_f}
$$

not:

$$
t^{exit}=t^{enter}+L/v_f.
$$

It becomes **eligible** to exit at \(t^{ready}\).

---

# Minimal `MesoTransport` state

I would start with these fields:

```text
MesoTransport
│
├── length
├── freeFlowSpeed
├── capacity
├── storageCapacity
│
├── occupancy
├── nextReleaseTime
│
├── vehicles
├── readyQueue
│
└── blockedUpstream
```

Mathematically:

$$
\tau^0=\frac{L}{v_f}
$$

and

$$
h=\frac{1}{C}
$$

where \(h\) is minimum discharge headway.

For the very first one-link test, you only need:

```text
length
freeFlowSpeed
capacity
occupancy
nextReleaseTime
```

**Do not implement spillback on day one.**

First prove:

```text
vehicle enters
      ↓
waits free-flow time
      ↓
capacity controls departure
      ↓
vehicle exits
```

Then add storage/blocking.

---

# Where `simulation/event` is useful

I would treat it as a **reference library**, not the runtime.

| Existing event class | Use for meso?                  | Decision                              |
| -------------------- | ------------------------------ | ------------------------------------- |
| `event.Model`        | Own scheduler/FEL              | **No**                                |
| `event.Entity`       | Separate entity representation | **No**                                |
| `event.Event`        | Explicit occurrence semantics  | **Borrow concept**                    |
| `event.WaitQueue`    | Queue of `Entity`              | **No**                                |
| `event.EventNode`    | Graphical event prototype      | **No**                                |
| `event.CausalLink`   | Event causality                | **Concept useful for MesoSpec/Maude** |

`Event` itself is clean: it has an activation time, a live/cancel flag, and an abstract `occur()` method.

That's useful when we later design:

```text
Enter
Ready
Release
Block
SpaceRelease
```

for **MesoSpec/Maude**.

But don't instantiate `event.Event` objects in the production meso simulator.

---

# The process package already has our blocking algorithm

This was the strongest thing I found.

`process.WaitQueue.waitIn()` does this:

```text
actor joins queue
      ↓
actor.yieldToDirector()
      ↓
actor has NO next activation
      ↓
someone explicitly wakes it
```

Then `Resource.release()` does:

```text
if queue not empty:
    waitingActor = queue.dequeue()
    waitingActor.schedule(0.0)
```

That is almost exactly our:

```text
downstream SpaceRelease
          ↓
find blocked upstream
          ↓
wake waiting vehicle
```

So we are **not inventing a strange scheduling mechanism**. Scalation process simulation already supports the pattern.

---

# Important: don't modify `Pathway` yet

I found one issue that we should handle **after Gate 1**.

Your current `Pathway` is hard-coded as:

```scala
val seg = Array.ofDim[VTransport](...)
```

and creates every segment with:

```scala
new VTransport(...)
```

`Route` is even more micro-specific. It accesses things such as:

```text
seg.length
seg.safetydist
seg.getFirst
seg.getLast
```

for lane-changing logic.

So **do not start by hacking `Route` and `Pathway`**.

That will consume time and mix Gate 1 with Gate 2.

Build your meso model separately first.

---

# Development tree

## Step 1 — one meso link

```text
Source
  |
  v
MesoTransport
  |
  v
Sink
```

Test:

$$
\tau^0=L/v_f.
$$

Check vehicle traversal times and event counts.

---

## Step 2 — two meso links

```text
Source
  |
  v
MesoTransport-0
  |
  v
MesoTransport-1
  |
  v
Sink
```

Now test:

* capacity;
* FIFO;
* link-to-link transfer.

---

## Step 3 — add storage/spillback

```text
        Link 0
       occupancy
           |
           v
        Link 1
      [FULL K1]
           |
           X
         BLOCK

When Link 1 releases:
           |
           v
   SpaceRelease(Link1)
           |
           v
   wake Link0 head
```

This is where:

$$
BlockedUpstream(e)
$$

comes in.

---

## Step 4 — node/movement

Then add:

```text
                    MesoNode
                   /        \
                  /          \
              Movement     Movement
                 ^            ^
                 |            |
             Mainline       Ramp
```

Don't create a hard-coded `Merge`.

Use:

```text
MesoNode
└── Movement
    ├── fromLink
    ├── toLink
    ├── priority
    └── routeCondition
```

So:

```text
Mainline -----\
               >---- downstream
Ramp ---------/
```

is just two `Movement`s competing for the same downstream link.

---

# Full meso object tree later

```text
MesoNetwork
│
├── MesoTransport L0
│   ├── occupancy
│   ├── readyQueue
│   └── parameters
│
├── MesoTransport L1
│   ├── occupancy
│   ├── readyQueue
│   └── parameters
│
├── MesoTransport Ramp0
│
└── MesoNode N0
    │
    ├── Movement M0
    │   ├── from = L0
    │   ├── to   = L1
    │   └── priority = mainline
    │
    ├── Movement M1
    │   ├── from = Ramp0
    │   ├── to   = L1
    │   └── priority = ramp
    │
    └── blockedUpstream
```

That becomes the basis for MesoSpec later.

---

# One thing we will have to refactor for mixed resolution

Once Gate 1 passes, the desired final route becomes:

```text
Route
│
├── segment 0 → VTransport
├── segment 1 → VTransport
├── segment 2 → MesoTransport
├── segment 3 → MesoTransport
└── segment 4 → VTransport
```

Your current `Pathway` cannot represent that because its array is specifically `Array[VTransport]`.

**Do not fix this now.**

After Gate 1, we introduce a common road-segment abstraction and make both:

```text
VTransport
MesoTransport
```

valid pathway segments.

That is Gate 2 work.

---

# One issue in the existing process agenda we must remember

Your process agenda is:

```scala
PriorityQueue[SimActor]
```

and `SimActor.compare` currently orders actors using only `actTime`.

So actors scheduled at exactly the same timestamp currently have **no explicit phase/sequence ordering**.

That is fine for the one-link Gate 1 prototype.

It is **not sufficient later for merges and `SpaceRelease → Release → Ready` ordering**.

We already know this from the locked blueprint.

Do not solve it today, but mark it:

```text
TODO Gate 2:
stable equal-time event ordering
```

---

## What I recommend you create **right now**

Only this:

```text
src/main/scala/scalation/simulation/process/meso/
│
├── MesoTransport.scala
│
└── example_1/
    └── MesoOneLinkTest.scala
```

And make the first test:

```text
VSource / Source
      |
      v
 MesoTransport
      |
      v
    Sink
```

The first target is **not** LLM, Maude, merge, spillback or hybrid.

It is:

$$
\boxed{
\text{Does one event-sparse MesoTransport behave correctly?}
}
$$

Then:

$$
\boxed{
\text{Does it reduce activations compared with VTransport?}
}
$$

Once those two work, we add the second link and spillback. That is the fastest path that does not create throwaway architecture.
