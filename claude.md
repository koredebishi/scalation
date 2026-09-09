# ScalaTion Traffic Simulation

Microscopic + mesoscopic traffic simulation on ScalaTion 2.0 (Scala 3).
US-101 / I-210 corridors; IDM / Gipps / Krauss car-following; event-sparse mesoscopic links.

**IDE:** IntelliJ IDEA **HPC:** Sapelo2 (krb84578@uga.edu)

---

# SCALATION REPOSITORY — STRICT ENGINEERING RULES

These rules are mandatory for every analysis, proposal, and implementation in this repository.
They are the top-level instruction and override any default behavior.

## 1. REPOSITORY-WIDE SEARCH BEFORE DESIGN

Before proposing or implementing any:

- data structure;
- collection;
- queue;
- map;
- counter;
- registry;
- statistics holder;
- report;
- result DTO or case class;
- abstraction;
- helper;
- wrapper;
- scheduler mechanism;
- lifecycle mechanism;
- test utility;

search the entire Scalation repository for an existing native implementation.

The search must be repository-wide, not limited to the current package, directory, module, or feature.

Inspect:

- class and trait definitions;
- companion objects;
- inheritance hierarchies;
- existing call sites;
- existing tests and examples;
- reporting and statistics infrastructure;
- scheduler and coroutine infrastructure;
- related implementations in other simulation paradigms.

Do not assume that a required facility is missing merely because it is absent from the current package.

Before proposing anything new, report:

1. Which existing Scalation classes were found.
2. What each relevant class already provides.
3. Whether an existing class can be reused, extended, composed, or corrected.
4. Why a new implementation is necessary, if it is still necessary.

No scratch, substitute, or duplicate data structure may be created without my explicit approval after this analysis.

## 2. REUSE BEFORE CREATION

Prefer, in this order:

1. Direct use of an existing Scalation class.
2. Composition of existing Scalation classes.
3. A small extension to the proper existing abstraction.
4. Correction of an existing implementation.
5. A new class only when the first four options are demonstrably insufficient.

Do not create a local replacement merely because it appears easier.

Examples of prohibited duplication include:

- creating `BasicStats` when `Statistic`, `TimeStatistic`, and `getStatistics` already exist;
- creating manual count/time arrays when components already own statistics;
- creating a new queue when Scalation already has an appropriate queue;
- creating another actor ordering structure when the lane or pathway already owns one;
- creating report-specific copies of values already held by authoritative components;
- creating manual counters in several classes for an event that has one authoritative execution point.

## 3. DETERMINE THE AUTHORITATIVE OWNER

Before changing code, ask:

- Why is this change needed?
- What event or state is being represented?
- Which class actually owns that event or state?
- Where does the authoritative state transition occur?
- Is this simulation state, instrumentation, reporting, or presentation?
- Does an existing abstraction already expose it?

Place behavior at the point where it is authoritative.

Examples:

- Scheduler activation counts belong where the scheduler dequeues actors.
- Vehicle generation belongs to the source.
- Link occupancy and FIFO state belong to the transport.
- Lane-wide leader/follower order belongs to the pathway/lane.
- Sink completion and system travel time belong to the sink statistic.
- Replication aggregation belongs to the model's existing `statV`/`reportV` mechanism.
- Interval flow and speed belong to `Recorder`.
- Formatting and exporting belong to reporting code, not simulation-state classes.

Do not place feature-specific data in `Model` simply because `Model` is globally accessible.

A model is the simulation director. It must not become a miscellaneous storage container for traffic-specific reporting,
lifecycle maps, or convenience summaries.

## 4. ONE SOURCE OF TRUTH

Every measurement or state value must have one authoritative owner.

Do not maintain the same fact in multiple places unless the second representation is explicitly a read-only derived view.

For every proposed field, ask:

- Is this already represented by `Statistic.num`, `Statistic.mean`, `TimeStatistic.mean`, a component's state, a recorder
  matrix, the simulation clock, or another native value?
- Can this value be derived instead of stored?
- Could two copies disagree after a reset, exception, replication, or future code change?

Derived values should be calculated from authoritative data when reporting.

Examples:

- Completed vehicles = `Sink.durationStat.num`.
- Mean system travel time = `Sink.durationStat.mean`.
- Link completions = transport `durationStat.num`.
- Mean link travel time = transport `durationStat.mean`.
- Mean occupancy = transport `persistentStat.mean`.
- Density = mean occupancy × 1000 / link length.
- Throughput = completed vehicles × 3600 / simulation duration.
- Effective speed = link length / mean link travel time.
- Events per vehicle = scheduler activation count / completed vehicles.

Do not store these again in a parallel summary object unless I explicitly authorize it.

## 5. USE THE SCALATION REPORTING STANDARD

The reporting source of truth is the established Scalation infrastructure, including:

- `Statistic`;
- `TimeStatistic`;
- component `durationStat`;
- component `persistentStat`;
- `getStatistics`;
- `report`;
- `reportV`;
- `reportF`;
- `statV`;
- `Recorder`, where interval detector matrices are required.

New simulation components must feed this infrastructure through the established mechanisms such as `tally` and `accum`.

Do not create:

- parallel basic-statistics classes;
- independent sink summaries;
- independent link-time arrays;
- independent link-count arrays;
- string-keyed lifecycle-count maps;
- manually formatted reports that recompute existing statistics;
- separate post-run copies merely to work around lifecycle behavior.

If the standard reporting lifecycle resets data too early, correct the lifecycle while preserving standard `Statistic`
objects. Do not work around it by building a second reporting system.

Specialized exporters may format standard statistics, but they must not become another source of truth.

## 6. MEASURE EVENTS AT THE ACTUAL EVENT LOCATION

Instrumentation must be placed where the measured action actually occurs.

For example, do not add `activations += 1` independently to both `VTransport` and `MesoVTransport` if the desired metric
is scheduler activations.

The authoritative scheduler event occurs when `Model` dequeues an actor from its agenda. Count it there once using the
existing `Statistic` infrastructure.

Measurements used for comparisons must have:

- the same definition;
- the same measurement point;
- the same units;
- the same start and stop boundaries;
- the same reset lifecycle;
- symmetric implementation for all compared models.

Do not compare a manually counted mesoscopic metric with an unmeasured or differently defined microscopic metric.

## 7. DISTINGUISH SIMULATION STATE FROM REPORTING

Simulation state must remain with the component that needs it to execute correctly.

For example, a mesoscopic transport may legitimately own:

- its FIFO queue;
- readiness state;
- occupancy;
- next release time;
- entered and released conservation state;
- storage capacity;
- blocked-upstream relationships.

Those values exist because the simulation requires them.

Reporting should observe or derive from that state. It must not maintain independent copies.

Conversely, presentation methods, CSV formatting, labels, comparison tables, and report text must not be embedded in core
simulation components.

## 8. PRESERVE MODELING SEMANTICS

Do not replace an established domain structure with a simpler global structure without proving semantic equivalence.

For this traffic model:

- A microscopic `Pathway` owns one lane-wide `DoublyLinkedList[Vehicle]`.
- That list provides leader/follower relationships across all microscopic segments in the lane.
- `Vehicle.myPathNode` and `Vehicle.myPathway` are the current microscopic ownership mechanism.
- Do not return to the legacy global `SimActor.alist`.
- A purely mesoscopic pathway does not need a lane-wide microscopic DLL.
- Each mesoscopic link owns FIFO queue state.
- Meso-to-micro transfer must explicitly join the destination microscopic pathway.
- Micro-to-meso transfer must explicitly leave the microscopic pathway and enter the mesoscopic queue.

Never introduce a structure that silently changes vehicle ordering, positional frames, leader selection, or ownership.

## 9. CHANGES TO FOUNDATIONAL CLASSES REQUIRE ADVANCE REVIEW

Before touching any foundational or previously undisclosed class—including, but not limited to:

- `Model`;
- `Component`;
- `SimActor`;
- `Vehicle`;
- `Transport`;
- `VTransport`;
- `Pathway`;
- `Route`;
- `Source`;
- `VSource`;
- `Sink`;
- `Recorder`;
- `Statistic`;
- `TimeStatistic`;

provide me with:

1. The complete list of files and classes that would be affected.
2. The exact responsibility of every proposed change.
3. Why each class is the correct owner.
4. Which existing implementation is being reused.
5. What will be removed.
6. What will remain.
7. What will be moved and where.
8. Any observable behavior that could change.
9. Any risks to existing microscopic, mesoscopic, animation, calibration, or reporting behavior.

Wait for my explicit approval before editing any of those classes.

Do not interpret approval for one class as approval for another.

If implementation reveals that another class must change, stop immediately, disclose the new scope, and wait for
approval.

## 10. DO NOT EXPAND THE APPROVED SCOPE

Edit only the files and responsibilities I explicitly approve.

Do not perform opportunistic:

- refactoring;
- cleanup;
- renaming;
- formatting;
- API redesign;
- documentation rewriting;
- dependency updates;
- generated-file cleanup;
- warning fixes;
- unrelated bug fixes.

If an unrelated issue is discovered, report it separately. Do not fix it without approval.

Preserve all existing user changes and unrelated working-tree modifications.

## 11. NO COMPILATION OR EXECUTION

Do not compile my repository.

Do not run:

- `sbt compile`;
- tests;
- `runMain`;
- simulations;
- benchmarks;
- formatters that rewrite files;
- package commands;
- dependency installation;
- IDE builds;
- generated-code tasks.

I will compile and run the code myself.

After an approved edit:

1. Perform only read-only inspection.
2. Show the exact files changed.
3. Explain the change.
4. Give me the command or main class I should run.
5. Wait for me to return the output.

Do not request permission to compile. Do not attempt compilation under another shell, cache, home directory, or
environment.

## 12. INTERPRETATION OF COMMON INSTRUCTIONS

The word "continue" means:

- continue analysis;
- continue explanation;
- continue reviewing evidence;
- continue the current discussion.

It does not authorize:

- writing code;
- editing files;
- compiling;
- running tests;
- running simulations;
- changing configuration;
- deleting files.

"No code yet" prohibits all source-code changes.

"Read only" permits inspection and analysis only.

"Go ahead" authorizes only the exact change most recently described, including only the disclosed files. It does not
authorize broader work.

If the scope is ambiguous, ask one concise question before editing.

## 13. REQUIRED PRE-IMPLEMENTATION RESPONSE

Before every implementation, provide a short design review containing:

- Goal.
- Existing native Scalation facilities found.
- Proposed authoritative owner.
- Exact affected files/classes.
- Exact additions.
- Exact removals.
- Derived values that will not be stored.
- Why no duplicate state is introduced.
- What I must run manually afterward.

Then wait for explicit approval.

## 14. TEST AND EXAMPLE CODE

Tests and examples must not pollute production models with temporary state.

Use existing Scalation test utilities, variates, statistics, and components whenever available.

Temporary correctness assertions should remain in a test/example file, not in production classes.

After a temporary test passes, remove its test harness if I request removal.

Do not introduce a collection merely to test an ordering property when scalar state or an existing Scalation statistic is
sufficient.

## 15. EVIDENCE AND HONESTY

Base conclusions on inspected repository code.

Provide file and line references when explaining ownership or existing behavior.

Clearly distinguish:

- what the current code does;
- what the blueprint requires;
- what is proposed;
- what has been verified by my run;
- what remains untested.

Do not call a gate complete when only a subtest has passed.

Do not describe two metrics as comparable unless they were measured symmetrically.

Do not describe a structure as native unless it was found in the repository.

If the repository already contains a suitable implementation, use it.

If no suitable implementation exists, state exactly what was searched and why the found alternatives are insufficient,
then wait for explicit authorization before creating anything new.

## 16. FINAL OPERATING PRINCIPLE

Search first.
Understand the existing architecture.
Identify the authoritative owner.
Reuse the native Scalation implementation.
Maintain one source of truth.
Derive rather than duplicate.
Disclose every affected class.
Wait for approval.
Edit only the approved scope.
Never compile or run my work.

---

# Reference

Session notes and pointers below are context, not authority. Verify against current code before asserting.

## Additional behavioral guidelines

- Do not plagiarize — all paper text must be original.
- Do not hallucinate — if unsure, say so.
- For session continuity, write a dated handoff file under `docs/phd-papers/` (see `traffic_meso_handoff.md`). Do not append session state into this file.

## Context files

| Need | File |
|------|------|
| Behavioral guidelines | `context/rules.md` |
| Coding style & key files | `context/scalation-style.md` |
| IDM params & domain knowledge | `context/traffic-simulation.md` |
| Config layer standard | `context/config-layer-standard.md` |
| Traffic state spec | `context/traffic-state-spec.md` |
| Variable lane count problem | `context/variable-lane-architecture.md` |
| HPC Sapelo2 commands | `context/hpc.md` |

## Micro-to-meso work (active)

| Need | File |
|------|------|
| Research plan | `docs/phd-papers/final_micro_to_meso_blueprint.md` |
| Meso session handoff | `docs/phd-papers/traffic_meso_handoff.md` |
| Gate-2 component design | `docs/phd-papers/mixed_resolution_pathway_design.md` |

Target venue: PADS 2027 (first call ~Oct 30, 2026).

## Papers / studies

| Study | Topic | Status |
|-------|-------|--------|
| Study 1 | Structural sensitivity (integrators, arrivals) | ANNSIM 2026 — submitted |
| Study 2 | Wildfire evacuation & contraflow (I-10, Palisades) | WSC 2026 track |
| Study 3 | Micro↔meso abstraction / mixed-resolution | Active — see blueprint |

Calibration is a supporting result, not a standalone paper.

## Key entry points (Rule 11: hand these to the user; do not run them)

| Model | File | Main |
|-------|------|------|
| US-101 | `model/CalRoute101_3.scala` | `runCalRoute101_3` |
| Eaton Fire (I-210 WB + SR-134 WB) | `model/EatonFireModel.scala` | `runEatonFireModel` |
| Calibration eval | `builder/CalibrationFramework.scala` | `eval` |
| Meso example | `meso/example_meso/OneWayPathwayMeso.scala` | `runOneWayPathwayMeso` |
| Micro example | `example_1/OneWayPathwayTest.scala` | `runOneWayPathwayTest` |
| Dynamics | `Dynamics.scala` (IDM, Gipps, Krauss) | — |
