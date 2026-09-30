# ScalaTion Traffic Simulation

Microscopic + mesoscopic traffic simulation on ScalaTion 2.0 (Scala 3).
US-101 / I-210 corridors; IDM / Gipps / Krauss car-following; event-sparse mesoscopic links.

**IDE:** IntelliJ IDEA **HPC:** Sapelo2 (krb84578@uga.edu)

---

# Rules (mandatory — they override default behavior)

## Before any code
1. **Search first.** Before adding any data structure, statistic, report, helper or abstraction, search the whole
   repo (not just the current package) for an existing ScalaTion class. Say what you found, what it provides, and
   why it does or doesn't fit. No new or duplicate structure without my approval.
2. **Reuse order:** use as is → compose → small extension → correct the existing one → new class (last resort).
3. **Right owner.** Put behavior where the state or event really lives (scheduler counts where `Model` dequeues;
   generation in the source; FIFO/occupancy in the transport; lane order in the `Pathway`; system travel time in
   `Sink`; interval flow/speed in `Recorder`; formatting in reporting code). `Model` is the director, not storage.
4. **One source of truth.** Derive, don't copy: completed = `Sink.durationStat.num`, travel time =
   `durationStat.mean`, occupancy = `persistentStat.mean`, density = occupancy × 1000 / length, etc.
5. **Use the ScalaTion reporting standard** (`Statistic`, `TimeStatistic`, `durationStat`, `persistentStat`,
   `tally`/`accum`, `statV`/`reportV`, `Recorder`). Exporters may format it; they never become a source of truth.
   If the lifecycle resets data too early, fix the lifecycle — don't build a second reporting system.
6. **Measure symmetrically.** Count an event where it actually happens, once. Compared metrics need the same
   definition, point, units, boundaries and reset.
7. **Simulation state stays in components; report text and CSV formatting stay out of them.**
8. **Keep modeling semantics.** A micro `Pathway` owns one lane-wide `DoublyLinkedList[Vehicle]`
   (`Vehicle.myPathNode` / `myPathway`); never return to `SimActor.alist`. Meso links own their FIFO. Meso→micro
   must join the pathway; micro→meso must leave it. Never silently change ordering, frames, leaders or ownership.
9. **Independent review, no yes-man.** Judge every design on its merits (mine, another AI's, old notes). For each
   field/class ask: fundamental, derived or runtime state? Who owns it? Who needs it? Can it be derived? If a
   weakness shows up in an earlier recommendation, say so and correct it.
10. **No plumbing-only or throwaway models.** No toy harnesses or placeholder sources presented as progress. A
    reduced example is fine only as a named, reusable regression test.

## Approval and scope
11. **Foundational classes need advance review** (`Model`, `Component`, `SimActor`, `Vehicle`, `Transport`,
    `VTransport`, `MesoVTransport`, `Pathway`, `Route`, `Source`, `VSource`, `Sink`, `Junction`, `Recorder`,
    `Statistic`, `TimeStatistic`, and any class I haven't been told will change). List affected files, the change
    to each, why that class owns it, what is reused/removed/kept, and risks. Wait for approval per class. If the
    scope grows, stop and disclose.
12. **Design review before every implementation:** goal, existing facilities found, owner, exact files,
    additions, removals, what is derived (not stored), what I run afterwards. Then wait.
13. **Edit only the approved scope.** No opportunistic refactors, renames, formatting or unrelated fixes — report
    them separately. Never create, move, rename or delete a file or folder (code, config, docs, data) without
    stating the path and getting a yes. Preserve my unrelated working-tree changes.
14. **Words:** "continue" = keep discussing only. "No code" / "read only" = no edits. "Go ahead" / "approved" =
    only the change just described, in the files disclosed. Ambiguous → ask one short question.

## Never run anything
15. **Do not compile, run models, run tests, or run git** (not even `git status`/`diff`). After an edit, show the
    changed lines, say it is unverified, and give me the command to run. The only exception is a run I explicitly
    ask for, that one time.
16. Tests and examples use synthetic arrivals (`nStop` ≈ 100), not the full PeMS day, and keep temporary checks
    out of production classes.

## Evidence
17. Base claims on the code and on run outputs — cite `file:line`. When judging a model, look at the run data
    (validation CSV / report / recorders), not only the code. Keep separate: what the code does, what is proposed,
    what my run verified, what is untested. Never call two metrics comparable unless measured the same way.

---

# OSM network ownership (from 2026-09-21)
- `OsmSpec` declares a named network, bounds and sensor locations; `ensurePrepared()` reuses or builds
  `data/network/<name>/edges.geojson`.
- `NetworkBuilder` is the reusable OSM topology API (nodes, geometry, `EdgePolicy`, micro/meso construction,
  routing, generic traversal). `BuiltGraph.drive` is generic traversal — keep it there.
- Model-specific demand, sources, destinations and driving live with the model (`process/model/eaton/`), never in
  `NetworkBuilder`.
- CalRoute models keep their analytical corridor until station, ramp, route and resolution mappings are validated.

---

# Reference (context, not authority — verify against current code)

| Model | File | Main |
|---|---|---|
| US-101 micro | `model/calroute101/CalRoute101_3.scala` | `runCalRoute101_3` |
| US-101 mixed micro/meso | `model/calroute101/CalRoute101_3_withMeso.scala` | `runCalRoute101_3_withMeso` |
| US-101 full meso | `model/calroute101/CalRoute101_3_fullMeso.scala` | `runCalRoute101_3_fullMeso` |
| US-101 evaluation | `model/calroute101/CalibrationFramework.scala` | `eval`, `evalWithMeso`, `evalFullMeso` |
| US-101 on OSM builder | `model/calrouteMix101/CalRouteMix101_3.scala` | `runCalRouteMix101_3` |
| Eaton (I-210 / SR-134) | `model/eaton/EatonMiniTest.scala` | `runEatonMiniTest` |
| Meso example | `meso/example_meso/OneWayPathwayMeso.scala` | `runOneWayPathwayMeso` |
| Micro example | `example_1/OneWayPathwayTest.scala` | `runOneWayPathwayTest` |
| Dynamics | `Dynamics.scala` (IDM, Gipps, Krauss) | — |

All paths are under `src/main/scala/scalation/simulation/process/`. Eval results go to `log/simulation/`.

| Need | File |
|---|---|
| Style & key files | `context/scalation-style.md` |
| IDM params & domain | `context/traffic-simulation.md` |
| Config layer | `context/config-layer-standard.md` |
| Traffic state spec | `context/traffic-state-spec.md` |
| Variable lanes | `context/variable-lane-architecture.md`, `context/variable-lane-dll-unification.md` |
| HPC commands | `context/hpc.md` |
| Micro→meso plan / log | `docs/phd-papers/final_micro_to_meso_blueprint.md`, `docs/phd-papers/micro_to_meso_validation_log.md` |

**Papers:** Study 1 structural sensitivity — ANNSIM 2026 (submitted). Study 2 wildfire evacuation (Eaton) — WSC 2026
track. Study 3 micro↔meso mixed resolution — active, target PADS 2027. Calibration is supporting, not a paper.
Paper text must be original; if unsure, say so.

**Session handoffs** go in a dated file under `docs/phd-papers/`, never in this file.
