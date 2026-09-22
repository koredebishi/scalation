# Repository Working Rules

These instructions apply to all work in this repository.

## Reuse Scalation Before Implementing Anything New

- Before implementing any data structure, collector, statistic, report, utility, abstraction, or helper, search the entire Scalation repository for an existing native implementation. Search repo-wide, not only in the current package or feature directory.
- Identify the existing candidates and explain why they do or do not satisfy the requirement before proposing a new implementation.
- Prefer adapting, extending, composing, or correctly using an existing Scalation class over introducing a parallel implementation.
- Do not create a scratch, substitute, or duplicate data structure unless the user explicitly requests or approves it after reviewing why no existing Scalation implementation is suitable.

## Justify Ownership Before Editing

- Before making a change, ask why the change is needed and which class properly owns the responsibility.
- Do not place feature-specific state or reporting inside a broad class merely because it is convenient. For example, `Model` is the simulation model/director; do not add a parallel `BasicStats` system there when Scalation already has `Statistic`, `TimeStatistic`, `getStatistics`, `report`, and `reportV`.
- Use the established statistics/reporting system as the source of truth. Derive feature-specific measures from those statistics and the minimum unique domain inputs (for example, deriving density from the appropriate statistic and link length) instead of maintaining duplicate counters or reports.

## Independent Design Review — No Yes-Man Implementation

- Independently evaluate every proposed design, regardless of whether it comes from the user, another AI, existing notes, or an earlier recommendation.
- Do not accept a proposal merely because it sounds plausible or is presented confidently. Clearly state agreement, disagreement, uncertainties, and tradeoffs before proposing implementation.
- For every proposed field, class, relation, or abstraction, determine:
  - whether it is fundamental semantics, derived information, or runtime state;
  - which class properly owns it;
  - which concrete consumer needs it;
  - whether it can be derived cheaply and deterministically from an existing authority;
  - whether storing it would create duplicate or conflicting sources of truth.
- Required behavior does not automatically justify adding a stored representation. Prefer deriving information from the authoritative model when practical.
- If further reasoning exposes a weakness in an earlier recommendation, correct it directly before implementation rather than defending or preserving it.

## No Plumbing-Only or Throwaway Models

- Do not implement a connectivity harness, plumbing model, toy source/sink path, or temporary scaffold and present it as progress toward the final simulation.
- Every model change must exercise the real semantics it is intended to support: actual network topology, resolution behavior, routing, demand, storage/capacity, and observable validation as applicable.
- A reduced example is acceptable only when it is a deliberate, reusable regression test for a named contract; label that contract and its limits before coding.
- Do not add PeMS demand, animation, or paper metrics on top of a network whose edge identity, path continuity, lane transitions, junction timing, or sink semantics are unresolved.
- If a proposed implementation is only plumbing, stop and say so; do not code it.

## Approval for Cross-Cutting Changes

- Before touching `Model`, `Component`, or any class the user has not already been told will change, show the user the complete list of affected classes/files and describe the intended change to each one.
- Wait for the user's explicit approval of that scope before editing those classes.
- If the scope expands during the work, stop and disclose the additional classes and intended changes before touching them.

## Builds and Execution

- Do not compile this repository for the user.
- Do not run build, test, benchmark, or simulation commands unless the user explicitly requests that exact execution. The user performs compilation by default.
- A message such as "continue" authorizes continued discussion or analysis only. It does not authorize code changes, compilation, tests, benchmarks, or simulations.

## EatonMiniTest Handoff — 2026-09-20

### Current focus

Use the actual Eaton GeoJSON network for both simulation and animation, with PeMS-driven traffic and existing sparse routing. This is Eaton application work, not the PADS blueprint work. Preserve the user's native Scalation style and disclose affected files before requesting implementation approval.

### Implemented

- `src/main/scala/scalation/simulation/process/builder/NetworkBuilder.scala` preserves `ref`, `key`, and `osmid` in `NetEdge`. Directed edge identity for station bindings is `(u, v, key)`.
- `data/network/eaton/station_edges.csv` contains 48 candidate bindings (40 mainline and eight ramp/connector stations) and seven explicit exclusions. These are reviewed candidates, not established lane-level detector coverage.
- `src/main/scala/scalation/simulation/process/config/EatonCorridorConfig.scala` adds `loadStationEdges`, reusing `StationRecord` and `NetEdge`. It validates station and edge IDs, rejects duplicate station bindings, and retains exclusion reasons. There is no nearest-edge fallback.
- `src/main/scala/scalation/simulation/process/model/EatonMiniTest.scala` loads and prints these bindings at startup. The user's run printed 48 mapped, seven excluded, and 65 unreviewed WB stations. Missing mappings are explicit, not silently assigned.
- `src/main/scala/scalation/simulation/process/arrival/ArrivalSource.scala` now retains PeMS station IDs in the cached aggregated ramp data and provides `loadRampByStation`; the existing positional `loadRamp` API remains available.
- `EatonMiniTest` now mixes in the existing `RowTimeLoader` contract, uses aggregated I-210 mainline demand, creates the reviewed I-210 ramp sources by station ID, and assigns each vehicle to an I-210 or SR-134/CA-2 terminal using the existing split-ratio calculation. `VSource` was not modified.
- Read-only data checks found no missing referenced edges or unknown stations. No agent compilation or simulation was performed. The user's supplied output shows startup and routing initialization, not a completed simulation result.

### What the model still does

- It still generates 30 synthetic vehicles using `nStop = 30` and `Sharp(5.0, stream)`, from OSM node `1687098935` to sink `1262444667`.
- Station bindings identify roads and print the mapping; the current mini model now uses the upstream mainline and four reviewed I-210 ramp bindings for arrivals. It still does not attach detector collection or compare simulated counts with PeMS.
- The user's run loaded 10,127 nodes and 25,093 edges: 661 micro edges and 24,432 meso edges.
- `motorway` and `motorway_link` use microscopic IDM; EatonMiniTest selects Ballistic integration. Other road classes use mesoscopic transport with an IDM-derived speed relation. Both resolutions share the agenda and vehicle actors.
- Keep existing sparse routing. Do not replace it with the dense cost matrix used in the small routing example.

### Agreed destination design

- Reuse the idea in `meso/example_meso/DestinationRoutingMesoMicro.scala`: `Car(origin, destination)` receives its destination at creation and routing selects the path.
- That example alternates destinations using fixed five-second arrivals; its synthetic 50/50 assignment is not an Eaton demand model. Do not copy its clock-based alternation into stochastic arrivals.
- PeMS supplies entry/on-ramp counts and timing, and internal observations for comparison. Counts do not uniquely specify individual destinations or an OD matrix.
- Internal mainline stations are observations, not extra sources. Freeway connectors must not generate duplicate traffic already represented upstream.
- Destination proportions must be supported or explicitly labelled assumptions. The existing EatonFireModel ratio `flow_717603 / flow_717634` is a proxy, not a directly measured turning fraction.

### Important topology and exclusions

- I-210 WB splits at node `279669949`: `279669949 -> 279669952` continues I-210; `279669949 -> 1377536883` enters SR-134. Both edges are tagged `motorway`; searching only `motorway_link` misses the split.
- The current WINONA source node `1687098935` is downstream of this split. It cannot supply the upstream westbound I-210/SR-134 choice.
- `PeMSDemand.I210_WB_Anchor()` names station **717653** (Rosemead), not 717615. Verify its time window and coverage before wiring it.
- Excluded stations: 717636 (Hill exit candidate 271 m away), 716583 (Fair Oaks OR nearest road is an exit), 716582 (Walnut entrance unresolved), 770165/775725 (shared coordinates and unresolved connector assignment), 773979/773980 (shared candidate with unresolved detector coverage).
- Co-located sensors must not automatically be summed or turned into separate sources. Other stations, including HOV records, remain unreviewed in this mapping file.

### Next session — start here

1. Add station-ID lookup for the SR-134 FF file (the current loader handles aggregated OR data; the reviewed SR-134 connector remains excluded from source creation).
2. Check the new mainline/ramp source indexing and time-bin behavior by user-run compilation and simulation.
3. Add detector/count comparison using native Recorder/statistics facilities, without duplicate counters.

The user agreed with the destination approach, but the next implementation scope has NOT been presented or approved. Do not treat this handoff as approval to edit runtime classes, compile, or run simulations.

## OSM API Ownership Rule — 2026-09-21

- `OsmSpec` declares a named network, its bounds, and optional real sensor locations.
  `OsmSpec.ensurePrepared()` reuses `data/network/<name>/edges.geojson` or invokes
  the configured venv and `build_osm_network.py` to create it.
- `NetworkBuilder` is the reusable OSM topology API. It reads the prepared directed
  GeoJSON, owns shared graph nodes and geometry, and consumes a caller-supplied
  `EdgePolicy` for resolution/speed classification, micro/meso component construction,
  routing costs, and generic mixed-resolution traversal.
- `process/model/eaton/` owns Eaton-specific source IDs, PeMS demand, destination
  selection, projection, and driving. `EatonMiniTest` is the lifecycle orchestrator;
  `EatonMiniConfig`, `EatonMiniData`, `EatonMiniNetwork`, and `EatonMiniDriver` hold
  the cohesive Eaton responsibilities. Those concerns must not be moved into
  `NetworkBuilder`.
- `BuiltGraph.drive` is generic graph traversal, not Eaton-specific logic: it applies
  routing, lane handoff, micro/meso movement, junction crossing, and sink completion.
  Keep it with the reusable built graph unless a future API replaces it for every OSM
  model, not by copying it into an individual model.
- CalRoute models may use the same OSM API, but their analytical corridor topology
  must not be silently replaced by the full OSM graph until station, ramp, route, and
  resolution mappings have been validated.
