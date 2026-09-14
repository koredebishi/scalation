# Paper Documentation

## System architecture

- The microscopic and mesoscopic parts run on the same ScalaTion process-interaction agenda rather than using separate simulators (see Blueprint §3).

- A vehicle keeps the same identity, route and simulation clock when it crosses a micro–meso boundary (see Blueprint §8).

## Intermediate representations

- MicroIR keeps only the network, demand, vehicle dynamics, solver, update interval and observables needed for abstraction (see Blueprint §9.1).

- Configured arrivals and route probabilities remain in MicroIR, while `nStop`, seeds, repetitions and animation remain ordinary run controls.

- `MicroIRJson` and `MesoSpecJson` use the shared `CanonicalJson` utility so both artifacts have stable RFC 8785 JSON and SHA-256 identities.

- MesoSpec is the single typed description consumed by both the ScalaTion and Maude translators (see Blueprint §9.2).

## LLM generation

- The LLM receives canonical MicroIR JSON and the MesoSpec schema, then returns candidate MesoSpec JSON rather than Scala source code (see Blueprint §11).

- `MesoSpecJson` rejects malformed or unknown fields before the candidate becomes a typed MesoSpec.

- `validate()` checks the internal mesoscopic model, while `validateAgainst()` checks its provenance and mapping against the source MicroIR.

## ScalaTion translation

- `T_S` constructs native ScalaTion runtime objects from an accepted MesoSpec instead of asking the LLM to generate executable code.

- Microscopic segments referenced by `MesoLink.microSegments` become mesoscopic links, while the remaining segments stay microscopic.

- The generated mesoscopic region uses `MesoVTransport`, while retained microscopic pockets continue to use `Pathway` and `VTransport`.

- Optional routing and signal control remain local junction features and are not imposed on every model.

## Formal translation

- `T_M` translates the same accepted MesoSpec into Maude so the formal model and executable model share one source of truth (see Blueprint §15).

- Translator agreement compares bounded canonical traces from ScalaTion and Maude before any verification result is accepted.

- Blocked-upstream wake-up relationships are derived from legal movements and queue state rather than stored as another MesoSpec relation.

## Large-network use

- The California hybrid model will use mesoscopic execution for most of the network and retain microscopic execution only in selected pockets.

- The current OSM integration supplies animation context, while simulation topology still comes from `NetworkConfig`, `CorridorLayout` and `CorridorBuilder`.

## Experimental status

- Gate 1 is closed because the mesoscopic mechanism passed the spillback and held-out capacity experiments.

- Gate 2 is closed because the same vehicle completed micro–meso–micro travel on one ScalaTion agenda.

- Gate 3 will compare LLM-generated MesoSpec models against the deterministic trace-fitted baseline (see Blueprint §24).
