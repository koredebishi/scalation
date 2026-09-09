# Mixed-resolution Pathway design (Gate 2 shape) — 2026-09-08

Design elaboration of Gate 2 in `final_micro_to_meso_blueprint.md`: how the existing
components compose for `μ → m → μ` without a parallel class tree. Companion to
`traffic_meso_handoff.md`.

---

You don't need `PathwayMix` or `RouteMix`. The blueprint's "common road-segment abstraction" means **widen the `Pathway` you already have**, not clone it.

## The three lists, sorted out

They're not three competitors for one job:

- **`SimActor.alist`** — legacy, only `OneWayVehicle`-style examples use it. The hybrid ignores it entirely. Dead-end.
- **`Pathway.vList`** — the micro car-following substrate: one ordered list per lane, `myPathNode.ahead` = your leader. Only micro segments need it.
- **meso `vdeque`** — a FIFO *queue* local to one meso link. Different data structure, different job. Not competing with `vList`.

A vehicle holds `myPathNode`. On a micro segment it's a node in `vList`. On a meso link `myPathNode == null` and it's in that link's FIFO. That toggle **is** the `¬(Micro ∧ Meso)` invariant — not two simulators.

## The design

**1. Generalize `Pathway`.**
`seg: Array[VTransport]` → `seg: Array[Transport]`. Both `VTransport` and `MesoVTransport` already extend `Transport`; both have `move()`. The constructor takes a list of segment specs, each tagged `micro`/`meso`, and builds the right subtype. `Pathway` still owns **one `vList`** for the lane. That's the whole structural change — it de-hardcodes an array type the blueprint explicitly flags.

**2. Boundary logic goes inside `move()`, not into `act()`.**
Template method on `Transport`, trivially overridden:
- `VTransport.move()` entry: `if actor.myPathNode == null then owner.joinAt(actor, owner.findLeaderForSeg(i), i)` — enters `vList` behind the right micro leader (β_mμ)
- `MesoVTransport.move()` entry: `if actor.myPathNode != null then owner.removeFromAlist(actor)` — leaves `vList` (β_μm), then local FIFO as today

The Car's `act()` stays the dumb loop it is now: `for i <- seg.indices do seg(i).move(); junc(i).jump()`. No branching, no type matching in the model.

**3. Gap acceptance (the only real new physics).**
In `VTransport.move()` entry, when `myPathNode` was null (coming from meso): compute safe insertion speed from `myPathNode.ahead`; if no gap, `waitQueue.waitIn()`, woken by the leader's next release. This is β_mμ's job and it's ~15 lines. Aimsun does the same thing.

**4. Equal-timestamp ordering.**
Add an `eventPhase` field to `SimActor.compare` so `SpaceRelease → Release → Ready` is deterministic. One field, one comparator change.

## `μ → m → μ` test model

`Pathway("lane", segs = [Micro(350), Meso(700), Micro(350)], entry, exit)`. Same `Car.act()` loop as `OneWayPathwayTest`. The vehicle: joins `vList` on seg 0, is removed entering seg 1 (meso FIFO), rejoins `vList` with gap check entering seg 2, removed at the sink.

## What stays untouched

`Source`, `VSource`, `Sink` — unchanged (a source vehicle entering a micro first-segment gets `joinAt`'d by that segment's `move()`; a meso first-segment doesn't). `Junction` — unchanged for micro-micro; `MesoNode` for meso merges is later. `Route` / lane-changing — out of scope for Paper 1's single-corridor hybrid; that's the only place a `RouteMix` question could ever come up, and it's explicitly later work.

So the total Gate-2 footprint: widen `Pathway.seg` to `Transport`, put ~2 lines of `vList` admit/release in each `move()`, ~15 lines of gap acceptance, one `SimActor` ordering field. No parallel class tree. That's the "resolution is a property of the segment" claim, delivered.
