You're not mistaken — this is directly usable, but as a **template**, not a 1:1 port. Here's how section 3.2's method maps onto a ScalaTion meso formalization.

## What the Ptolemy/RTM method actually gives you

Their recipe is: formalize the DE **director loop once** (Fig. 1) as a rewrite theory, then formalize each **atomic actor** as a tiny transition system with three hooks — `init` / `fire` / `postfire` — all plugging into that one loop. Composites just flatten (transparent director only). That decomposition is the reusable idea.

## The mapping for your paper

**1. The director loop → formalize once.**
`process.Model.act()` (the pseudocode) is your Fig. 1. In Maude it becomes the engine: a configuration holding `{ clock, agenda (time-ordered multiset of scheduled activations), component states }` and one main rewrite rule — *take the earliest activation, advance the clock to its tag, apply that component's rule, reschedule*. This is contribution C2 ("single agenda") stated formally.

**2. Each meso primitive → an `init`/`enter`/`ready`/`release` transition system.**
Your "actors" are the MesoSpec primitives, and the blueprint already names their events (`Enter, Ready, Release, Block`). Map each to the three-hook shape:

| MesoSpec primitive | Ptolemy analogue | hooks |
|---|---|---|
| `MesoTransport` (link) | `Time Delay` | `enter`: post `Ready` at `t + L/v_f`; `ready`: check capacity/downstream → `Release` or `Block`; on release: post next FIFO head |
| FIFO + `releaseHeadway` | `Noninterruptible Timer` | delay next head until `releaseHeadway` since last release |
| `MesoNode` / `Movement` | atomic `FSM` actor | one guarded transition per firing when downstream space is available |
| `Source` | `Clock` | `init` posts first arrival; `postfire` posts the next |
| `Sink` | `Timed Plotter` | record arrival time; no output |

This tells you the **granularity** of the Maude rules: one rule module per primitive, each "on event E at time t: update local state, emit/schedule E′." That's exactly `init`/`fire`/`postfire`.

**3. Composite / hierarchy → the corridor, transparent only.**
The Ptolemy paper restricts to the transparent case "since we verify DE models." Your single-agenda architecture *is* always transparent — a `Pathway`/corridor flattens into the one agenda. You get to cite that precedent and skip hierarchical-director semantics entirely.

## Why it matters specifically for the paper

- **Scopes the formalization.** You formalize the director once + ~5 primitives, each small. Same scale as their ~10 actors. Not "all of ScalaTion."
- **Structures T_M (MesoSpec → Maude, blueprint §15).** Per-primitive rule modules make translator agreement checkable primitive-by-primitive against T_S (MesoSpec → ScalaTion).
- **Forces the equal-timestamp discipline (blueprint §7) into the director spec.** Ptolemy resolves simultaneous events with a defined tag/microstep order; your agenda currently orders by `actTime` only (`SimActor.compare` — verified, no tie-break field). The Maude director rule must fix the order `SpaceRelease → Release → Ready → seq`, and — per their method — that lives in the **director** formalization, inherited by every primitive. This is the load-bearing addition.
- **Real-Time Maude, not plain Maude.** Your times are real-valued (`t + L/v_f`, `releaseHeadway`), like theirs. RTM is the tool; that paper is your citation for "RTM handles dense-time DE."

## Where the analogy breaks (state honestly)

- **No port fixpoint.** Ptolemy iterates `fire()` to a fixed point over ports each tag. ScalaTion process-interaction has none — components touch shared mutable state directly (a vehicle joining a lane's `vList`, a link's `occupancy`). Your Maude configuration models that shared state explicitly as configuration terms, not as port values. Simpler in one way (no fixpoint loop), but the shared state must be first-class.
- **Coroutine ≠ `fire()`.** A `SimActor.act()` suspends and resumes many times mid-body. This fits the **meso** subset well (a traversal *is* just `Enter → Ready → Release`, event-routine style) but not the micro tick loop. Since the formal target is the meso semantics, that's acceptable — say so.
