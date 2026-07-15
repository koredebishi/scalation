# Off-Ramp Exit Design

## Plain English

> "At spawn, each vehicle rolls a dice to decide if it exits at an off-ramp.  
> If yes, it knows which ramp. When it gets close, it moves toward the exit lane.  
> At the diverge point, it takes the ramp. Everyone else drives to the Sink."

## Architecture — 3 Pieces

### 1. Vehicle Flag (engine-level)

In `Vehicle.scala`:
```
var exitRampId: String = null   // null = through traffic → drives to Sink
```
- Set once at spawn, never changes
- Generic — no model name, no hardcoded ramp ID
- Any model's vehicles can carry this

### 2. Off-Ramp Config (config-level)

A simple case class, lives in the config layer (e.g., `CorridorLayout` or new `OffRampConfig`):
```scala
case class OffRampSpec(
    rampId:       String,    // matches a Junction/Sink name, e.g., "off_R26"
    junctionIdx:  Int,       // which junction along the Route this ramp branches from
    exitLane:     Int,       // which lane the vehicle must be in to exit (usually 0 or nLanes-1)
    turnoffPct:   Double     // fraction of mainline vehicles that exit here (0.0–1.0)
)
```

**Where the percentages come from:**
- **PeMS data**: off-ramp detector flow / mainline detector flow = turn-off fraction
- **Synthetic mode**: hardcode reasonable values (0.10–0.20 per ramp)
- **Fire-day mode**: different percentages (near-zero for ramps behind the fire, high for evacuation routes)

The model passes `List[OffRampSpec]` when building the Route. The Route stores it.

### 3. Exit Logic in VTransport.move() (engine-level)

Inside `VTransport.move()`, right after the MOBIL check:

```
// Existing:
1. updateV()                    // IDM acceleration
2. MOBIL discretionary check    // "is the other lane better?"

// NEW — after MOBIL:
3. EXIT CHECK:
   a. Does car.exitRampId match an upcoming off-ramp on this Route?
   b. How far away is the diverge junction? (segments remaining × segment length)
   c. If within mandatory-change zone (~500m):
      - Override MOBIL → mandatory lane change TOWARD exitLane
      - Use tighter gap acceptance (smaller safe gap threshold)
   d. At the diverge junction (segment boundary):
      - If car is in exitLane → route onto ramp Pathway
      - If car missed it → car.exitRampId = null (becomes through traffic)
```

### Assignment at Spawn

When a Source creates a vehicle:
1. Draw `u ~ Uniform(0, 1)`
2. Walk through the Route's `List[OffRampSpec]` in order:
   - Accumulate `turnoffPct` values
   - If `u < cumulative` → assign `car.exitRampId = spec.rampId`, break
3. If no ramp matched → `car.exitRampId = null` (through traffic)

This lives in the Source/arrival logic, NOT in the Vehicle constructor.

## Mandatory Lane Change vs MOBIL

| Aspect | MOBIL (discretionary) | Mandatory exit |
|--------|----------------------|----------------|
| Trigger | Incentive criterion | Distance to exit ramp |
| Direction | Either lane | Only toward exit lane |
| Gap tolerance | Normal (b_safe) | Tighter (more aggressive) |
| Cooldown | 3 seconds | 1 second (urgent) |
| Failure | Stay in lane (no cost) | Miss exit → become through traffic |

The mandatory zone has **urgency levels**:
- `> 500m` from exit: MOBIL handles it (discretionary, may drift toward exit lane naturally)
- `200–500m`: Mandatory — override MOBIL, always move toward exit lane when gap exists
- `< 200m`: Aggressive — accept smaller gaps, signal to followers
- `at diverge`: If in exit lane → take ramp. If not → missed exit.

## What Changes in Each File

| File | Change | Size |
|------|--------|------|
| `Vehicle.scala` | Add `var exitRampId: String = null` | 1 line |
| `Route.scala` | Store `offRampSpecs: List[OffRampSpec]` | ~5 lines |
| `VTransport.scala` | Exit check after MOBIL block in `move()` | ~25 lines |
| Config (new or existing) | `case class OffRampSpec(...)` | ~5 lines |
| Model (e.g., EatonFireModel) | Pass `OffRampSpec` list when building Route | ~10 lines |
| Source/arrival logic | Dice roll → assign `exitRampId` at spawn | ~15 lines |

**Total: ~60 lines of new code.**

## What This Does NOT Do

- No route recalculation (vehicle doesn't "plan" — it just has a destination)
- No multi-ramp decision (vehicle exits at its assigned ramp or nowhere)
- No dynamic rerouting (fire blocks a ramp → vehicle doesn't know)
- Those are future features (Study 3: Agentic Architecture)

## Dependencies

- Needs off-ramp Pathways to exist in the Route (they may already be Junction branches)
- Needs PeMS ramp data for real percentages (synthetic is fine for now)
- MOBIL must yield to mandatory override when `car.exitRampId != null` and close to exit (no extra flag — `exitRampId` serves double duty)

