# Off-Ramp Exit Design — ✅ IMPLEMENTED

## Plain English

> "At spawn, each vehicle rolls a dice to decide if it exits at an off-ramp.  
> If yes, it knows which ramp. When it gets close, it moves toward the exit lane.  
> At the diverge point, it takes the ramp. Everyone else drives to the Sink."

## What Was Implemented (April 20, 2026)

### Files Changed (4 files, ~60 lines new code)

| File | Change |
|------|--------|
| `Vehicle.scala` | Added `var exitRampId: String = null` — off-ramp destination flag |
| `Route.scala` | Added `case class OffRampSpec`, `object OffRampSpec.assignExit()`, `Route.exitCheckAndSteer()`, `var offRampSpecs` on Route |
| `VTransport.scala` | Added exit steering call after MOBIL block in `move()` |
| `EatonFireModel.scala` | Built `offRampSpecs210/134`, wired into routes, dice-roll at spawn, off-ramp exit in `driveHighway` |

### How It Works

1. **`OffRampSpec`** — data class: `(rampId, divergeSeg, exitLane, turnoffPct)`
2. **`OffRampSpec.assignExit(specs, rng)`** — dice roll at spawn → returns rampId or null
3. **`Route.exitCheckAndSteer(car, seg, clock)`** — mandatory lane change toward exit lane when within 500m
4. **`driveHighway`** — at diverge segment, if car is in exit lane → drives off-ramp → leaves at off-ramp sink
5. **Missed exit** — if car passes diverge in wrong lane → `exitRampId = null` (becomes through traffic)

### Urgency Zones (in exitCheckAndSteer)
- `> 500m`: MOBIL handles it (discretionary)
- `200–500m`: Mandatory lane change, 2s cooldown
- `< 200m`: Aggressive, 1s cooldown
- `at diverge + exit lane`: Take ramp
- `at diverge + wrong lane`: Missed exit → through traffic

