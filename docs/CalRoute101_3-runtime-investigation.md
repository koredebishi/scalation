# CalRoute101_3 runtime investigation — September 5, 2026

The current simulation finished headless in **466.4 seconds (7m 46s)**. The August 23 simulation phase finished in approximately two minutes, then hung during coroutine shutdown. The exact reported 15-minute versus six-hour timings were **not reproduced**. The measurements identify both increased simulation work and an animation memory problem; they do not validate the corridor's traffic behavior.

## Checkpoints and method

- `9ece3245c` — August 23 baseline, the first commit in the two-week window.
- `f2cc38f20` — September 3, including spacing diagnostics and the collision clamp.
- `4176c1642` — checkpoint of the user's previously uncommitted source and documentation. IDE state, generated images, and compiled artifacts were excluded from this commit.

Source snapshots live under `target/runtime-investigation`. Identical counters measure actor activations, movement updates, stationary updates, creations and sink exits. A stationary update means displacement did not increase; it does not necessarily mean the stored velocity was zero. Animation is disabled only for headless runs. Demand files are shared and were unchanged across these revisions. Each snapshot preserves its own dynamics and default random initialization; results are single runs, not statistical estimates.

All snapshots used the existing Scala 3.8.2 compiler, Java 23.0.2, current dependencies, and a 4 GB maximum Java heap. Compilation is excluded from timing. Build settings are temporary sbt-session settings; `build.sbt` and production Scala source were not edited. Compilation and timed simulation phases were run sequentially.

## Results

| Run | Measured result |
| --- | --- |
| August 23, headless | Last progress sample: 110.3 s wall, 41,728 s simulated, 57,656 exits. Simulation subsequently completed and entered cleanup. Exact completion time was not instrumented in this first run. |
| Current, headless | Full simulation: **466.4 s**, plus 0.3 s setup. Last progress sample: 133.87 million activations, 133.40 million movement updates, 100.72 million stationary updates (**75.5%**). |
| August 23, animated, 120 s limit | 36,701 simulated seconds (**10.2 h**), 50,183 exits, 427 active vehicles. |
| Current, animated, 120 s limit | 13,012 simulated seconds (**3.6 h**), 16,236 exits, 845 active vehicles. |
| September 3, headless, 120 s limit | 30,494 simulated seconds (**8.5 h**), 40,444 exits, 343 active vehicles; **935,234 negative-gap messages**, 291,797,312 bytes in the PowerShell-captured log (UTF-16). |

At approximately 27,000 simulated seconds, the old headless run needed 60.1 wall seconds and 11.68 million activations. The current headless sample needed approximately 180 wall seconds and 47.65 million activations to reach 27,287 simulated seconds. The slowdown therefore includes substantially more events, not just slower execution of each event.

The August 23 run's last sample had 1.7% stationary movement updates. September 3's capped run had 4.4%. The checkpoint's large increase in queue-related work appears after September 3, in the previously uncommitted changes. Comparing the August 23 baseline through August 30 found no changes to the movement/dynamics implementation; changes in that interval principally added map loading, which is skipped headless.

## Why runtime grows

1. **Animation retains an unbounded command queue.** `Model.animate` enqueues commands during simulation, but `Model.fini` starts playback only after simulation. `VTransport.move` emits a move command on every unfinished tick, including stationary ticks. The current run therefore generates on the order of 100 million animation commands if animation is enabled. During its animated sample, Java heap usage was already about **1.5 GB around 50 seconds** (a `jcmd GC.heap_info` sample). Playback subsequently also retains commands in `DgAnimator.frames`. This is a strong memory-pressure mechanism, although a six-hour animated completion was not attempted.
2. **September 3 added diagnostic flooding.** `Dynamics.updateM` prints every negative-spacing tick. The checkpoint replaces this with transition-only diagnostics. The measured logging cost belongs to September 3, not to the checkpoint's quieter implementation.
3. **The checkpoint changes which traffic can move.** Earlier dynamics use inconsistent total-distance origins across merges and/or allow negative-spacing pairs to take the free-flow branch. September 3 adds corridor-relative spacing and a position clamp, but still allows negative-spacing free flow. The checkpoint repairs joining/leader lookup, brakes negative-spacing pairs and prevents backward displacement. It also changes `Vehicle` defaults from `T=3, s=5` to `T=1, s=2`. These are behavior changes, so the faster old result is not evidence of equivalent physics. Individual checkpoint changes have not been separately ablated.
4. **The HUD adds work but is secondary evidence.** Its 50-activation refresh existed before the window. September 3 changes a single vehicle scan to two scans per refresh. More queued vehicles make both versions' refreshes more expensive.

## Why “the true corridor simulation” is not established

- Mainline `joinAt(..., 0)` and ramp merging insert vehicles without an admission gap check. Fixing movement overlap does not fix overlapping insertion; source backlog should be represented upstream rather than stacked at position zero.
- Lane changes are attempted once per segment, before blocking in `move()`. A vehicle stopped mid-segment cannot try another lane until it completes that segment.
- PeMS observed entry speed also becomes each vehicle's lifetime desired-speed cap. That assumption can prolong queues; it predates the measured regression and needs validation rather than arbitrary retuning.
- `CalRoute101_3` constructs segment lengths from animation curves; its construction path does not assign physical segment lengths. `Coordinates` scales GPS coordinates to the requested display size. This pre-existing coupling needs verification before treating geometry or density as physical ground truth.

These are code findings and modeling concerns, not measured proof that every observed queue is artificial. A validity check needs safe insertion, ordered leaders, physical lengths, and observed versus simulated detector flow/travel-time comparisons. Short runtime alone is also insufficient.

## Separate shutdown hang

The August 23 thread dump shows the main thread waiting in `Coroutine.shutdown`, joining a virtual thread after traffic processing and cleanup finished. `Model.act` skips sources after the time limit without terminating the skipped actor; this is a plausible cause of a stranded coroutine, not yet individually proven. The current run also remained alive after printing its completed wall time. Owned benchmark processes were stopped after collecting results. The benchmark Java wrapper now has an independent timeout so shutdown hangs cannot leave future runs running indefinitely.

## Next implementation steps

1. Make headless execution selectable, and bound/sample animation recording independently of simulation ticks. Avoid queuing stationary duplicates. Preserve the car-following calculations while measuring the performance improvement.
2. Add safe source admission and safe ramp merge handling, then validate leader ordering and queue discharge. Do not restore drive-through behavior just to recover the old runtime.
3. Fix skipped-source termination and verify all coroutines finish.

## Reproduce and inspect

From the repository root, use `scripts/prepare-runtime-benchmark.ps1 -Revision <sha> -Label <label>`, then `scripts/compile-runtime-benchmark.ps1 -Label <label>`, then `scripts/run-runtime-benchmark.ps1 -Label <label> -Seconds 180`. Add `-Animated` for the display-enabled comparison and use `-RunName` to preserve previous logs. The run script reads dependency jar paths from `benchmark-classpath.log`, obtained with `sbt "show Runtime / fullClasspath"`. The preparation script now also emits an exact `[probe-complete]` record for future completed runs.

Raw evidence (ignored build artifacts): `target/runtime-investigation/current/run.log`, `current/sample-180s.log`, `current/animated.log`, `aug23/run.log`, `aug23/animated.log`, `aug23/threads.txt`, and `sep03/headless.log`. Exit 124 means the configured measurement/watchdog limit, not a successful full simulation.
