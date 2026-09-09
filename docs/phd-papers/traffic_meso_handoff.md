# Traffic-to-meso handoff — 2026-09-05

## Current state

- Branch: testing-animation. Latest checkpoint discussed: 308139987.
- Shared accounting/reporting and IDM-default alignment changes remain in the working tree. This note is not a new Git checkpoint.
- User ran eval successfully with IDM [s, amax, bmax, T, rt] = [2, 4, -2, 1, 0.5], Ballistic, headless.
- Saved output prefix: log/simulation/run_1788660888901_9e31e1fd-6e66-4b83-9729-59f177a3d423.
- Generated and completed: 59,750 each. Ramp admissions: 2,815 and 872. Mean travel time: 1,135.64 seconds. Final simulated time: 48,154.82 seconds.
- Flow NRMSE*100: 3.4761; speed: 32.6426. Fitness 36.5700 is weighted flow/speed plus a flow penalty, despite its average label.
- Sampled delay is concentrated in lane index 1: 19,714 mainline generations, 17,720 sensor-1 passages within 12 hours, and 17,653 sensor-5 passages within 12 hours. Full-run lane-index-1 sensor counts are 19,712.
- Current lane changing is attempted once before each segment when the leader is below 10% of vmax; it is not MOBIL. Route.changeLane transfers DLL membership but not active segment deque membership. Mid-segment changes would require coordinated movement/occupancy/animation updates.
- User paused lane-change/MOBIL work. No decision to implement it.

## Next direction

- Read final_micro_to_meso_blueprint.md, including “A Starting point maybe” at line 1709.
- User controls paper division; do not decide the split.
- Start with one event-sparse mesoscopic link in simulation/process/meso, sharing Model, SimActor, and Vehicle.
- Proposed initial files from the blueprint: MesoTransport.scala and example_1/MesoOneLinkTest.scala.
- First behavior: Source -> MesoTransport -> Sink; free-flow readiness L/vf, capacity-limited FIFO release, occupancy and vehicle conservation, activation counts.
- Capacity units must be explicit: h = 1/C for veh/s, or 3600/C for veh/h.
- One-link correctness is a first milestone, not completion of the full Gate 1 fidelity evaluation.
- Later: two links, finite storage/blocking/wakeup, then node/movement composition. Mixed-resolution boundaries, MesoSpec, LLM and Maude follow later.
- Do not start by changing Route/Pathway or using simulation/event's separate scheduler/entities.
- Equal-time ordering remains a known design issue; local FIFO must still be defined for the first prototype.
- Current instruction: no code yet. Do not compile or execute simulations without updated authorization.
