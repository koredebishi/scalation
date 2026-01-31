# ScalaTion Traffic Simulation - Claude Instructions

## Project Overview
Microscopic traffic simulation using ScalaTion 2.0 framework (Scala 3). Simulates US-101 corridor (CalRoute101_2 model) with IDM car-following dynamics integrated via Dormand-Prince ODE solver, validated against PeMS sensor data.

## Rules

### Project-Specific
- **Always ask permission before writing or modifying code**
- **Do not plagiarize** - all paper text must be original
- Simulation may be running on HPC - don't interrupt with code changes unless asked
- Use ScalaTion 2.0 style: Scala 3, `cfor` loops, `VectorD`/`MatrixD`
- Do not hallucinate - if unsure, say so

### Behavioral Guidelines

**1. Think Before Coding** — Don't assume. Don't hide confusion. Surface tradeoffs.

Before implementing:
- State your assumptions explicitly. If uncertain, ask.
- If multiple interpretations exist, present them - don't pick silently.
- If a simpler approach exists, say so. Push back when warranted.
- If something is unclear, stop. Name what's confusing. Ask.

**2. Simplicity First** — Minimum code that solves the problem. Nothing speculative.

- No features beyond what was asked.
- No abstractions for single-use code.
- No "flexibility" or "configurability" that wasn't requested.
- No error handling for impossible scenarios.
- If you write 200 lines and it could be 50, rewrite it.
- Ask yourself: "Would a senior engineer say this is overcomplicated?" If yes, simplify.

**3. Surgical Changes** — Touch only what you must. Clean up only your own mess.

When editing existing code:
- Don't "improve" adjacent code, comments, or formatting.
- Don't refactor things that aren't broken.
- Match existing style, even if you'd do it differently.
- If you notice unrelated dead code, mention it - don't delete it.

When your changes create orphans:
- Remove imports/variables/functions that YOUR changes made unused.
- Don't remove pre-existing dead code unless asked.
- The test: Every changed line should trace directly to the user's request.

**4. Goal-Driven Execution** — Define success criteria. Loop until verified.

Transform tasks into verifiable goals:
- "Add validation" → "Write tests for invalid inputs, then make them pass"
- "Fix the bug" → "Write a test that reproduces it, then make it pass"
- "Refactor X" → "Ensure tests pass before and after"

For multi-step tasks, state a brief plan:
1. [Step] → verify: [check]
2. [Step] → verify: [check]
3. [Step] → verify: [check]

Strong success criteria let you loop independently. Weak criteria ("make it work") require constant clarification.

**Success indicators:** Fewer unnecessary changes in diffs, fewer rewrites due to overcomplication, clarifying questions come before implementation rather than after mistakes.

## Papers

### ANNSIM 2026 — ✅ SUBMITTED
- **Submitted:** January 11, 2026
- **Title:** "HIGHER-ORDER IDM INTEGRATION FOR MICROSCOPIC TRAFFIC SIMULATION: A DORMAND-PRINCE APPROACH WITH MULTI-SCALE VALIDATION"
- **Location:** `docs/2026_annsim_paper_2026/bishi_annsim2026.tex`

### WSC 2026 — 🔄 IN PROGRESS
- **Deadline:** April 5, 2026 (Contributed Papers) — **66 days from Jan 29**
- **Notification:** May 25, 2026
- **Camera-ready:** June 26, 2026
- **Page limit:** 12 pages max (150-word abstract)
- **Location:** `docs/wsc_2026/` (TBD)

#### Action Plan (66 days)

| Phase | Dates | Days | Tasks | Deliverable |
|-------|-------|------|-------|-------------|
| **1. Setup** | Jan 29 - Feb 2 | 5 | Smoke test Krauss, create experiment runner, set up HPC batch scripts | Working 3-model pipeline |
| **2. Experiments** | Feb 3 - Mar 2 | 28 | Run all CF×optimizer combinations on HPC (30-day wall time) | Raw results |
| **3. Analysis** | Mar 3 - Mar 16 | 14 | Process results, build tables, generate figures | Results section draft |
| **4. Writing** | Mar 17 - Mar 30 | 14 | Full paper draft (intro, related work, methodology, results, conclusion) | Complete draft |
| **5. Polish** | Mar 31 - Apr 5 | 5 | Internal review, revisions, formatting, submission | Submitted paper |

#### Phase 1 Checklist (Jan 29 - Feb 2)
- [ ] Smoke test `KraussDynamics` — run one simulation
- [ ] Smoke test `GippsDynamics` — verify still works
- [ ] Create experiment configuration (CF model × optimizer matrix)
- [ ] Create HPC batch scripts for 30-day runs
- [ ] Set up results collection directory structure

#### Phase 2 Checklist (Feb 3 - Mar 2)
- [ ] Submit HPC jobs for all combinations
- [ ] Monitor job progress weekly
- [ ] Collect intermediate results if available

#### Phase 3 Checklist (Mar 3 - Mar 16)
- [ ] Parse all result logs
- [ ] Compute lane-level MAPE for each combination
- [ ] Build comparison tables (CF model × optimizer × lane)
- [ ] Generate figures (convergence plots, bar charts)
- [ ] Identify best-performing combination

#### Phase 4 Checklist (Mar 17 - Mar 30)
- [ ] Write Introduction (motivation, contribution)
- [ ] Write Related Work (CF models, calibration literature)
- [ ] Write Methodology (models, optimizers, validation approach)
- [ ] Write Experimental Setup (US-101, PeMS data, HPC)
- [ ] Write Results (tables, analysis, discussion)
- [ ] Write Conclusion (findings, limitations, future work)

#### Phase 5 Checklist (Mar 31 - Apr 5)
- [ ] Internal review with advisor
- [ ] Address feedback
- [ ] Format to WSC template
- [ ] Final proofread
- [ ] Submit by April 5

#### Research Question
Which car-following model + optimizer combination produces the best lane-level flow and speed validation against PeMS data?

#### Experimental Design

**CF Models (3):**
| Model | Type | Key Feature |
|-------|------|-------------|
| IDM | Acceleration-based | Continuous, ODE-derived |
| Gipps | Velocity-based | Discrete, deterministic |
| Krauss | Velocity-based | Discrete, stochastic (σ=0.5) |

**Optimizers (2-3):**
- SPSA — stochastic gradient-free
- Nelder-Mead — simplex method
- (Optional) DE — differential evolution

**Integrator:** Ballistic for all models (ANNSIM 2026 showed no significant difference)

**Validation Metrics:**
- Per-lane flow (veh/hr) vs PeMS
- Per-lane speed (mph) vs PeMS
- Aggregate metrics for literature comparison
- RMSE, MAPE

#### HPC Plan
- Cluster: Sapelo2
- Wall time: 30 days
- Experiment matrix: 3 models × 2-3 optimizers × N runs

#### Key Citations
- \cite{ANNSIM_2026} — Ballistic equivalent to DOPRI5 for this application

## Key Files

### Simulation Model
- `src/main/scala/scalation/simulation/process/example_1/CalRoute101_2.scala` - Main simulation model
- `src/main/scala/scalation/simulation/process/Vehicle.scala` - Vehicle properties and CF parameters
- `src/main/scala/scalation/simulation/process/Dynamics.scala` - CF models: IDM, Gipps, Krauss (Ballistic default)
- `src/main/scala/scalation/simulation/process/VSource.scala` - Vehicle source with Erlang2S arrivals
- `src/main/scala/scalation/simulation/process/MultiVSource.scala` - Multi-lane vehicle source

### Calibration
- `src/main/scala/scalation/simulation/process/CalibrateCalRoute101.scala` - Optimization entry points (SPSA, SPSA_Mo, Nelder-Mead, GA, DE)
- `src/main/scala/scalation/simulation/process/TrafficConfig2.scala` - PeMS data loading and validation
- `src/main/scala/scalation/simulation/process/TrafficOptimization.scala` - Objective function wrapper

### ODE Solver
- `src/main/scala/scalation/dynamics/DormandPrince.scala` - DOPRI5 integrator (`integrateVV` method)
- Butcher tableau coefficients: lines 37-65
- 7-stage RK evaluation: lines 103-109

### Random Variates
- `src/main/scala/scalation/random/Variate.scala` - Contains `Erlang2S` shifted arrival distribution

## IDM Parameters
Order: `[s, amax, bmax, T, rt]`
- `s` - minimum gap (meters) - paper: $s_0$
- `amax` - max acceleration (m/s²) - paper: $a_{\max}$
- `bmax` - max deceleration (m/s², negative) - paper: $b$
- `T` - safe time headway (seconds) - paper: $T$
- `rt` - reaction time τ (seconds) - paper: $\tau$

### Known Good Starting Point
```scala
VectorD(5.0, 4.0, -2.0, 3.0, 0.5)
```

### Parameter Bounds for Optimization
```scala
val lower = VectorD(2.0, 1.5, -3.0, 1.0, 0.3)
val upper = VectorD(8.0, 6.0, -1.0, 5.0, 1.5)
```

## Code-to-Paper Variable Mapping
| Code Variable | Paper Symbol | Meaning |
|---------------|--------------|---------|
| `car.t_disp` | $x_n$ | cumulative displacement from origin |
| `car.velocity` | $v_n$ | current velocity |
| `car.vmax` | $v_{\max}$ | desired/maximum velocity |
| `amax` | $a_{\max}$ | maximum acceleration |
| `bmax` | $b$ | comfortable deceleration (negative in code) |
| `s` | $s_0$ | minimum gap |
| `T` | $T$ | desired time headway |
| `rt` | $\tau$ | reaction time |
| `del` | $\delta$ | acceleration exponent (typically 4) |

## HPC (Sapelo2)

### Directories
- Home (git): `/home/krb84578/scalation_2.0`
- SPSA runner: `/scratch/krb84578/workDir/scalation_2.0`
- Nelder-Mead/MO runner: `/scratch/krb84578/workDir/scalation_MO`

### Commands
```bash
# Load Java
module load Java/21.0.5

# Compile
cd /scratch/krb84578/workDir/scalation_2.0
sbt clean package

# Submit jobs
sbatch run_SPSA.sbatch
sbatch run_NelderMead.sbatch

# Monitor
tail -f log/spsa/spsa_*.out
tail -f log/nm/nm_*.out
squeue -u krb84578

# Kill job
scancel <job_id>
```

## PeMS Data
- Location: `data/Mainline_VDS_Donald_Doyle/` (5 mainline sensors)
- Ramps: `data/Ramps_VDS_Donald_Doyle/` (2 on-ramp sensors)
- Format: 48 rows (15-min intervals, 6AM-6PM), 4 lanes per sensor
- Columns: timestamp, L1_flow, L1_speed, L2_flow, L2_speed, ...

## Fitness Function
- Metric: Normalized RMSE (NRMSE) - scale invariant
- Formula: `0.5 * avgCountNRMSE + 0.5 * avgSpeedNRMSE`
- Lower is better

## Known Issues
- `ERROR @ Variate.init: parameter tau must be less than mu` - Expected from ramp Erlang2S, not a problem
- `Skipping Source actor due to time limit` - Happens near end of simulation window, normal
- Sensor 4 lanes 2-3 have anomalous PeMS data - flagged in validation

## AI Disclosure
Disclosed in ANNSIM 2026 submission per SCS policy:
> "We used Claude to assist with drafting portions of Section 3 (Methodology) and structuring the manuscript."

