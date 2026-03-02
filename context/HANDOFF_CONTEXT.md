# Agent Handoff Context — February 19, 2026

**Read this entire file before doing anything.** This is a complete state dump of an ongoing PhD research project. The student (Korede Bishi) is a CS PhD student at UGA working in Discrete Event / Discrete Time Simulation with traffic simulation as the application domain.

---

## CRITICAL RULES
1. **Always ask permission before writing or modifying any code.**
2. Do not hallucinate. If unsure, say so.
3. Do not plagiarize — all paper text must be original.
4. Read `context/rules.md` for behavioral guidelines, `context/scalation-style.md` for coding style.

---

## WHO IS KOREDE BISHI

- PhD student in Computer Science, University of Georgia
- Advisor: Dr. John A. Miller
- Track: Simulation and Analytics
- Extending the **ScalaTion 2.0** framework (Scala 3) — this is Dr. Miller's framework, not Korede's
- Korede's contributions: lane-level validation infrastructure, multi-level fitness functions, HPC deployment pipeline, route abstraction, ramp modeling
- Currently preparing for PhD candidacy exam
- Taking Transportation Planning (Spring 2026)

---

## ACTIVE HPC EXPERIMENT (Job 42800445)

**Status as of Feb 18, 2026 ~19hrs runtime:** All 20 jobs running on Sapelo2 (GACRC).

### Experiment Design
2 car-following models (IDM, Gipps) × 5 optimizers (SPSA, SPSA_Mo, NelderMead, GA, DE) × 2 fitness levels (MACRO corridor-level, MICRO lane-level) = **20 jobs**

Krauss was **removed** from this experiment to save compute time.

### Array Index Mapping
| Index | Model | Optimizer | Fitness | Expected Runtime |
|-------|-------|-----------|---------|-----------------|
| 1 | IDM | SPSA | MACRO | ~10 hrs |
| 2 | IDM | SPSA_Mo | MACRO | ~10 hrs |
| 3 | IDM | NelderMead | MACRO | ~15 hrs |
| 4 | IDM | GA | MACRO | ~24 hrs |
| 5 | IDM | DE | MACRO | ~24 hrs |
| 6 | Gipps | SPSA | MACRO | ~8 hrs |
| 7 | Gipps | SPSA_Mo | MACRO | ~8 hrs |
| 8 | Gipps | NelderMead | MACRO | ~12 hrs |
| 9 | Gipps | GA | MACRO | ~24 hrs |
| 10 | Gipps | DE | MACRO | ~24 hrs |
| 11 | IDM | SPSA | MICRO | ~10 hrs |
| 12 | IDM | SPSA_Mo | MICRO | ~10 hrs |
| 13 | IDM | NelderMead | MICRO | ~15 hrs |
| 14 | IDM | GA | MICRO | ~24 hrs |
| 15 | IDM | DE | MICRO | ~24 hrs |
| 16 | Gipps | SPSA | MICRO | ~8 hrs |
| 17 | Gipps | SPSA_Mo | MICRO | ~8 hrs |
| 18 | Gipps | NelderMead | MICRO | ~12 hrs |
| 19 | Gipps | GA | MICRO | ~24 hrs |
| 20 | Gipps | DE | MICRO | ~24 hrs |

### Iteration Counts
- SPSA, SPSA_Mo: 100 iterations (these finish fast, ~2 evals per iteration = 200 evals)
- NelderMead: ~200 evals (internal stopping)
- GA: 40 generations × 20 population = 800 evals
- DE: 40 generations × 20 population = 800 evals

### Completed Results (as of ~19hrs in)

**Macro reports written:**
- gipps_spsa_macro, gipps_spsa_mo_macro, gipps_neldermead_macro, gipps_ga_macro ✅
- idm_spsa_macro, idm_spsa_mo_macro, idm_neldermead_macro, idm_ga_macro ✅

**Micro reports written:**
- gipps_spsa_micro, gipps_spsa_mo_micro, gipps_neldermead_micro, gipps_ga_micro ✅
- idm_spsa_micro, idm_spsa_mo_micro, idm_neldermead_micro ✅

**Still running (DE jobs — need ~24hrs):**
- Jobs 5, 10, 15, 20 (all DE variants)

### Latest Fitness Values (NRMSE*100, lower = better)

**MACRO-calibrated:**
| Model | SPSA | SPSA_Mo | NelderMead | GA | DE |
|-------|------|---------|------------|----|----|
| IDM | 2.833 | 2.911 | 2.048 | 1.762 | running |
| Gipps | 3.646 | 3.754 | 2.252 | 1.709 | running |

**MICRO-calibrated (report fitness is always macro-computed):**
| Model | SPSA | SPSA_Mo | NelderMead | GA | DE |
|-------|------|---------|------------|----|----|
| IDM | 2.284 | 2.306 | 2.046 | 6.929 | running |
| Gipps | 2.651 | 2.651 | 2.252 | 1.711 | running |

**Key observation:** MICRO-calibrated jobs (IDM) show different optimal parameters than MACRO-calibrated. Micro fitness values during optimization were 6-10+ (because lane-level NRMSE is naturally higher), but the final report always uses macro fitness.

### Best Parameters Found

**IDM MACRO NelderMead (fitness 2.048):**
`VectorD(1.00000, 3.42283, -0.500000, 0.800000, 1.43774)`

**Gipps MACRO GA (fitness 1.709):**
`VectorD(3.76474, 3.50749, -6.39432, 1.00262, 4.79738)`

**IDM MICRO NelderMead (fitness 2.046):**
`VectorD(1.00000, 3.52255, -0.500000, 0.800000, 1.51662)`

### Important Note on Fitness Reporting
The optimization fitness (what optimizer minimizes) differs between MACRO and MICRO modes:
- MACRO jobs: minimize `validate().fitness` (corridor-level 0.3*flow + 0.7*speed NRMSE)
- MICRO jobs: minimize `microLevelFitness()` (lane-level, same weighting per lane)
- But the **report** always computes and prints the macro-level fitness via `report.report()` → `validate()`
- Both macro AND micro metrics are printed in every report (see the report format below)

### Report Format
Each report contains both macro and micro tables:
```
MACRO-LEVEL VALIDATION (Sensor Aggregates)
Sensor  Flow_R²  Flow_MAE  Flow_RMSE  Flow_Mean  Flow_NRMSE*100  Speed_R²  Speed_MAE  ...

MICRO-LEVEL VALIDATION (Lane Detail)  
Sensor  Lane  Flow_R²  Flow_MAE  Flow_RMSE  Flow_Mean  Flow_NRMSE*100  Speed_R²  Speed_MAE  ...

SUMMARY
Average Flow NRMSE*100:  X.XXXX
Average Speed NRMSE*100: X.XXXX  
Fitness (avg NRMSE*100): X.XXXX   ← This is always the macro fitness
```

Reports and CSVs are at: `/scratch/krb84578/workDir/scalation_2.0/log/simulation/`
Job logs are at: `/scratch/krb84578/workDir/scalation_2.0/log/calibration_42800445_*.out`

---

## PREVIOUS EXPERIMENT (Job 42787423 — Old Run, Cancelled)

The previous run used 15 jobs (3 models including Krauss × 5 optimizers, MACRO only). Those results showed:
- IDM SPSA: fitness 3.159, params VectorD(2.00, 1.50, -1.21, 1.20, 0.60)
- Gipps NelderMead: fitness 3.616, params VectorD(2.00, 2.84, -3.00, 1.70, 1.50)
- Krauss models had fitness 3.6-3.7 (similar to Gipps but much slower, hence removed)
- GA and DE were running 250+ hours and were cancelled

Old reports (without macro/micro suffix) still exist in the simulation directory.

---

## THREE PAPERS

### Paper 1: ANNSIM 2026 — ✅ SUBMITTED (Jan 2026)
- **Title:** "Beyond Corridor Averages: Lane-Level Validation of Microscopic Freeway Simulation with Data-Driven Arrivals"
- **Key findings:** Integrator choice <1% impact, Erlang-2 arrivals reduce flow error ~28%, lane-level validation reveals hidden dynamics
- **File:** `docs/2026_annsim_paper_2026/bishi_annsim2026.tex`

### Paper 2: WSC 2026 — 🔄 IN PROGRESS (Target: April 5, 2026)
- **Title:** "Comparative Analysis of Car-Following Models and Optimization Algorithms for Multi-Lane Traffic Simulation Calibration"
- **Core contribution:** IDM vs Gipps × 5 optimizers, MACRO corridor-level fitness, HPC results from Sapelo2
- **Current best result:** Gipps+GA job 42800445 fitness ~4.94 (NRMSE×100)
- **Critical note:** The 1.71 fitness figure from the handoff notes is from an older run with different weighting — do NOT use it as a current result
- **Status:** HPC experiments done (job 42800445 and 43050887). Job 43050887 was a worse run (tighter bounds caused boundary hugging). Use job 42800445 results for the paper.
- **Research questions:**
  1. Which CF model (IDM, Gipps) best reproduces multi-lane dynamics?
  2. Which optimizer most effectively calibrates model parameters?
  3. How does fitness weighting (flow vs speed) affect convergence?

### Paper 3: WSC 2026 — 🔄 PROPOSED (Target: April 5, 2026)
- **Title:** "Evaluating Evacuation Resilience Under Wildfire Disruption: A PeMS-Calibrated Microscopic Simulation of I-10 During the 2025 Palisades Fire"
- **Track:** WSC 2026 — Simulation for Climate Resilience (special track)
- **Core contribution:** Wildfire evacuation reconstruction + contraflow counterfactual using ScalaTion DES
- **Status:** Not yet implemented. Study design complete (see Study 3 section above).
- **Dependency:** Requires PeMS data pull for I-10 corridor and baseline calibration before any implementation

---

## PhD DISSERTATION — THREE STUDIES

**NOTE: The LLM-orchestrated causal calibration proposal was NOT accepted by the advisor. It is stale and should be ignored.**

The dissertation now comprises three studies:

### Study 1 — Structural Sensitivity Analysis ✅ SUBMITTED (ANNSIM 2026)
- Integrator choice: <1% impact. Shifted Erlang-2 arrivals: ~28% flow improvement.
- Establishes: arrival processes govern flow; CF parameters govern speed.

### Study 2 — Constrained Calibration Framework 🔄 IN PROGRESS (WSC 2026)
- 2 CF models (IDM, Gipps) × 5 optimizers (SPSA, SPSA_Mo, NelderMead, GA, DE) × MACRO fitness
- HPC on Sapelo2 (GACRC). Current best: Gipps+GA from job 42800445 (fitness ~4.94)
- WSC 2026 deadline: April 5, 2026

### Study 3 — Wildfire Evacuation & Contraflow Simulation 🔄 PROPOSED (WSC 2026 Climate Resilience Track)
**Title:** "Evaluating Evacuation Resilience Under Wildfire Disruption: A PeMS-Calibrated Microscopic Simulation of I-10 During the 2025 Palisades Fire"

**Primary Research Question:**
> Would directional lane reallocation (contraflow) materially improve evacuation resilience during wildfire-induced demand surge in a dense urban freeway network?

**Scope:**
- Corridor: I-10 eastbound (Santa Monica → I-405)
- Event: January 7, 2025 Palisades Fire — mass evacuation
- Data: Caltrans PeMS lane-level flow, speed, occupancy
- Time window: fire day + 2–3 baseline comparison days

**What it will NOT do:**
- Simulate wildfire physics
- Model full Caltrans operations
- Implement GIS fire-spread modeling
- Attempt real-time adaptive control

**Four Experimental Phases:**
1. Baseline calibration — reproduce normal weekday I-10 dynamics (R², RMSE, SMAPE)
2. Fire-day reconstruction — detect demand surge, inject into calibrated simulator
3. Smoke module — behavioral degradation: v₀↓, T↑, reaction variability↑, lane-change aggressiveness↓ (mild/moderate/severe)
4. Contraflow counterfactual — 4 scenarios: (A) baseline wildfire, (B) +1 reversed lane, (C) full reallocation, (D) contraflow + smoke

**Evaluation Metrics:**
- Throughput (veh/hr)
- Mean speed
- Shockwave speed: w = (q₂ − q₁) / (k₂ − k₁)
- Congestion clearance time
- Resilience Index: R = 1 − (Performance Loss Area / Baseline Area)

**Validation Levels:**
1. Baseline: normal-day PeMS reproduction
2. Fire-day: match observed congestion patterns
3. Structural: shockwave comparison to theory
4. Sensitivity: demand magnitude × smoke severity × lane-change aggressiveness

**Expected Contributions:**
1. Data-calibrated wildfire evacuation reconstruction
2. Smoke-behavior degradation modeling in microscopic DES
3. Counterfactual contraflow evaluation in dense urban freeway
4. Quantitative resilience threshold identification
5. Insight into when capacity expansion is ineffective under visibility degradation

**Software Stack:** ScalaTion (core simulation) + Python (data processing) + LaTeX (manuscript)
**Target:** WSC 2026 — Simulation for Climate Resilience track, April 5, 2026

**4–6 Month Timeline:**
- Month 1: PeMS extraction, baseline calibration
- Month 2: Fire-day reconstruction, smoke module
- Month 3: Contraflow implementation, initial experiments
- Month 4: Sensitivity analysis, validation, writing draft
- Month 5–6 (optional): Refinement, submission polishing

---

## RESEARCH VISION WEBSITE

`docs/RESEARCH_VISION_WEBSITE.md` — A public-facing academic website that the PI has reviewed. The PI requested adding the dissertation proposal to the website. This has NOT been done yet.

The website currently has:
- Research Overview
- Study 1 (ANNSIM) with abstract and findings
- Study 2 (WSC/MASCOTS) with abstract and preliminary findings
- Technical Contributions to ScalaTion table
- Publications list
- About ScalaTion section
- Education and Contact

---

## CANDIDACY EXAM

Files in `docs/candidacy_exam/`:
- `research_summary_A4.tex` — Research summary for committee
- `ANNSIM-CVSE-Presentation.tex` — ANNSIM presentation for exam
- `WSC-CVSE-Presentation.tex` — WSC presentation for exam
- `SCALATION_REFERENCES.md` — Reference list

Committee: Dr. John A. Miller (major), Dr. Maria Hybinette, Dr. Qianwen Li

---

## KEY SOURCE FILES

| File | Purpose |
|------|---------|
| `src/main/scala/scalation/simulation/process/builder/CalibrationFramework.scala` | **MAIN ENTRY POINT** for HPC jobs. Contains `runCalibrationArrayJob` (reads SLURM_ARRAY_TASK_ID), all optimizer runners, `CalibratableModel` trait, `ModelOptimizer`, `CalibrationDefaults` |
| `src/main/scala/scalation/simulation/process/config/SimulationReport.scala` | Validation logic: `validate()`, `microLevelFitness()`, `fitness()`, report generation, CSV export |
| `src/main/scala/scalation/simulation/process/example_1/CalRoute101_2.scala` | Main simulation model (US-101 corridor) |
| `src/main/scala/scalation/simulation/process/Vehicle.scala` | Vehicle properties and CF parameters |
| `src/main/scala/scalation/simulation/process/Dynamics.scala` | CF models: IDM, Gipps, Krauss dynamics |
| `src/main/scala/scalation/simulation/process/CalibrateCalRoute101.scala` | **LEGACY** — old calibration code, entirely commented out |
| `run_CalibrationArray.sbatch` | SLURM batch script for array jobs |

### Parameter Order (all models): `[s, amax, bmax, T, rt]`
- s: minimum gap (meters)
- amax: max acceleration (m/s²)  
- bmax: max deceleration (negative, m/s²)
- T: safe time headway (seconds)
- rt/τ: reaction time (seconds)

### Current Parameter Bounds (expanded Feb 2026)
```
lower: VectorD(1.0, 0.8, -4.0, 0.8, 0.2)
upper: VectorD(8.0, 6.0, -0.5, 5.0, 2.0)
```

---

## HPC ACCESS

- **Cluster:** Sapelo2 (University of Georgia GACRC)
- **User:** krb84578
- **SSH:** `ssh sapelo2` (alias configured)
- **Working dir:** `/scratch/krb84578/workDir/scalation_2.0`
- **Logs:** `/scratch/krb84578/workDir/scalation_2.0/log/`
- **Reports:** `/scratch/krb84578/workDir/scalation_2.0/log/simulation/`
- **Module:** `module load Java/21.0.5`
- **Build:** `sbt assembly` (creates fat JAR)
- **Submit:** `sbatch run_CalibrationArray.sbatch`
- **Monitor:** `squeue -u krb84578`

---

## SYNC TO HPC
Local PowerShell script: `sync_to_sapelo.ps1`

---

## WHAT NEEDS TO HAPPEN NEXT

### Immediate — Paper 2 (WSC Calibration, April 5 deadline)
1. Decide: use job 42800445 results (better run, wider bounds) — NOT job 43050887
2. Build comparison tables: IDM vs Gipps × SPSA/SPSA_Mo/NelderMead/GA/DE fitness and SMAPE
3. Confirm that best results from 42800445 are verifiable and reproducible
4. Draft paper sections — methodology, results, discussion
5. Submit by April 5, 2026

### Immediate — Paper 3 (WSC Wildfire, April 5 deadline)
1. Pull I-10 eastbound PeMS data for January 7, 2025 (fire day) + 2–3 baseline days
2. Identify PeMS stations on I-10 (Santa Monica → I-405 segment)
3. Detect demand surge timing and magnitude from PeMS observations
4. Begin baseline calibration using Study 1–2 methodology on I-10 corridor
5. Implement smoke-behavior degradation module in ScalaTion
6. Run contraflow scenarios (A–D)
7. Write paper targeting WSC Climate Resilience track

### Website
1. Update `docs/candidacy_exam/RESEARCH_VISION_WEBSITE.md`:
   - Remove stale 1.71 fitness claim
   - Remove 19–29% micro/macro claim (not verified)
   - Remove LLM proposal references
   - Update Study 3 to wildfire/contraflow plan
   - Keep Study 2 numbers conservative until paper is finalized

### Candidacy Exam
1. Prepare presentation with actual results from job 42800445
2. Frame three studies as coherent dissertation arc

---

## DOMAIN KNOWLEDGE NOTES

- **bmax = -9.0 or more negative** is extreme emergency braking (Treiber says nearly a stop)
- **Ballistic integrator** is used for all runs (ANNSIM proved integrator choice doesn't matter)
- **PeMS data:** 5 mainline sensors, 4 lanes each, 48 time intervals (15-min, 6AM-6PM)
- **Corridor:** US-101 Northbound, Donald Doyle section, 7 segments, 2 on-ramps
- **Krauss** was removed because: similar fitness to Gipps but 2-3x slower (stochastic model requires more compute)
- **The student's framing:** This is NOT a traffic paper. It's a CS simulation methodology paper that USES traffic as the application domain. Balance CS angle (DES, optimization, validation methodology) with traffic angle (car-following models, PeMS data).

