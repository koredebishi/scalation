# Eaton Corridor — Modeling Notes

## Sensor Role Mapping

| Lane Type | Network Role | Simulation Action |
|-----------|-------------|-------------------|
| ML | Continuation | Track flow along freeway segment |
| OR | Source | Spawn vehicles (surface street → freeway) |
| FR | Sink | Remove vehicles (freeway → surface street) |
| HV | Continuation (Express Lane) | Same as ML; keep separate for HOV lane modeling |
| FF | Junction turning movement | Derive split ratio; no spawn/removal |

## Junction Routing Model

At each freeway-to-freeway interchange (SR-134 ↔ I-210, SR-2 ↔ I-210), use a **time-varying Bernoulli** per vehicle:

$$X_t \sim \text{Bernoulli}(p_t)$$

where:

$$p_t = \frac{\text{FF\_count}(t)}{\text{FF\_count}(t) + \text{downstream\_ML\_count}(t)}$$

- `p_t` is computed per 5-min interval from PeMS data
- Baseline `p_t`: average of Dec 3, 10, 17
- Fire day `p_t`: Jan 7 — expect spike after ~10:30 AM at SR-134/I-210 interchange

## FF Data Files

FF sensor data is stored separately in `pems/eaton-corridor/eaton_corridor_*_FF.csv`.
Each file includes lat/lon from `station_map.csv` for spatial reference.
These files are the **sole source** for computing time-varying junction split ratios (`p_t`).
Do not use the main corridor CSVs for routing decisions — FF rows have been removed from them.

## Fire Day Time Window — Pending Decision

The Eaton Fire ignited at approximately **6:18 PM PST on January 7, 2025** (not morning).
The primary traffic impact signal is expected in the **18:00–23:00** window on that day.

However, all `7thData-FireDay` files currently retain the full **06:00–23:00** window
to preserve the pre-fire baseline within the same day for comparison.

**Decision needed:** Whether to analyze the full day or narrow the fire-response window to 18:00–23:00.
Keep as-is until this is decided.

## Key Interchanges in Corridor

- **PM 18.3–19.1** (I-210) — SR-2 junction (LA Crescenta / Angeles Crest)
- **PM 24.4–24.5** (I-210 / SR-134) — Main evacuation split point
- **PM 8.4–9.6** (SR-134) — SR-2 connectors

## SR-134 Ramp Data Gap — Pending Resolution

**Status:** All 18 OR/FR/FF stations on SR-134 are **100% null across all 4 dates** (Dec 3, 10, 17 baseline + Jan 7 fire day). Verified directly in the raw D7 PeMS source files — this is a persistent sensor outage in PeMS, not a processing error.

**Affected stations (18 total):**
- OR: 716555 (CENTRAL), 716557 (GLENDALE 1), 716560 (HARVEY), 716562 (FIGUEROA), 716564 (SAN RAFAEL), 761046 (PACIFIC), 761052 (SAN FERNANDO)
- FR: 717581 (CENTRAL), 717589 (HARVEY), 717596 (FIGUEROA), 717598 (SAN RAFAEL), 761039 (GLENDALE 1), 761049 (PACIFIC), 761056 (SAN FERNANDO), 774037 (COLORADO)
- FF: 773979, 773980 (OAKVIEW), 774057 (FM 2 SB TO 134)

**Implication:** No direct on/off ramp counts available for SR-134. ML mainline sensors are healthy.

## SR-134 WB HOV Data Quality — Confirmed Imputation

**Status:** On Jan 7 (fire day), SR-134 WB HOV sensor data is **100% imputed by PeMS** — it does NOT reflect observed evacuation behavior.

**Evidence:** Fire day HOV values match the Dec 3/10/17 baseline with **zero difference** (max absolute flow diff = 0.0, across all 12 HOV stations × 204 time intervals). Timestamps are genuine 2025-01-07 dates, ruling out a file mix-up.

**Root cause:** HOV detectors on SR-134 WB were malfunctioning on Jan 7. PeMS filled in missing readings using its internal imputation algorithm, which draws from recent historical Tuesday data — the exact same Dec 3/10/17 source used to construct our baseline. The result is a perfect mirror, not a real signal.

**Affected stations:** All 12 SR-134 WB HOV stations (Lane Type = 'HV').

**Implication for analysis:**
- SR-134 WB HOV rows in the summary table are flagged as unreliable (`⚠ PeMS imputed`)
- Do **not** use SR-134 WB HOV for fire day demand or evacuation response analysis
- SR-134 WB ML (mainline) sensors appear valid — flow surge of ~+16% observed post-ignition

**SR-134 WB sensor health summary (Jan 7):**
| Sensor Type | Status |
|-------------|--------|
| ML (mainline) | ✅ Valid — real data, fire response visible |
| HV (HOV lane) | ⚠ Imputed — PeMS historical fill, not observed |
| OR / FR / FF (ramps) | ❌ 100% null — persistent outage (all 4 dates) |

**Planned resolution (deferred to simulation calibration phase):**
Estimate ramp flows via ML conservation residuals:
$$q_{OR} - q_{FR} = q_{ML,\text{downstream}} - q_{ML,\text{upstream}}$$

**Caveat:** At most SR-134 PM locations an OR and FR are co-located (e.g., PM 6.503 PACIFIC, PM 8.743 HARVEY, PM 11.473 FIGUEROA, PM 12.253 SAN RAFAEL). The residual gives the *net* ramp effect but not individual OR and FR flows. Disaggregating them requires an assumed OR:FR split ratio — this assumption must be discussed with PI before implementation.

---

## Research Arc — Up for Debate

---

### Your Arc

1. **Baseline** (Dec 3/10/17) — Characterize normal Tuesday operations. Establishes what the network looks like under routine demand. Used as a diagnostic reference, not a simulation target.

2. **Fire day observed** (Jan 7) — Ground truth of what actually happened. Surges, collapses, congestion onset times, spatial patterns along I-210 and SR-134.

3. **Delta analysis** — Quantify deviation between fire day and baseline. Identifies where the network failed, how severe, and when. Produces calibration targets (peak % deviation, first significant deviation time per segment).

4. **Simulation — fire day replay** — Feed the Jan 7 observed demand (flows, speeds, entry/exit counts) into Scalation as inputs. Model reproduces the fire day scenario as-is, no interventions. This becomes the **uncontrolled evacuation baseline** for the simulation.

5. **Simulation — intervention scenarios** — Modify Step 4 by adding control strategies (contraflow, ramp metering, signal coordination). Goal is not to recover normal day traffic — it is to **reduce clearance time and prevent gridlock during the evacuation**. Benchmark is Step 4 (uncontrolled fire day), not the Dec baseline.

---

### My Arc (original — calibration framing)

1. **Baseline** (Dec 3/10/17) — Same as above.

2. **Fire day observed** (Jan 7) — Same as above.

3. **Delta analysis** — Same as above.

4. **Simulation — calibration** — Tune Scalation model parameters (free-flow speed, jam density, reaction time) until simulated output matches fire day observed flow. Fire day = calibration target. RMSE between simulated and observed flow is the goodness-of-fit metric. This is a standard traffic simulation calibration loop.

5. **Simulation — intervention scenarios** — Run the calibrated model with control strategies. Benchmark is the Dec baseline — framed as "did the intervention bring traffic closer to normal day conditions?"

---

### Key Differences

| | Your Arc | My Arc |
|---|---|---|
| Step 4 | Fire day as direct simulation *input* (replay) | Fire day as calibration *target* (fit model to it) |
| Step 5 benchmark | Uncontrolled fire day replay | Normal day (Dec baseline) |
| Success metric | Reduced clearance time vs. uncontrolled | RMSE / match to normal day |
| Problem framing | Scenario comparison | Model calibration + comparison |

---

## Session State — Paused (resume here)

**Last completed:** Summary tables in notebook (Cell 4) + SR-134 WB HOV imputation investigation.

**Notebook:** `data-eaton/analysis/eaton_fire_analysis.ipynb` — 11 cells, all executed and producing output.

### What's done
- ✅ 48 data files loaded (ML_HV 23 cols, OR/FR/FF 16 cols)
- ✅ BaselineData_Dec03-10-17/ — 12 averaged files verified
- ✅ ML/HV correctly split at load time (`fire_ml`, `fire_hv`, `baseline_ml`, `baseline_hv`)
- ✅ Macro plots: 3 corridors × 2 cols (Flow left, Speed right), ignition marker
- ✅ Micro plots: 40 standalone figures — 1 per corridor × lane × metric (flow or speed), 2 lines each (baseline dashed, fire solid)
- ✅ Summary tables: macro (6 rows: 3 corridors × ML + HOV) + micro (per-lane; Lane 1–6 + HOV per corridor), colored ▲/▼ deltas
- ✅ SR-134 WB HOV imputation confirmed and documented (this file)

### What's still pending
- ⏳ **Flag SR-134 WB HOV rows in summary table** — add `⚠ PeMS imputed` note to Cell 4 (`#VSC-10333dae`) in the notebook so readers see it inline
- ⏳ **Decide on research arc** (Your Arc vs. My Arc — see section above); affects how simulation inputs are framed
- ⏳ **OR/FR/FF ramp estimation** — conservation residual approach (see SR-134 Ramp Data Gap section above); deferred to calibration phase
- ⏳ **Full simulation pipeline** — feeding calibrated demand into Scalation, building intervention scenarios

### How to restart
1. `cd /mnt/c/Simulation/traffic-twin/caltrans-pems`
2. `source .venv/bin/activate`
3. Open `data-eaton/analysis/eaton_fire_analysis.ipynb` and re-run all cells (kernel was cleared)
4. Pick up from the pending items above

