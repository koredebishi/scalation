# IEEE Transactions on Intelligent Transportation Systems - Journal Extension Plan

## Paper Title (Working)
**"Lane-Level Validation of Microscopic Traffic Simulation: Addressing a Critical Gap in Car-Following Model Assessment"**

---

## Executive Summary

This document outlines the plan to extend the ANNSIM 2026 conference paper into a full journal submission for IEEE Transactions on Intelligent Transportation Systems (T-ITS).

**Target Journal:** IEEE Transactions on Intelligent Transportation Systems  
**Impact Factor:** ~8.5  
**Submission Target:** September 2026  
**Review Timeline:** 3-6 months typical  

---

## Current Work (ANNSIM 2026)

### What We Have:
- ✅ 8 numerical integrators compared (Ballistic, Euler, Heun, RK2, RK3, RK4, DOPRI5)
- ✅ 2 arrival processes compared (Poisson vs Shifted Erlang-2)
- ✅ IDM car-following model implementation in ScalaTion 2.0
- ✅ 1 corridor: US-101 Donald Doyle Highway (1.45 km, 4 lanes, 5 sensors)
- ✅ 12-hour simulation (6:00 AM - 6:00 PM)
- ✅ Macro-level validation (R², NRMSE for flow and speed)
- ✅ Micro-level lane-wise validation (20 lane-sensor combinations)
- ✅ ~60,000 vehicles simulated

### Key Findings:
1. ODE integrator choice has <1% impact on accuracy (only affects runtime)
2. Shifted Erlang-2 reduces flow error by 28% vs Poisson
3. Lane-level validation reveals dynamics obscured by aggregate metrics
4. Fast lanes: R² > 0.95; Slow lanes: higher variability due to merging

### Gap Identified:
- Only Gao et al. (2025) has done lane-level validation before (single bottleneck in SUMO)
- Literature overwhelmingly uses aggregate corridor metrics

---

## Journal Extension Requirements

### 1. Additional Corridors (REQUIRED)
**Purpose:** Generalizability of findings

| Corridor | Characteristics | Data Source |
|----------|-----------------|-------------|
| Current: US-101 Donald Doyle | 4 lanes, 2 on-ramps, 1.45 km | PeMS |
| New #1: Different geometry | 3 lanes, bottleneck, ~2 km | PeMS |
| New #2: Different demand | Peak-only vs all-day | PeMS |

**Effort:** 2-3 weeks data collection + 1-2 weeks simulation runs

### 2. Simulator Comparison (HIGHLY RECOMMENDED)
**Purpose:** Validate findings aren't ScalaTion-specific

| Simulator | Car-Following Model | Notes |
|-----------|---------------------|-------|
| ScalaTion 2.0 | IDM | Current implementation |
| SUMO | IDM, Krauss | Open source, widely used |
| (Optional) Vissim | Wiedemann 99 | Commercial, gold standard |

**Effort:** 2-4 weeks for SUMO implementation and runs

### 3. Statistical Rigor (REQUIRED)
**Purpose:** Publication-quality analysis

- [ ] Confidence intervals for all metrics
- [ ] Paired t-tests for integrator comparison
- [ ] ANOVA for arrival process × integrator interaction
- [ ] Effect size reporting (Cohen's d)
- [ ] Multiple runs per configuration (n ≥ 10)

**Effort:** 1-2 weeks for reruns + analysis

### 4. Car-Following Model Comparison (RECOMMENDED)
**Purpose:** Show lane-level validation applies beyond IDM

| Model | Type | Implementation |
|-------|------|----------------|
| IDM | Continuous | Current ✅ |
| Gipps | Discrete | Add to ScalaTion |
| Krauss | Discrete | Via SUMO |

**Effort:** 2-3 weeks

### 5. Application Case Study (RECOMMENDED)
**Purpose:** Demonstrate practical value of lane-level validation

Options:
- **Ramp Metering:** Show lane-specific flow matters for control
- **Variable Speed Limits:** Lane-specific speed targets
- **Lane Change Advisory:** Merge behavior under different conditions

**Effort:** 2-3 weeks

### 6. Sensitivity Analysis (REQUIRED)
**Purpose:** Understanding parameter influence

- IDM parameters: a_max, b, T, s_0, v_max
- Erlang-2 parameters: τ (shift), μ (scale)
- Simulation parameters: Δt (time step)

**Effort:** 1-2 weeks

---

## Proposed Paper Structure

```
1. INTRODUCTION (1.5 pages)
   - Importance of microscopic traffic simulation
   - Gap: lane-level validation is neglected
   - Contributions (3-4 bullet points)

2. RELATED WORK (1.5 pages)
   - Car-following models (IDM, Gipps, Wiedemann)
   - Numerical integration in traffic simulation
   - Arrival process modeling
   - Validation practices (highlight gap)
   - Gao et al. (2025) as only prior lane-level work

3. METHODOLOGY (3 pages)
   3.1 Car-Following Model (IDM)
   3.2 Numerical Integration Methods
   3.3 Vehicle Arrival Processes
   3.4 Lane-Level Validation Framework
   3.5 Performance Metrics

4. EXPERIMENTAL SETUP (2 pages)
   4.1 Study Corridors (3 corridors)
   4.2 Data Collection (PeMS)
   4.3 Simulation Platform (ScalaTion 2.0)
   4.4 Experimental Design

5. RESULTS (4 pages)
   5.1 Numerical Integrator Comparison
   5.2 Arrival Process Comparison
   5.3 Macro-Level Validation
   5.4 Lane-Level Validation
   5.5 Cross-Corridor Comparison
   5.6 Simulator Comparison (if included)

6. DISCUSSION (2 pages)
   6.1 Why Lane-Level Validation Matters
   6.2 Implications for Simulation Practice
   6.3 Comparison with Gao et al. (2025)
   6.4 Limitations

7. CONCLUSION (0.5 pages)

REFERENCES (~40-50 references)
```

**Target Length:** 12-15 pages (IEEE two-column format)

---

## Timeline

```
2026:
├── Jan-Mar: Focus on ANNSIM revision/camera-ready
├── Apr: ANNSIM submission complete
├── May: ANNSIM conference presentation
│
├── May-Jun: Collect additional corridor data
├── Jun-Jul: Implement SUMO comparison
├── Jul-Aug: Run extended experiments
├── Aug: Statistical analysis
├── Sep: Write journal paper
├── Oct: Internal review, revision
├── Nov: Submit to IEEE T-ITS
│
2027:
├── Jan-Mar: Reviews received (expected)
├── Apr-May: Revision
├── Jun-Jul: Resubmission
├── Sep-Dec: Decision (hopefully accepted)
```

---

## Differentiation from ANNSIM Paper

| Aspect | ANNSIM 2026 | IEEE T-ITS Extension |
|--------|-------------|----------------------|
| Corridors | 1 | 3 |
| Simulators | ScalaTion only | ScalaTion + SUMO |
| Statistical tests | Descriptive only | Hypothesis testing |
| CF models | IDM only | IDM + Gipps/Krauss |
| Depth | 8 pages | 12-15 pages |
| Application | None | Case study |
| Self-citation | N/A | Cite ANNSIM paper |

---

## Risk Assessment

| Risk | Likelihood | Mitigation |
|------|------------|------------|
| SUMO results differ significantly | Medium | Report as finding, not problem |
| Additional corridors show different patterns | Medium | Discuss as generalizability insight |
| Reviewers want more CF models | High | Add Gipps at minimum |
| Long review time | Medium | Submit early (Sept), have backup venue |
| Rejection | Low-Medium | Revise for Transportation Research Part C |

---

## Resource Requirements

### Computational:
- HPC access for extended simulation runs (already have via Sapelo2)
- ~500-1000 simulation runs (multiple configurations × multiple seeds)

### Data:
- PeMS access for additional corridors (free, already have account)
- 15-minute aggregated loop detector data

### Software:
- ScalaTion 2.0 (current)
- SUMO (open source, need to set up)
- R/Python for statistical analysis

### Time:
- Estimated total effort: 3-4 months part-time
- Can be parallelized with WSC 2026 work

---

## Success Metrics

1. **Minimum Viable Submission:**
   - 2 corridors (current + 1 new)
   - Statistical tests added
   - Sensitivity analysis
   - Strong discussion section

2. **Ideal Submission:**
   - 3 corridors
   - SUMO comparison
   - 2+ car-following models
   - Application case study
   - All statistical rigor

---

## Next Steps

1. [ ] Identify 2nd corridor from PeMS (different geometry)
2. [ ] Download and preprocess new corridor data
3. [ ] Set up SUMO with equivalent scenario
4. [ ] Design statistical analysis plan
5. [ ] Create simulation experiment matrix
6. [ ] Begin extended runs after ANNSIM camera-ready

---

## Notes

- This plan can be adjusted based on WSC 2026 workload
- The calibration work for WSC could feed back into this journal paper
- Consider co-authorship with advisor for journal submission
- IEEE T-ITS has author guidelines: https://its.ieee.org/publications/t-its

---

*Document created: January 13, 2026*  
*Last updated: January 13, 2026*

