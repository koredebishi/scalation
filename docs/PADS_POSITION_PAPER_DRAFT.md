# PADS 2026 Position Paper: Lane-Level Parallelism

**Working Title:**  
*Lane-Level Logical Processes for High-Fidelity Microscopic Traffic Simulation*

**Target:** PADS 2026 (ACM SIGSIM Conference on Principles of Advanced Discrete Simulation)  
**Submission Type:** Short Paper (4-6 pages)  
**Deadline:** January 23, 2026  
**Created:** January 5, 2026

---

## Abstract (Draft)

Microscopic traffic simulators are increasingly used as digital twins for real-time monitoring and control. While existing platforms scale through parallelization at the road-segment or network-partition level, recent lane-level validation studies reveal systematic fidelity loss that cannot be resolved through parameter calibration alone. In this paper, we argue that this limitation arises from a mismatch between the unit of parallel execution and the underlying causal structure of lane-based traffic dynamics.

Using empirical observations from lane-resolved sensor validation, we show that per-lane flow and speed dynamics exhibit localized interactions that are obscured by segment-level aggregation. We propose a simulation architecture in which each lane is treated as an independent logical process, with lane-change maneuvers modeled explicitly as cross-process synchronization events. Within-lane vehicle interactions remain serial and causally ordered, while sensor recording and aggregation become embarrassingly parallel.

We discuss the implications of lane-level logical processes for synchronization, message passing, and execution efficiency in discrete-event simulation, and contrast this approach with traditional segment-based parallelism. While motivated by traffic simulation, the proposed principles apply broadly to lane-structured systems such as runways, conveyor networks, and multi-channel service systems. This work positions lane-level parallelism as a necessary architectural foundation for high-fidelity digital twins of microscopic flow systems.

---

## 1. Introduction

*(2-3 paragraphs motivating the problem)*

Digital twins for traffic systems require microscopic simulation models that can reproduce lane-level dynamics with sufficient fidelity to support real-time monitoring, calibration, and what-if analysis. Current parallel discrete-event simulation (PDES) approaches to traffic modeling typically decompose networks at the level of road segments, intersections, or spatial partitions. Under this formulation, multiple lanes within a segment are simulated within a single logical process (LP), with lane-specific behavior encoded as internal state.

This design choice reflects historical assumptions about computational granularity and communication overhead. However, empirical validation against lane-resolved sensor data reveals systematic discrepancies that persist even after extensive parameter calibration. These errors manifest as lane-specific biases in flow counts and speed profiles that cannot be attributed to behavioral model limitations alone.

This paper argues that the root cause is architectural: **the unit of parallel execution is misaligned with the causal structure of lane-based traffic dynamics**. We propose treating each lane as an independent logical process, fundamentally restructuring how parallelism is expressed in microscopic traffic simulation.

---

## 2. Problem Statement: Parallelism Granularity and Lane-Level Fidelity

Microscopic traffic simulators increasingly serve as digital twins for operational analysis, calibration, and real-time decision support. To scale to large networks, most contemporary simulators adopt parallel execution strategies that partition the system at the level of road segments, junctions, or subnetworks. In this formulation, multiple lanes within a segment are simulated within a single logical process (LP), with lane-specific behavior represented as internal state.

This design implicitly assumes that lane-level interactions are either weakly coupled or sufficiently captured through aggregate execution. However, recent lane-resolved validation studies indicate that this assumption does not hold in practice. Empirical comparisons against per-lane sensor data reveal systematic discrepancies in lane-specific flow, speed, and congestion propagation that persist even after extensive parameter calibration. These errors are not uniformly distributed across lanes, nor do they vanish when aggregate segment-level metrics appear accurate.

The root cause of this mismatch lies in a misalignment between the unit of parallel execution and the true causal structure of lane-based traffic dynamics. Within a lane, vehicle interactions are strictly ordered and locally causal, while lane-change maneuvers introduce discrete synchronization events between neighboring lanes. When multiple lanes are simulated within a single LP, these cross-lane interactions are resolved implicitly through shared state updates, obscuring causal boundaries and preventing explicit synchronization control. As a result, simulators are unable to isolate, observe, or validate lane-level dynamics independently.

This architectural choice limits both fidelity and extensibility. From a fidelity perspective, it prevents simulators from reproducing empirically observed lane asymmetries and localized congestion effects. From a simulation systems perspective, it restricts opportunities for fine-grained parallelism, as lane-level computations and sensor observations remain entangled within coarse-grained execution units.

**The problem addressed in this paper is therefore not parameter estimation or behavioral modeling, but execution architecture: what is the correct unit of parallelism for microscopic, lane-resolved traffic simulation?** We argue that resolving lane-level fidelity requires treating each lane as an independent logical process, with lane-change events modeled explicitly as cross-process synchronization. Without this architectural shift, lane-level validation errors are structural and cannot be eliminated through calibration alone.

---

## 3. Lane-as-LP Architecture (Proposed)

### 3.1 Causal Structure of Lane-Based Traffic

Within a single lane:
- Vehicle interactions are **serial** and **causally ordered**
- Each vehicle responds only to its immediate predecessor (car-following)
- No synchronization required between non-adjacent vehicles

Between adjacent lanes:
- Lane-change maneuvers create **discrete synchronization events**
- Gap acceptance requires reading state from target lane
- Merge completion requires atomic state transfer

This natural decomposition suggests:

| Component | Parallel Model |
|-----------|----------------|
| Within-lane following | Serial, local event queue |
| Lane-change decision | Cross-LP message (gap query) |
| Lane-change execution | Cross-LP synchronization (vehicle transfer) |
| Sensor recording | Embarrassingly parallel (per-lane aggregation) |

### 3.2 Logical Process Structure

```
Lane LP (for lane i):
├── Local event queue (vehicle movements within lane)
├── Predecessor pointer (next LP downstream)
├── Neighbor pointers (adjacent lanes for lane-change)
├── Vehicle list (ordered by position)
└── Sensor observer (local count/speed aggregation)

Lane-change event:
├── Source LP: remove vehicle, notify target
├── Target LP: insert vehicle, acknowledge
└── Synchronization: conservative or optimistic
```

### 3.3 Contrast with Segment-Level Parallelism

| Aspect | Segment-as-LP | Lane-as-LP |
|--------|---------------|------------|
| LP granularity | 4 lanes × 1 LP | 4 lanes × 4 LPs |
| Lane-change | Internal state update | Explicit message |
| Sensor isolation | Aggregated post-hoc | Independent observers |
| Causal visibility | Hidden | Explicit |
| Validation unit | Segment | Lane |

---

## 4. Empirical Motivation

*(Evidence from ANNSIM lane-level validation work)*

### 4.1 Lane-Level Validation Against PeMS Data

We conducted lane-resolved validation of a microscopic traffic simulator against California PeMS sensor data on a 4-lane US-101 corridor with 5 detector stations at 15-minute intervals.

**Observation 1: Aggregate accuracy masks lane-level errors**

| Metric | Segment-Level R² | Lane 1 R² | Lane 2 R² | Lane 3 R² | Lane 4 R² |
|--------|------------------|-----------|-----------|-----------|-----------|
| Flow   | 0.96             | 0.98      | 0.93      | 0.64      | 0.97      |
| Speed  | 0.75             | 0.84      | 0.72      | 0.53      | 0.72      |

Segment-level metrics hide that Lane 3 (merge-adjacent) has significantly lower fidelity.

**Observation 2: Lane-specific biases are persistent**

Even after calibration, inner lanes (near merge) show systematic speed underestimation that does not propagate uniformly across lanes.

**Observation 3: Lane-change events create localized disturbances**

Speed drops propagate lane-by-lane with observable delay, not simultaneously across the segment.

These observations cannot be resolved through parameter tuning within a segment-level LP model.

---

## 5. Implications for PDES Design

### 5.1 Synchronization Strategies

**Conservative (Chandy-Misra-Bryant):**
- Lane-change events as timestamped messages
- Null messages for quiescent lanes
- Deadlock avoidance via lookahead (reaction time τ)

**Optimistic (Time Warp):**
- Speculative lane-change execution
- Rollback on gap acceptance failure
- State saving per vehicle (lightweight)

### 5.2 Expected Overhead Analysis

| Operation | Segment-LP | Lane-LP | Difference |
|-----------|------------|---------|------------|
| Within-lane move | O(1) | O(1) | Same |
| Lane-change | O(1) shared | O(1) + msg | +message overhead |
| Sensor aggregation | Sequential | Parallel | Speedup opportunity |
| Validation isolation | Impossible | Natural | Fidelity improvement |

Trade-off: Increased message volume for lane-changes, but:
- Lane-changes are infrequent (~1-5% of movements)
- Within-lane operations (95%+) remain local
- Sensor aggregation becomes embarrassingly parallel

### 5.3 Generalization Beyond Traffic

Lane-structured flow systems with similar properties:

| Domain | "Lane" Analog | Cross-Lane Event |
|--------|---------------|------------------|
| Runway operations | Parallel runways | Aircraft reassignment |
| Conveyor systems | Parallel belts | Item transfer |
| Multi-channel queues | Service channels | Customer switching |
| Packet networks | Virtual circuits | Route migration |

The proposed architecture applies wherever:
1. Within-channel interactions are serial
2. Cross-channel transfers are discrete events
3. Per-channel validation is required

---

## 6. Related Work

### LP Granularity in Parallel DES
- Link-based vs node-based network simulation debates
- Server-centric vs queue-centric queuing models
- Fine-grained vs coarse-grained decomposition trade-offs

### Traffic Simulation Parallelization
- SUMO: Segment-based spatial decomposition
- VISSIM: Proprietary partitioning
- MATSim: Agent-based parallelization
- *None explicitly model lane as LP*

### Actor-Based DES
- Lane-as-actor formulation
- Message-passing semantics for lane-change
- Local event queue per actor

---

## 7. Conclusion and Future Work

We have argued that lane-level fidelity in microscopic traffic simulation requires architectural change, not merely parameter calibration. Treating each lane as an independent logical process:

1. **Exposes causal structure** hidden by segment-level aggregation
2. **Enables independent validation** at the lane level
3. **Makes lane-change synchronization explicit** rather than implicit
4. **Generalizes** to other lane-structured flow systems

Future work includes:
- Full parallel implementation and performance evaluation
- Synchronization protocol comparison (conservative vs optimistic)
- Extension to lane-addition/lane-drop geometry
- Application to multi-runway airport simulation

---

## Contribution Summary (for reviewer response)

> This paper makes three contributions to the PDES community:
>
> 1. **Diagnosis:** We identify LP granularity mismatch as a structural cause of lane-level validation errors in microscopic traffic simulation, distinct from behavioral model limitations.
>
> 2. **Architecture:** We propose lane-as-LP decomposition with explicit lane-change synchronization, contrasting with current segment-based approaches.
>
> 3. **Generalization:** We abstract the principle to lane-structured flow systems broadly, positioning this as a general PDES design pattern.

---

## Why PADS Short Paper (Not Regular)

| Criterion | Assessment |
|-----------|------------|
| Novel principle | ✅ LP granularity for lane-structured systems |
| Empirical evidence | ✅ Lane-level validation data (from ANNSIM) |
| Full performance evaluation | ❌ Not yet (future work) |
| Synchronization benchmarks | ❌ Not yet (future work) |
| Implementation maturity | ⚠️ Coroutine-based, not fully parallel |

**Recommendation:** Short paper establishes the principle; regular paper (later) adds performance.

---

## PADS Category Wording

> "This short paper presents a position on logical process granularity for microscopic simulation of lane-structured flow systems, motivated by empirical lane-level validation results and proposing lane-as-LP decomposition as an architectural principle."

---

## References (Draft)

- Chandy, K.M., Misra, J. (1979). Distributed Simulation: A Case Study in Design and Verification of Distributed Programs. *IEEE TSE*.
- Jefferson, D.R. (1985). Virtual Time. *ACM TOPLAS*.
- Treiber, M., Kesting, A. (2013). *Traffic Flow Dynamics*.
- Fujimoto, R.M. (2000). *Parallel and Distributed Simulation Systems*.
- [Add PADS precedent papers on LP granularity]
- [Add traffic simulation parallelization papers]

---

## Timeline to PADS Submission (Jan 23)

| Day | Date | Task |
|-----|------|------|
| 1-6 | Jan 5-10 | Complete ANNSIM submission |
| 7 | Jan 11 | Submit ANNSIM, pivot to PADS |
| 8-12 | Jan 12-16 | Write PADS Sections 1-4 (principles) |
| 13-15 | Jan 17-19 | Write Section 5 (PDES implications) |
| 16-17 | Jan 20-21 | Write Section 6-7, polish abstract |
| 18 | Jan 22 | Internal review |
| 19 | Jan 23 | Submit to PADS |

---

*This is a principled position paper. Empirical evidence comes from ANNSIM lane-level validation. Performance evaluation is future work.*

