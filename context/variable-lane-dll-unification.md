# Variable Lane Architecture — DLL Unification Visual Explainer

## 1. Current Architecture: Two Parallel Data Structures

A 3-lane corridor with 4 segments. Each lane has **both** a Pathway-level DLL
**and** per-segment VTransport deques tracking the **same** cars.

```
LANE 0 (Pathway_0)
══════════════════════════════════════════════════════════════════════════════

  Pathway_0.vList (ONE DLL spanning ALL segments):
  ┌──────────────────────────────────────────────────────────────────────┐
  │  tail ←→ [C9] ←→ [C6] ←→ [C4] ←→ [C3] ←→ [C1] ←→ head          │
  │  (just entered)                               (about to exit)      │
  │                                                                    │
  │  C1.myPathNode.ahead == null  (C1 is the lead car)                 │
  │  C3.myPathNode.ahead → C1     (C3 follows C1)                     │
  │  C4.myPathNode.ahead → C3     (C4 follows C3)       ← CAR-FOLLOWING
  │  C6.myPathNode.ahead → C4     (C6 follows C4)                     │
  │  C9.myPathNode.ahead → C6     (C9 follows C6)                     │
  └──────────────────────────────────────────────────────────────────────┘

  VTransport deques (per-segment, ALSO tracking the same cars):

  seg[0].vdeque     seg[1].vdeque     seg[2].vdeque     seg[3].vdeque
  ┌───────────┐     ┌───────────┐     ┌───────────┐     ┌───────────┐
  │ [C9] [C6] │     │ [C4] [C3] │     │    [C1]   │     │  (empty)  │
  └───────────┘     └───────────┘     └───────────┘     └───────────┘
       ↑                  ↑                 ↑                 ↑
   getFirst/Last      getFirst/Last     getFirst/Last     getFirst/Last
   density()          density()         density()         density()
   changeLane()       changeLane()      changeLane()      changeLane()

══════════════════════════════════════════════════════════════════════════════
LANE 1 (Pathway_1)  — same pattern, different cars
LANE 2 (Pathway_2)  — same pattern, different cars
```

### The Problem: They Can Disagree

When C4 finishes seg[1] and enters seg[2], the **deque** updates immediately:
```
  seg[1].vdeque: [C4] removed     seg[2].vdeque: [C4] added
```

But in the **DLL**, C4 is still between C6 and C3:
```
  ... ←→ [C6] ←→ [C4] ←→ [C3] ←→ [C1] ←→ ...
```

C6 is in seg[0], C4 is now in seg[2]. C6 asks "who is ahead of me?"
→ DLL says C4. But C4 is two segments ahead. Is that the right leader?

**This is why the `segId` patch exists in IDMDynamics (line 330):**
```scala
if car_ahead.segId < car.segId then     // stale! leader moved ahead
    (car.t_disp + 1000.0, car.velocity)  // ← pretend no leader (free-flow)
```

The DLL doesn't know about segments. The deque does.  
Two data structures. Redundant. Can disagree. Patch required.

---

## 2. Proposed Architecture: Per-VTransport DLL (Unification)

**Move the DLL down into VTransport.** Each segment owns its own DLL.
The Pathway-level DLL is removed.

```
LANE 0 (Pathway_0)
══════════════════════════════════════════════════════════════════════════════

  Pathway_0.vList → REMOVED (no more lane-spanning DLL)

  Each VTransport has its own DLL:

  seg[0].vList              seg[1].vList              seg[2].vList              seg[3].vList
  ┌───────────────────┐     ┌───────────────────┐     ┌───────────────────┐     ┌────────────┐
  │ tail←→[C9]←→[C6]←→head │ tail←→[C4]←→[C3]←→head │ tail←→[C1]←→head  │     │  (empty)   │
  └───────────────────┘     └───────────────────┘     └───────────────────┘     └────────────┘
         ↑                         ↑                         ↑
      Within-segment            Within-segment            Within-segment
      car-following:            car-following:            car-following:
      C9.ahead → C6            C4.ahead → C3             C1.ahead → null
      C6.ahead → null ←─┐      C3.ahead → null ←─┐       (lead car) ←──────── free-flow
                         │                        │
                    BOUNDARY                 BOUNDARY
                    LOOKUP                   LOOKUP
                         │                        │
                         └→ seg[1].vList.last      └→ seg[2].vList.last
                            = C4 (tail)               = C1 (tail)
```

### Within-Segment Car-Following (unchanged)

```
  seg[1].vList:   tail ←→ [C4] ←→ [C3] ←→ head

  C4 asks: "who is ahead of me?"
  → C4.myPathNode.ahead → C3     ← O(1), exactly like today
  → IDM computes gap to C3, adjusts velocity

  C3 asks: "who is ahead of me?"
  → C3.myPathNode.ahead → null   ← I'm the lead car in THIS segment
  → Need cross-boundary lookup (see below)
```

### Cross-Segment Boundary Lookup (new, O(1))

```
  C3 is the HEAD of seg[1].vList (lead car, about to exit segment 1)
  C3.myPathNode.ahead == null

  Where is C3's real leader?  →  The TAIL of the NEXT segment's DLL.

  seg[1].vList                              seg[2].vList
  ┌──────────────────────────┐              ┌──────────────────────┐
  │ ... ←→ [C3] ←→ head     │   boundary   │  tail ←→ [C1] ←→ ...│
  │         .ahead == null ──┼──────────────┼→ seg[2].vList.last   │
  │                          │              │  = C1 (just entered  │
  │                          │              │   seg 2, closest to  │
  │                          │              │   the boundary)      │
  └──────────────────────────┘              └──────────────────────┘

  Pseudocode in Dynamics.updateM:
  ─────────────────────────────
  val ref = car.myPathNode.ahead
  val car_ahead =
      if ref != null then ref.elem                          // normal: within-segment
      else if car.segId + 1 < pathway.seg.length then       // boundary: look at next seg
          pathway.seg(car.segId + 1).vList.last              // O(1) — tail of next DLL
      else null                                              // last segment: free-flow
```

**No `segId < car.segId` patch needed.** The DLL can never return a car from
a different segment — each DLL is scoped to exactly one VTransport.

---

## 3. Why Variable Lanes Fall Out Naturally

### Current: All Lanes Must Span All Segments

```
  numLanes = 5 (forced uniform)

  Pathway_0: seg[0]──seg[1]──seg[2]──seg[3]──seg[4]──seg[5]──seg[6]
  Pathway_1: seg[0]──seg[1]──seg[2]──seg[3]──seg[4]──seg[5]──seg[6]
  Pathway_2: seg[0]──seg[1]──seg[2]──seg[3]──seg[4]──seg[5]──seg[6]
  Pathway_3: seg[0]──seg[1]──seg[2]──seg[3]──seg[4]──seg[5]──seg[6]
  Pathway_4: seg[0]──seg[1]──seg[2]──seg[3]──seg[4]──seg[5]──seg[6]
                                  ↑
                     Every lane has a VTransport at every
                     segment because the Pathway-level DLL
                     expects to span them all.

  I-210 WB reality:  5    6    4    2    4    5    4   ← lanes per segment
  Simulation:         5    5    5    5    5    5    5   ← forced uniform
```

### Proposed: Sparse — Only Real Lanes Get VTransports

```
  I-210 WB lane profile:   5    6    4    2    4    5    4

  Lane 0: seg[0]──seg[1]──seg[2]──seg[3]──seg[4]──seg[5]──seg[6]   ← always exists
  Lane 1: seg[0]──seg[1]──seg[2]──seg[3]──seg[4]──seg[5]──seg[6]   ← always exists
  Lane 2: seg[0]──seg[1]──seg[2]──  ___  ──seg[4]──seg[5]──seg[6]   ← gap at seg[3]
  Lane 3: seg[0]──seg[1]──  ___  ──  ___  ──  ___  ──seg[5]──  ___   ← only where 4+ lanes
  Lane 4: seg[0]──  ___  ──  ___  ──  ___  ──  ___  ──seg[5]──  ___   ← only where 5+ lanes
  Lane 5:   ___  ──seg[1]──  ___  ──  ___  ──  ___  ──  ___  ──  ___   ← only where 6 lanes

              5      6      4      2      4      5      4   ✓ matches reality
```

Each `___` means: no VTransport, no DLL, no vehicles.  
No phantom segments. No lane masks. The lane simply **doesn't exist** there.

### What Happens When a Lane Ends?

```
  Lane 3 exists at seg[0] but NOT at seg[1]:

  Lane 3, seg[0]:                   Lane 3, seg[1]:
  ┌──────────────────────┐          ┌─────────────────┐
  │ tail ←→ [C7] ←→ head │          │    DOES NOT      │
  │                      │          │    EXIST          │
  └──────────────────────┘          └─────────────────┘
                ↑
       C7 is driving seg[0].
       Before C7 reaches the end of seg[0],
       driveHighway must FORCE a lane change:

       "Lane 3 doesn't exist at segment 1.
        Merge to lane 2 or lane 4 (if they exist)."

       → route.forceMerge(3, availableLanes(seg=1), car, seg=0)

  This is exactly what real drivers do:
  ┌─────────────────────────────────────────────────────────────┐
  │   LANE ENDS          │                                      │
  │   ═══════╗           │  The lane physically ends.           │
  │          ║ MERGE →   │  Driver must merge.                  │
  │   ═══════╝           │  Simulation forces the lane change.  │
  │                      │                                      │
  └─────────────────────────────────────────────────────────────┘
```

---

## 4. Before & After Comparison

```
┌─────────────────────────────────────┬─────────────────────────────────────┐
│           CURRENT                   │           PROPOSED                  │
├─────────────────────────────────────┼─────────────────────────────────────┤
│                                     │                                     │
│  Pathway                            │  Pathway                            │
│  ├── vList: DLL (all segments)      │  ├── vList: REMOVED                 │
│  └── seg: Array[VTransport]         │  └── seg: Array[Option[VTransport]] │
│       └── vdeque: ArrayDeque        │       └── vList: DLL (this seg)     │
│           (bookkeeping only)        │           (car-following + booking)  │
│                                     │                                     │
│  Two structures, same cars:         │  One structure per segment:         │
│   • DLL for car-following           │   • DLL for car-following           │
│   • Deque for segment queries       │   • (deque functionality merged)    │
│   • Can disagree → segId patch      │   • Cannot disagree → no patch      │
│                                     │                                     │
│  Route(numLanes: Int)               │  Route(lanesPerSeg: Array[Int])     │
│   → N identical Pathways            │   → max(lanes) Pathways, sparse     │
│   → rectangular seg array           │   → seg(i) = None where lane absent │
│                                     │                                     │
│  Car-following leader lookup:       │  Car-following leader lookup:        │
│   car.myPathNode.ahead              │   car.myPathNode.ahead              │
│   (may return car from ANY seg)     │   (always same segment)             │
│   + segId patch for stale refs      │   + boundary: nextSeg.vList.last    │
│                                     │   (no patch needed)                 │
│                                     │                                     │
│  Variable lanes: IMPOSSIBLE         │  Variable lanes: NATURAL            │
│  (Pathway must span all segments)   │  (no VTransport = lane doesn't      │
│                                     │   exist at that segment)            │
└─────────────────────────────────────┴─────────────────────────────────────┘
```

---

## 5. Call Flow: IDMDynamics.updateM — Before and After

### Before (Current)
```
  IDMDynamics.updateM(car, length)
      │
      ├── ref = car.myPathNode.ahead          ← Pathway-level DLL
      │       → may return car from ANY segment
      │
      ├── if ref == null → free-flow (no leader)
      │
      ├── if car_ahead.segId < car.segId      ← PATCH: DLL gave stale leader
      │       → free-flow (ignore it)
      │
      ├── if gap > FREERANGE → free-flow
      │
      └── else → real leader, compute IDM acceleration
```

### After (Proposed)
```
  IDMDynamics.updateM(car, length)
      │
      ├── ref = car.myPathNode.ahead          ← VTransport-level DLL
      │       → only returns car from SAME segment (or null)
      │
      ├── if ref != null → real leader (same segment), compute IDM
      │
      ├── if ref == null → boundary lookup:
      │       │
      │       ├── nextSeg = pathway.seg(car.segId + 1)
      │       │
      │       ├── if nextSeg exists AND nextSeg.isDefined
      │       │       → leader = nextSeg.get.vList.last     ← O(1)
      │       │       → compute IDM with cross-boundary gap
      │       │
      │       └── else → free-flow (last segment or lane ends)
      │
      └── NO segId patch needed — DLL is always segment-correct
```

---

## 6. File Change Map

```
  ┌─────────────────────────────┬───────────────────────────────────────────┐
  │ File                        │ Change                                    │
  ├─────────────────────────────┼───────────────────────────────────────────┤
  │ VTransport.scala            │ + vList: DoublyLinkedList[Vehicle]        │
  │                             │ + addToAlist / removeFromAlist            │
  │                             │ - vdeque (merged into vList, or kept for  │
  │                             │   density queries if needed)              │
  ├─────────────────────────────┼───────────────────────────────────────────┤
  │ Pathway.scala               │ - vList (removed — no lane-spanning DLL) │
  │                             │ ~ seg: Array[Option[VTransport]]          │
  │                             │ ~ addToAlist → delegates to VTransport    │
  │                             │ ~ removeFromAlist → delegates             │
  ├─────────────────────────────┼───────────────────────────────────────────┤
  │ Route.scala                 │ ~ numLanes → lanesPerSeg: Array[Int]     │
  │                             │ ~ creates sparse Pathways                │
  │                             │ ~ changeLane checks lane existence        │
  │                             │ + forceMerge at lane-end boundaries       │
  ├─────────────────────────────┼───────────────────────────────────────────┤
  │ Dynamics.scala              │ ~ updateM: cross-boundary leader lookup   │
  │                             │ - segId patch (lines 330-331 removed)     │
  ├─────────────────────────────┼───────────────────────────────────────────┤
  │ NetworkConfig.scala         │ ~ lanesPerSegment: Int → Array[Int]       │
  ├─────────────────────────────┼───────────────────────────────────────────┤
  │ EatonCorridorConfig.scala   │ ~ use laneCounts array directly           │
  │                             │   (stop discarding per-sensor lane data)  │
  ├─────────────────────────────┼───────────────────────────────────────────┤
  │ EatonFireModel.scala        │ ~ driveHighway: check lane existence      │
  │ (and CalRoute101_3.scala)   │   before seg.move(), force merge if lane  │
  │                             │   ends at next segment                    │
  ├─────────────────────────────┼───────────────────────────────────────────┤
  │ Vehicle.scala               │ ~ myPathNode now points into VTransport's │
  │                             │   DLL (not Pathway's)                     │
  │                             │   No field type change needed.            │
  └─────────────────────────────┴───────────────────────────────────────────┘
```

---

## 7. The Key Insight (One Picture)

```
  CURRENT: Two structures, one truth, one patch
  ─────────────────────────────────────────────
                          Pathway.vList (DLL)
  entry ═══[C9]═══[C6]═══[C4]═══[C3]═══[C1]═══ exit     ← flat, no segments
                    │       │       │
                    ▼       ▼       ▼
            seg[0].deque  seg[1].deque  seg[2].deque      ← segment-aware
            {C9, C6}      {C4, C3}      {C1}

            Q: Who is ahead of C6?
            DLL says: C4 ← might be in a different segment (STALE)
            Need segId patch to detect and ignore


  PROPOSED: One structure, one truth, no patch
  ─────────────────────────────────────────────
            seg[0].vList     seg[1].vList     seg[2].vList
            ┌────────────┐   ┌────────────┐   ┌────────────┐
            │[C9]←→[C6]  │   │[C4]←→[C3]  │   │   [C1]     │
            └────────────┘   └────────────┘   └────────────┘
                       ↑           ↑
                  C6.ahead=null    C3.ahead=null
                  → boundary:      → boundary:
                    seg[1].last      seg[2].last
                    = C4 ✓           = C1 ✓

            Q: Who is ahead of C6?
            DLL says: null (C6 is head of seg[0])
            Boundary lookup: seg[1].vList.last = C4 ✓ (always correct segment)
```

