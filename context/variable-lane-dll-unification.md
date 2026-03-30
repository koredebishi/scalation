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

## 7. What Does Each Class Become?

### Current Roles (Who Owns What)

```
  ┌──────────────────────────────────────────────────────────────────────────┐
  │  ROUTE  (the whole highway — all lanes)                                 │
  │  ┌────────────────────────────────────────────────────────────────────┐  │
  │  │  Owns: pathway: Array[Pathway]   (one per lane, size = numLanes)  │  │
  │  │  Does: changeLane(), forceMerge()                                 │  │
  │  │  Does: segmentOffsets, toCumulative                               │  │
  │  │  Does: display all pathways                                       │  │
  │  │  Knows: numLanes (single Int, uniform)                            │  │
  │  └────────────────────────────────────────────────────────────────────┘  │
  │       │           │           │           │           │                  │
  │       ▼           ▼           ▼           ▼           ▼                  │
  │  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐          │
  │  │Pathway_0│ │Pathway_1│ │Pathway_2│ │Pathway_3│ │Pathway_4│          │
  │  │         │ │         │ │         │ │         │ │         │          │
  │  │ vList   │ │ vList   │ │ vList   │ │ vList   │ │ vList   │ ← DLL   │
  │  │ (DLL)   │ │ (DLL)   │ │ (DLL)   │ │ (DLL)   │ │ (DLL)   │         │
  │  │         │ │         │ │         │ │         │ │         │          │
  │  │ seg[0]  │ │ seg[0]  │ │ seg[0]  │ │ seg[0]  │ │ seg[0]  │ ← ALL   │
  │  │ seg[1]  │ │ seg[1]  │ │ seg[1]  │ │ seg[1]  │ │ seg[1]  │   segs  │
  │  │ seg[2]  │ │ seg[2]  │ │ seg[2]  │ │ seg[2]  │ │ seg[2]  │   must  │
  │  │ seg[3]  │ │ seg[3]  │ │ seg[3]  │ │ seg[3]  │ │ seg[3]  │   exist │
  │  └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘          │
  └──────────────────────────────────────────────────────────────────────────┘

  Pathway is:
    1. Owner of the lane-spanning DLL (car-following backbone)
    2. Owner of the seg[] array (all VTransports in this lane)
    3. Entry point for addToAlist / removeFromAlist (DLL operations)
    4. Entry point for getFirst / getLast (lane-wide queries)
    5. Visual display (draw all segments)
```

### After Unification: What Changes

```
  ┌──────────────────────────────────────────────────────────────────────────┐
  │  ROUTE  (the whole highway)                                             │
  │  ┌────────────────────────────────────────────────────────────────────┐  │
  │  │  Owns: pathway: Array[Pathway]   (one per lane, size = maxLanes)  │  │
  │  │  NEW:  lanesAt(seg: Int): Int    (how many lanes at segment seg)  │  │
  │  │  NEW:  laneExistsAt(lane, seg): Boolean                           │  │
  │  │  Does: changeLane(), forceMerge() — now checks laneExistsAt()     │  │
  │  │  Does: segmentOffsets, toCumulative (unchanged — geometry same)   │  │
  │  │  Does: display all pathways                                       │  │
  │  └────────────────────────────────────────────────────────────────────┘  │
  │       │           │           │           │           │                  │
  │       ▼           ▼           ▼           ▼           ▼                  │
  │  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐          │
  │  │Pathway_0│ │Pathway_1│ │Pathway_2│ │Pathway_3│ │Pathway_4│          │
  │  │         │ │         │ │         │ │         │ │         │          │
  │  │ NO vList│ │ NO vList│ │ NO vList│ │ NO vList│ │ NO vList│ ← GONE  │
  │  │         │ │         │ │         │ │         │ │         │          │
  │  │ seg[0]✓ │ │ seg[0]✓ │ │ seg[0]✓ │ │ seg[0]✓ │ │ seg[0]✓ │          │
  │  │ seg[1]✓ │ │ seg[1]✓ │ │ seg[1]✓ │ │ seg[1]✓ │ │ seg[1]_ │ ← None  │
  │  │ seg[2]✓ │ │ seg[2]✓ │ │ seg[2]✓ │ │ seg[2]_ │ │ seg[2]_ │ ← None  │
  │  │ seg[3]✓ │ │ seg[3]✓ │ │ seg[3]_ │ │ seg[3]_ │ │ seg[3]_ │ ← None  │
  │  └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘          │
  │                               lane profile: 5, 5, 3, 2                 │
  └──────────────────────────────────────────────────────────────────────────┘

  And inside each VTransport that DOES exist:
  ┌──────────────────────────────────────────────────────────────────┐
  │  VTransport (one road segment in one lane)                      │
  │                                                                 │
  │  EXISTING:  vdeque: ArrayDeque[Vehicle]  → kept for density()   │
  │  NEW:       vList:  DoublyLinkedList[Vehicle]  ← car-following  │
  │  NEW:       addToAlist(actor, other)                             │
  │  NEW:       removeFromAlist(actor)                               │
  │  NEW:       getFirst → vList.head                                │
  │  NEW:       getLast  → vList.last                                │
  │  EXISTING:  move()  (unchanged — still calls motion.updateV)    │
  │  EXISTING:  snapshotDensity()  (still uses vdeque.size)         │
  └──────────────────────────────────────────────────────────────────┘
```

### Pathway's New Role: Lane Coordinator (Not DLL Owner)

```
  BEFORE — Pathway did 5 things:
  ──────────────────────────────
  ┌──────────────────────────────────────────────────────────────────┐
  │  1. Own the DLL (vList)                    → MOVES TO VTransport│
  │  2. Own the seg[] array                    → STAYS (but sparse) │
  │  3. addToAlist / removeFromAlist           → DELEGATES to VT    │
  │  4. getFirst / getLast (lane-wide)         → DELEGATES to VT    │
  │  5. display() (draw all segments)          → STAYS (skip None)  │
  └──────────────────────────────────────────────────────────────────┘

  AFTER — Pathway becomes a thin coordinator:
  ──────────────────────────────────────────────
  ┌──────────────────────────────────────────────────────────────────┐
  │  class Pathway:                                                  │
  │                                                                  │
  │    seg: Array[Option[VTransport]]    // sparse — None = no lane  │
  │                                                                  │
  │    def addToAlist(actor, other, segId):                           │
  │        seg(segId).get.addToAlist(actor, other)   // delegate     │
  │                                                                  │
  │    def removeFromAlist(actor, segId):                             │
  │        seg(segId).get.removeFromAlist(actor)     // delegate     │
  │                                                                  │
  │    def existsAt(segId): Boolean =                                │
  │        segId >= 0 && segId < seg.length && seg(segId).isDefined  │
  │                                                                  │
  │    def display(): // draw only the segments that exist           │
  │        for s <- seg.indices if seg(s).isDefined do               │
  │            seg(s).get.display()                                  │
  └──────────────────────────────────────────────────────────────────┘

  Pathway LOSES: vList (the lane-spanning DLL)
  Pathway KEEPS: seg[] array (now sparse), display(), lane identity
  Pathway GAINS: existsAt(segId) query
  Pathway DELEGATES: addToAlist, removeFromAlist, getFirst, getLast → VTransport
```

### Route's New Logic: Lane-Existence Awareness

```
  BEFORE — Route assumed all lanes exist everywhere:
  ──────────────────────────────────────────────────
  Route(numLanes = 5, ...)
      pathway = Array[Pathway](5)     // 5 identical full-length Pathways
      changeLane(l1, l2, car, seg):
          pathway(l1).removeFromAlist(car)
          pathway(l2).addToAlist(car, vAhead)     // assumes l2 exists at seg
      forceMerge(l1, range, car, seg):
          // scans all lanes in range — assumes all exist

  AFTER — Route checks lane existence per segment:
  ─────────────────────────────────────────────────
  Route(lanesPerSeg: Array[Int], ...)            // e.g., Array(5,6,4,2,4,5,4)
      maxLanes = lanesPerSeg.max                  // e.g., 6
      pathway = Array[Pathway](maxLanes)          // 6 Pathways, some sparse

      def laneExistsAt(lane: Int, seg: Int): Boolean =
          lane < lanesPerSeg(seg)

      changeLane(l1, l2, car, seg):
          if !laneExistsAt(l2, seg) then return false   // ← NEW guard
          pathway(l1).removeFromAlist(car, seg)          // delegate with segId
          pathway(l2).addToAlist(car, vAhead, seg)       // delegate with segId

      forceMerge(l1, range, car, seg):
          val availHere = range.filter(laneExistsAt(_, seg))  // ← NEW filter
          // scan only lanes that actually exist at this segment
```

### driveHighway's New Logic: Lane-End Merge

```
  BEFORE — blindly drives all segments:
  ──────────────────────────────────────
  while seg < hwLen do
      route.pathway(laneID).seg(seg).move()    // assumes seg always exists
      junc(seg + 1).jump()
      seg += 1

  AFTER — checks lane existence, forces merge if lane ends:
  ──────────────────────────────────────────────────────────
  while seg < hwLen && !diverted do
      route.pathway(laneID).seg(seg).get.move()   // .get — we know it exists
      junc(seg + 1).jump()

      // ── NEW: check if my lane continues at the NEXT segment ──
      if seg + 1 < hwLen && !route.laneExistsAt(laneID, seg + 1) then
          // Lane ends! Must merge before moving to seg+1
          val available = (0 until route.lanesAt(seg + 1))  // lanes that exist
          laneID = route.forceMerge(laneID, available, this, seg)
      end if

      seg += 1
  end while
```

### The Full Lifecycle of a Car (After Unification)

```
  ┌─────────────────────────────────────────────────────────────────────────┐
  │  1. Car spawns from VSource, assigned laneID = 2                       │
  │                                                                        │
  │  2. Car enters Route at seg[0]:                                        │
  │     pathway(2).seg(0).get.addToAlist(car, carAhead)                    │
  │     → car.myPathNode points into seg[0]'s DLL                         │
  │     → seg[0].vdeque also tracks car (for density)                      │
  │                                                                        │
  │  3. Car drives seg[0]:                                                 │
  │     seg[0].move()                                                      │
  │       → motion.updateV() → car.myPathNode.ahead → leader in seg[0]    │
  │       → if ahead==null → boundary: seg[1].vList.last                   │
  │       → on exit: seg[0].removeFromAlist(car)                           │
  │                                                                        │
  │  4. Car transitions to seg[1]:                                         │
  │     seg(1).get.addToAlist(car, carAhead_in_seg1)                       │
  │     → car.myPathNode NOW points into seg[1]'s DLL                     │
  │                                                                        │
  │  5. Before moving to seg[2], driveHighway checks:                      │
  │     Does lane 2 exist at seg[2]?                                       │
  │     ├── YES → continue driving seg[2].move()                           │
  │     └── NO  → forceMerge(2, availableLanes(seg2), car, seg1)           │
  │              → car is now on lane 1, seg[1]'s VTransport DLL           │
  │              → continue driving lane 1 from seg[2]                     │
  │                                                                        │
  │  6. Car reaches corridor end:                                          │
  │     pathway(laneID).seg(lastSeg).get.removeFromAlist(car)              │
  │     sink.leave()                                                       │
  └─────────────────────────────────────────────────────────────────────────┘
```

### One Detail: Segment Transition = DLL Re-insertion

```
  In the CURRENT code, segment transition is invisible to the DLL:
  ─────────────────────────────────────────────────────────────────
  Car drives seg[0].move() → exits → drives seg[1].move()
  The DLL doesn't change. Car stays in the same Pathway.vList node.
  Only the vdeque updates (remove from seg[0], add to seg[1]).

  In the PROPOSED code, segment transition = DLL hop:
  ────────────────────────────────────────────────────
  Car drives seg[0].move() → exits →
      seg[0].removeFromAlist(car)           // remove from seg[0]'s DLL
      seg[1].addToAlist(car, carAhead)      // insert into seg[1]'s DLL
      seg[1].move()                         // drive seg[1]

  This is NEW work per segment. But it's O(1) — DLL insert/remove is O(1).
  And it's the SAME work that addToAlist/removeFromAlist already does.
  The segment transition just becomes explicit instead of invisible.

  WHERE does this happen?  In driveHighway (or a helper called by move()):

      while seg < hwLen do
          pathway(laneID).seg(seg).get.move()         // drive this segment
          // → on exit, move() removes car from seg[seg]'s DLL

          if seg + 1 < hwLen then
              // Re-insert into next segment's DLL
              val nextVT = pathway(laneID).seg(seg + 1).get
              val ahead  = nextVT.getLast               // car closest to boundary
              nextVT.addToAlist(car, ahead)              // O(1) insert
          end if

          junc(seg + 1).jump()
          seg += 1
      end while
```

---

## 8. The Key Insight (One Picture)

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

---

## 9. "How Is This Not a Skip List?"

Good question. The sparse lane diagram _looks_ like a skip list:

```
  Lane 0: seg[0]──seg[1]──seg[2]──seg[3]──seg[4]──seg[5]──seg[6]   ← level 0 (always full)
  Lane 1: seg[0]──seg[1]──seg[2]──seg[3]──seg[4]──seg[5]──seg[6]   ← level 1 (always full)
  Lane 2: seg[0]──seg[1]──seg[2]──  ___  ──seg[4]──seg[5]──seg[6]   ← level 2 (gap)
  Lane 3: seg[0]──seg[1]──  ___  ──  ___  ──  ___  ──seg[5]──  ___   ← level 3 (sparse)
  Lane 4: seg[0]──  ___  ──  ___  ──  ___  ──  ___  ──seg[5]──  ___   ← level 4 (very sparse)
  Lane 5:   ___  ──seg[1]──  ___  ──  ___  ──  ___  ──  ___  ──  ___   ← level 5 (one node)
```

A classic skip list:
```
  Level 3:  [3] ─────────────────────────────── [25] ──────────── [NIL]
  Level 2:  [3] ──────── [9] ──────────── [19] ─ [25] ──────────── [NIL]
  Level 1:  [3] ── [6] ─ [9] ── [12] ── [19] ─ [25] ── [26] ─── [NIL]
  Level 0:  [3] ── [6] ─ [9] ── [12] ── [19] ─ [25] ── [26] ─── [NIL]
```

They share the visual shape: lower levels are denser, upper levels are sparser.
But they are **fundamentally different** in purpose, traversal, and semantics:

```
  ┌─────────────────────────┬───────────────────────────────────────────────┐
  │     SKIP LIST           │     SPARSE LANE ARRAY                        │
  ├─────────────────────────┼───────────────────────────────────────────────┤
  │                         │                                               │
  │  PURPOSE:               │  PURPOSE:                                     │
  │  Fast search O(log n)   │  Model physical road geometry                │
  │  in sorted data         │  (lanes that exist/don't exist)              │
  │                         │                                               │
  │  TRAVERSAL:             │  TRAVERSAL:                                   │
  │  Vertical — start at    │  Horizontal ONLY — a car drives              │
  │  top level, drop down   │  seg[0] → seg[1] → seg[2] → ...             │
  │  to find target key     │  within ONE lane, never jumps                │
  │                         │  up/down levels to "skip ahead"              │
  │                         │                                               │
  │  LEVELS = SHORTCUTS:    │  LANES = PARALLEL ROADS:                     │
  │  Higher level = fewer   │  Higher lane = a separate physical           │
  │  nodes = skip over      │  road. NOT a shortcut. Cars in               │
  │  nodes for faster       │  lane 4 don't skip past seg[2].             │
  │  lookup.                │  They MERGE into lane 1 because              │
  │                         │  their lane ENDS.                            │
  │                         │                                               │
  │  NODES SHARED:          │  NODES INDEPENDENT:                          │
  │  Same key appears at    │  seg[0] in Lane 0 and seg[0] in             │
  │  multiple levels.       │  Lane 3 are DIFFERENT VTransports.           │
  │  Level 2 node for       │  Different DLLs. Different cars.             │
  │  key=9 IS the same      │  A car on Lane 3 seg[0] is NOT              │
  │  record as level 0      │  on Lane 0 seg[0].                          │
  │  node for key=9.        │                                               │
  │                         │                                               │
  │  RANDOMIZED:            │  DETERMINISTIC:                              │
  │  Promotion to higher    │  Lane existence at each segment is           │
  │  levels is probabilistic│  FIXED by physical road geometry.            │
  │  (coin flip per insert) │  Lane 4 exists at seg[0] because            │
  │                         │  the PeMS sensor says 5 lanes.              │
  │                         │                                               │
  │  VERTICAL LINKS:        │  VERTICAL LINKS = LANE CHANGES:             │
  │  Built into every node  │  A car can changeLane() from                │
  │  (drop-down pointers)   │  Lane 2 → Lane 1, but only at              │
  │                         │  segments where BOTH lanes exist.            │
  │                         │  This is NOT a skip operation.               │
  └─────────────────────────┴───────────────────────────────────────────────┘
```

### The Critical Difference: What "Gap" Means

```
  SKIP LIST — gap means "skip over for speed":
  ─────────────────────────────────────────────
  Level 2:  [3] ──────────────── [19] ──── [NIL]
  Level 0:  [3] ── [6] ── [9] ── [19] ── [NIL]

  To find key=9: start at level 2 → [3] → skip to [19] → too far →
                 drop to level 0 → [3] → [6] → [9] ✓ found!

  The gap at level 2 between [3] and [19] is a SHORTCUT.
  You traverse it to AVOID visiting [6] and [9].


  SPARSE LANE ARRAY — gap means "road doesn't exist here":
  ─────────────────────────────────────────────────────────
  Lane 3:  seg[0] ── seg[1] ──  ___  ──  ___  ──  ___  ── seg[5]
  Lane 0:  seg[0] ── seg[1] ── seg[2] ── seg[3] ── seg[4] ── seg[5]

  Car C7 on Lane 3, seg[1]:
  → C7 CANNOT "skip" from seg[1] to seg[5].
  → C7 MUST merge into Lane 0/1/2 (whichever exists at seg[2]).
  → C7 drives EVERY segment in between — seg[2], seg[3], seg[4] — on a lower lane.

  The gap is NOT a shortcut. It's a WALL. The car must leave the lane.
```

### Summary

```
  Skip list:          gaps = shortcuts (go FASTER by skipping nodes)
  Sparse lane array:  gaps = walls     (lane ENDS, car must MERGE)

  Structurally similar?  Yes — both are sparse arrays of linked lists.
  Semantically the same? No — completely different invariants and traversal.
  
  A skip list optimizes SEARCH across one sorted dimension.
  The sparse lane array models PARALLEL INDEPENDENT roads that start/end
  at different physical locations along the corridor.
```

---

## 10. How Do Ramps Work in the New Design?

### Current Ramp Architecture (unchanged by unification)

A Ramp is already a **self-contained** single-segment component with its own DLL:

```
  ┌─────────────────────────────────────────────────────────────┐
  │  Ramp (e.g., "ON_ramp_3")                                  │
  │                                                             │
  │  lane: VTransport    ← single segment (from → to junction) │
  │  vList: DLL          ← its OWN DLL, separate from Pathway  │
  │  mode: On | Off      ← ramp direction                      │
  │                                                             │
  │  addToAlist(actor, other)    ← operates on Ramp's vList     │
  │  removeFromAlist(actor)      ← operates on Ramp's vList     │
  └─────────────────────────────────────────────────────────────┘
```

**Key fact:** The Ramp already has its own DLL. It does NOT share Pathway's DLL.
This is exactly the pattern we're proposing for VTransport — each component
owns its own DLL. The Ramp already does what we want VTransport to do.

### Current Ramp Flow (from EatonFireModel.actOnCorridor)

```
  On-Ramp car lifecycle:
  ──────────────────────

  1. Car spawns at VSource (subtype = nLanes + rampIdx)
     laneID = nLanes - 1                     ← assigned to rightmost lane

  2. Drive the ramp:
     ramp.addToAlist(car, carAhead)           ← enters Ramp's DLL
     ramp.lane.move()                         ← drives the single VTransport
     ramp.removeFromAlist(car)                ← exits Ramp's DLL

  3. Merge into mainline at joinSeg:
     carAhead = route.pathway(laneID).seg(joinSeg).getLast
     route.pathway(laneID).addToAlist(car, carAhead)  ← enters Pathway's DLL
     junc(joinSeg).jump()
     driveHighway(route, junc, sinks, hwLen, joinSeg)  ← drives from joinSeg

  Visually:
                                   joinSeg
  ═══ seg[0] ═══ seg[1] ═══ seg[2] ═══ seg[3] ═══ seg[4] ═══  (mainline)
                                 ╱
                      ramp.lane ╱  ← single VTransport with own DLL
                               ╱
                          VSource
```

### After Unification: What Changes for Ramps?

**Almost nothing.** The Ramp already follows the per-component-DLL pattern.
The changes are at the **merge point** — where the ramp car enters the mainline.

```
  BEFORE:
  ───────
  // After driving the ramp, merge into mainline
  laneID = nLanes - 1                                      // rightmost lane
  carAhead = route.pathway(laneID).seg(joinSeg).getLast     // uses VT deque
  route.pathway(laneID).addToAlist(car, carAhead)           // adds to PATHWAY's DLL
  driveHighway(route, junc, sinks, hwLen, joinSeg)

  AFTER:
  ──────
  // After driving the ramp, merge into mainline
  laneID = nLanes - 1                                      // rightmost lane
  // NEW: check that the rightmost lane EXISTS at joinSeg
  if !route.laneExistsAt(laneID, joinSeg) then
      laneID = route.lanesAt(joinSeg) - 1                   // use outermost existing lane
  end if
  val vt = route.pathway(laneID).seg(joinSeg).get           // get the VTransport
  carAhead = vt.getLast                                      // uses VT's DLL (not deque)
  vt.addToAlist(car, carAhead)                               // adds to VTRANSPORT's DLL
  driveHighway(route, junc, sinks, hwLen, joinSeg)
```

The key difference: the car is added to the **VTransport's DLL** at the
specific segment, not to a lane-spanning Pathway DLL.

### The Interesting Case: Ramp Joins at a Segment With Fewer Lanes

```
  I-210 WB lane profile:   5    6    4    2    4    5    4
                          seg0  seg1  seg2  seg3  seg4  seg5  seg6

  What if a ramp joins at seg[3] (which only has 2 lanes)?

  Currently: laneID = nLanes - 1 = 4 (rightmost of 5)
             → seg[3] has no lane 4 → BUG (index out of bounds or phantom lane)

  After:     laneID = nLanes - 1 = 4
             → route.laneExistsAt(4, 3) = false
             → laneID = route.lanesAt(3) - 1 = 1  (outermost existing lane)
             → car merges into lane 1 at seg[3] ← correct!

  Visually:
  Lane 0: ═══ seg[3] ═══  ← exists
  Lane 1: ═══ seg[3] ═══  ← exists ← ramp merges HERE
  Lane 2:      ___         ← doesn't exist
  Lane 3:      ___         ← doesn't exist
  Lane 4:      ___         ← doesn't exist
                    ╱
          ramp.lane╱
                  ╱
             VSource

  The variable lane design actually FIXES a latent bug:
  ramps that join at narrow segments now merge into the correct lane.
```

### FFConnector: Also Unchanged

```
  FFConnector already has the same self-contained pattern as Ramp:

  ┌────────────────────────────────────────────────────────────────┐
  │  FFConnector (e.g., "FF_I210_to_SR134")                       │
  │                                                                │
  │  lane: VTransport    ← single segment (fromJunc → toJunc)     │
  │  vList: DLL          ← its OWN DLL                            │
  │  addToAlist / removeFromAlist                                  │
  └────────────────────────────────────────────────────────────────┘

  Same lifecycle as Ramp:
  1. Remove car from source corridor's VTransport DLL
  2. Add to FFConnector's DLL → drive lane → remove from DLL
  3. Add to destination corridor's VTransport DLL at merge segment

  Change needed: same as Ramp — at the merge point, check that
  the target lane exists at the merge segment on corridor B.
```

### Summary: Ramp/FFConnector Impact

```
  ┌────────────────────┬──────────────────────────────────────────────┐
  │  Component         │  Impact of DLL Unification                   │
  ├────────────────────┼──────────────────────────────────────────────┤
  │  Ramp.vList        │  NONE — already owns its own DLL             │
  │  Ramp.lane         │  NONE — single VTransport, unchanged         │
  │  Ramp merge point  │  SMALL — add laneExistsAt() guard,           │
  │                    │  merge into VTransport DLL (not Pathway DLL) │
  │  FFConnector.vList │  NONE — already owns its own DLL             │
  │  FFConnector.lane  │  NONE — single VTransport, unchanged         │
  │  FF merge point    │  SMALL — same guard as Ramp                  │
  ├────────────────────┼──────────────────────────────────────────────┤
  │  Total new code    │  ~5 lines per merge point (lane existence    │
  │                    │  check + fallback to outermost existing lane)│
  └────────────────────┴──────────────────────────────────────────────┘

  Ramp and FFConnector already prove the per-component-DLL pattern works.
  They've been doing it all along — separate from Pathway's lane-spanning DLL.
  Unification just makes VTransport follow the same pattern they already use.
```

---

## 11. Implementation Blueprint

### Guiding Principle

**Backward-compatible first.** CalRoute101_3 (uniform 4-lane) must compile and
run identically after every task. We add variable-lane capability without breaking
the uniform-lane path. Every task ends with `sbt compile` green.

### Task Order & Dependencies

```
  Task 0: Git branch
     │
     ▼
  Task 1: VTransport gets DLL ──────────────────────────┐
     │                                                    │
     ▼                                                    │
  Task 2: Pathway delegates to VTransport DLL             │  These 3 are
     │                                                    │  the CORE
     ▼                                                    │  engine change
  Task 3: Dynamics — remove segId patch,                  │
          add cross-boundary leader lookup               │
     │                                                    │
     ▼ ───────────────────────────────────────────────────┘
  Task 4: driveHighway — explicit DLL re-insertion
          at segment transitions (CalRoute101_3 + Eaton)
     │
     ▼
  Task 5: Validate — CalRoute101_3 runs identically
     │
     ║  ← CHECKPOINT: uniform lanes work with new DLL ownership
     ║    (nothing above this line changes lane counts)
     ▼
  Task 6: Route — accept Array[Int] lane profile
     │
     ▼
  Task 7: Pathway — sparse seg array
     │
     ▼
  Task 8: NetworkConfig + EatonCorridorConfig — feed lane array
     │
     ▼
  Task 9: CorridorBuilder — pass lane array to Route
     │
     ▼
  Task 10: driveHighway — lane-end detection + forceMerge
     │
     ▼
  Task 11: Ramp/FFConnector merge-point guards
     │
     ▼
  Task 12: Validate — EatonFireModel with real lane profile
```

---

### Task 0: Git Branch

**Create a feature branch before touching any code.**

```bash
git checkout -b feature/variable-lane-dll-unification
```

No code changes. Just a safety net.

---

### Task 1: VTransport Gets Its Own DLL

**File:** `VTransport.scala`
**Goal:** Add a DLL to VTransport. Keep the existing vdeque for density. Add
`addToAlist` / `removeFromAlist` / `getFirst` / `getLast` that use the DLL.

```
  CURRENT VTransport (line 38-52):
  ────────────────────────────────
  class VTransport (...) extends Transport (...):
      val vdeque = ArrayDeque[Vehicle]()        // bookkeeping only
      def getFirst: Vehicle = vdeque.head       // uses deque
      def getLast:  Vehicle = vdeque.last        // uses deque

  AFTER:
  ──────
  class VTransport (...) extends Transport (...):
      val vdeque = ArrayDeque[Vehicle]()        // kept for density/HUD stats
      val vList  = DoublyLinkedList[Vehicle]     // NEW: car-following DLL
      val dllId  = s"DLL_${name}"               // NEW: debug label

      // --- NEW DLL-based methods (same signatures as Ramp/FFConnector) ---
      def addToAlist(actor: Vehicle, other: Vehicle): Unit =
          val otherNode = if other != null
              then other.myPathNode.asInstanceOf[vList.Node] else null
          actor.myPathNode = vList.add(actor, otherNode)
          actor.pathInfo = dllId
      end addToAlist

      def removeFromAlist(actor: Vehicle): Unit =
          vList.remove(actor.myPathNode.asInstanceOf[vList.Node])
          actor.myPathNode = null
      end removeFromAlist

      // --- EXISTING getFirst/getLast: switch from deque to DLL ---
      def getFirst: Vehicle = if vList.isEmpty then null else vList.head
      def getLast:  Vehicle = if vList.isEmpty then null else vList.last

      // --- density still uses deque (count-only, no ordering needed) ---
      def snapshotDensity(): Double =
          if length > 0.0 then vdeque.size.toDouble / length else 0.0
```

**What changes in `move()` (line 93-137):**
- `vdeque += actor` (line 112) stays — density tracking
- `vdeque -= actor` (line 126) stays — density tracking
- No other changes to move() in this task

**Compile check:** `sbt compile` — VTransport now has both vdeque and vList.
Nothing calls the new methods yet. Zero behavioral change.

---

### Task 2: Pathway Delegates to VTransport's DLL

**File:** `Pathway.scala`
**Goal:** Pathway.addToAlist / removeFromAlist now delegate to
`seg(segId).addToAlist(...)` instead of operating on Pathway.vList.
The Pathway.vList field is **kept but unused** (deprecate, remove later).

```
  CURRENT Pathway.addToAlist (line 75-84):
  ─────────────────────────────────────────
  def addToAlist(actor: Vehicle, other: Vehicle): Unit =
      val otherNode = if other != null
          then other.myPathNode.asInstanceOf[vList.Node] else null
      actor.myPathway = this
      actor.myFFConnector = null
      actor.myRamp = null
      actor.myPathNode = vList.add(actor, otherNode)       // ← Pathway DLL
      actor.pathInfo = s"${dllId}"

  AFTER (overloaded — add segId parameter):
  ──────────────────────────────────────────
  // NEW: segment-aware delegation
  def addToAlist(actor: Vehicle, other: Vehicle, segId: Int): Unit =
      actor.myPathway = this
      actor.myFFConnector = null
      actor.myRamp = null
      seg(segId).addToAlist(actor, other)                   // ← VTransport DLL

  // KEEP OLD signature for backward compat during migration
  // (CalRoute101_3 calls addToAlist(car, carAhead) without segId)
  // This overload delegates to seg(0) — will be removed after Task 4.
  def addToAlist(actor: Vehicle, other: Vehicle): Unit =
      addToAlist(actor, other, 0)                           // default to seg 0

  CURRENT Pathway.removeFromAlist (line 90-95):
  ──────────────────────────────────────────────
  def removeFromAlist(actor: Vehicle): Unit =
      vList.remove(actor.myPathNode.asInstanceOf[vList.Node])
      actor.myPathNode = null
      actor.myPathway  = null

  AFTER:
  ──────
  def removeFromAlist(actor: Vehicle, segId: Int): Unit =
      seg(segId).removeFromAlist(actor)
      actor.myPathway = null

  // KEEP OLD signature for backward compat during migration
  def removeFromAlist(actor: Vehicle): Unit =
      // Find which seg the car is in via actor.segId
      if actor.segId >= 0 && actor.segId < seg.length then
          seg(actor.segId).removeFromAlist(actor)
      actor.myPathway = null
```

**Compile check:** `sbt compile` — old call sites still work via overload.
New call sites can pass segId for precision.

---

### Task 3: Dynamics — Cross-Boundary Leader Lookup

**File:** `Dynamics.scala`
**Goal:** In all three CFMs (IDM, Gipps, Krauss), replace the DLL-based leader
lookup + segId patch with: (1) same-segment DLL lookup, (2) cross-boundary
lookup via next VTransport's DLL.

**The car needs to know its current Pathway and segId to do boundary lookup.**
It already has both: `car.myPathway` and `car.segId`.

```
  CURRENT IDMDynamics.updateM (lines 321-335):
  ─────────────────────────────────────────────
  val ref = car.myPathNode.ahead
  val car_ahead = if ref != null then ref.elem else null
  val (x_leader, v_leader) =
      if car_ahead == null then
          (car.t_disp + 1000.0, car.velocity)           // no leader: free-flow
      else if car_ahead.segId < car.segId then
          (car.t_disp + 1000.0, car.velocity)           // PATCH: stale DLL node
      else if car_ahead.t_disp - car.t_disp > FREERANGE then
          (car.t_disp + 1000.0, car.velocity)           // far ahead: free-flow
      else
          (car_ahead.t_disp, car_ahead.velocity)         // real leader

  AFTER:
  ──────
  // Step 1: within-segment leader (DLL is now per-VTransport)
  val ref = car.myPathNode.ahead
  var car_ahead = if ref != null then ref.elem else null

  // Step 2: if no within-segment leader, cross-boundary lookup
  if car_ahead == null && car.myPathway != null then
      val segs = car.myPathway.seg
      val nextIdx = car.segId + 1
      if nextIdx < segs.length && segs(nextIdx) != null then
          car_ahead = segs(nextIdx).getLast              // tail of next seg's DLL
  end if

  // Step 3: compute leader state (NO segId patch needed)
  val (x_leader, v_leader) =
      if car_ahead == null then
          (car.t_disp + 1000.0, car.velocity)           // no leader: free-flow
      else if car_ahead.t_disp - car.t_disp > FREERANGE then
          (car.t_disp + 1000.0, car.velocity)           // far ahead: free-flow
      else
          (car_ahead.t_disp, car_ahead.velocity)         // real leader
```

**Same pattern applies to:**
- `GippsDynamics.gipps()` (line 130-138) — remove `cp.segId < cn.segId` check
- `KraussDynamics.updateM()` (line 211-217) — remove `car_ahead.segId < car.segId` check

**Extract helper to avoid duplication (optional but clean):**
```scala
// In Vehicle.scala or Dynamics trait:
def findLeader(car: Vehicle): Vehicle =
    val ref = car.myPathNode.ahead
    if ref != null then ref.elem
    else if car.myPathway != null then
        val segs = car.myPathway.seg
        val nextIdx = car.segId + 1
        if nextIdx < segs.length && segs(nextIdx) != null
        then segs(nextIdx).getLast
        else null
    else null
```

**Compile check:** `sbt compile`

---

### Task 4: driveHighway — Explicit DLL Re-insertion at Segment Transitions

**Files:** `CalRoute101_3.scala`, `EatonFireModel.scala`
**Goal:** When a car finishes `seg(n).move()` and advances to `seg(n+1)`,
explicitly remove from seg(n)'s DLL and insert into seg(n+1)'s DLL.

**Currently** this is invisible — the car stays in Pathway's lane-spanning DLL
across all segments. After Task 2, the DLL is per-VTransport, so segment
transitions must explicitly hop the DLL.

```
  CURRENT CalRoute101_3.driveHighway (lines 194-221):
  ───────────────────────────────────────────────────
  cfor (joinSeg, highway_length) { seg =>
      // lane change logic...
      route.pathway(laneID).seg(seg).move()
      if junc(seg + 1).name.startsWith("sensor") then
          junc(seg + 1).jump()
  }
  route.pathway(laneID).removeFromAlist(this)
  sinks.head.leave()

  AFTER:
  ──────
  cfor (joinSeg, highway_length) { seg =>
      // lane change logic...
      route.pathway(laneID).seg(seg).move()

      // ── DLL hop: exit this segment, enter next ──
      route.pathway(laneID).seg(seg).removeFromAlist(this)
      if seg + 1 < highway_length then
          val nextVT = route.pathway(laneID).seg(seg + 1)
          val ahead  = nextVT.getLast
          nextVT.addToAlist(this, ahead)
      end if

      if junc(seg + 1).name.startsWith("sensor") then
          junc(seg + 1).jump()
  }
  // Final removal already handled by last seg's removeFromAlist above
  sinks.head.leave()
```

**Same pattern in EatonFireModel.driveHighway (lines 373-415).**

**Entry point also changes:**
```
  CURRENT (CalRoute101_3 line 181-183):
  route.pathway(laneID).addToAlist(this, carAhead)   // Pathway DLL
  junc(0).jump()

  AFTER:
  val vt = route.pathway(laneID).seg(0)
  val carAhead = vt.getLast
  vt.addToAlist(this, carAhead)                       // VTransport DLL
  this.myPathway = route.pathway(laneID)              // set pathway ref
  junc(0).jump()
```

**Compile check:** `sbt compile`

---

### Task 5: Validate — CalRoute101_3 Runs Identically

**No code changes.** Run the existing model and compare output.

```bash
sbt "runMain scalation.simulation.process.model.runCalRoute101_3"
```

**Acceptance criteria:**
- Compiles without errors
- Simulation runs to completion (no crashes, no hangs)
- Vehicle count at sink matches pre-change baseline
- No new assertion failures
- Animation renders correctly (if checked)

**If this fails:** Tasks 1-4 introduced a bug. Debug before proceeding.

This is the **CHECKPOINT**. Everything above changes DLL ownership without
changing lane counts. Everything below adds variable-lane capability.

```
  ═══════════════════════════════════════════════════════════════
  ██  CHECKPOINT: Uniform lanes work with per-VTransport DLL  ██
  ═══════════════════════════════════════════════════════════════
```

---

### Task 6: Route Accepts Lane Profile Array

**File:** `Route.scala`
**Goal:** Add a secondary constructor that takes `Array[Int]` (lanes per segment).
The existing `numLanes: Int` constructor remains for backward compatibility.

```
  CURRENT Route constructor (line 35-57):
  ───────────────────────────────────────
  class Route(name: String, numLanes: Int, junc: Array[Junction],
              from: Component, to: Component, motion: Dynamics, ...)

      val pathway = Array.ofDim[Pathway](numLanes)
      for i <- pathway.indices do
          val shift = calcShift2 * ((physicalLane - (numLanes-1)/2.0) * GAP)
          pathway(i) = new Pathway(s"${name}_$i", junc, from, to, ...)

  AFTER — add lanesPerSeg field + companion factory:
  ──────────────────────────────────────────────────
  class Route(name: String, numLanes: Int, junc: Array[Junction],
              from: Component, to: Component, motion: Dynamics, ...,
              val lanesPerSeg: Array[Int] = null)            // NEW param

      // If lanesPerSeg is null, all segments have numLanes (backward compat)
      private val _lanesPerSeg: Array[Int] =
          if lanesPerSeg != null then lanesPerSeg
          else Array.fill(junc.length + 1)(numLanes)         // +1 = nSegments

      val maxLanes: Int = _lanesPerSeg.max

      // NEW: query methods
      def lanesAt(seg: Int): Int = _lanesPerSeg(seg)
      def laneExistsAt(lane: Int, seg: Int): Boolean = lane < _lanesPerSeg(seg)

      val pathway = Array.ofDim[Pathway](maxLanes)
      // Pathway creation changes in Task 7

      // changeLane — add guard:
      def changeLane(l1: Int, l2: Int, actor: Vehicle, seg: Int): Boolean =
          if !laneExistsAt(l2, seg) then return false       // NEW guard
          // ... rest unchanged ...

      // forceMerge — filter available lanes:
      def forceMerge(l1: Int, availLanes: Range, car: Vehicle, seg: Int): Int =
          // NEW: only consider lanes that exist at this segment
          val available = availLanes.filter(laneExistsAt(_, seg))
          // ... rest uses `available` instead of `availLanes` ...
```

**Backward compat:** When `lanesPerSeg = null` (default), `_lanesPerSeg` is
filled with `numLanes`. Every query returns uniform. CalRoute101_3 sees no change.

**Compile check:** `sbt compile`

---

### Task 7: Pathway — Sparse Segment Array

**File:** `Pathway.scala`
**Goal:** Pathway's `seg` array becomes `Array[Option[VTransport]]` (or we use
`null` for absent segments). Only create VTransports where the lane exists.

```
  CURRENT Pathway constructor (lines 44-54):
  ──────────────────────────────────────────
  val seg = Array.ofDim[VTransport](points.length - 1)
  for i <- 0 until points.length - 1 do
      seg(i) = new VTransport(s"${name}_seg${i}", p1, p2, ...)

  AFTER — accept optional laneIndex + lanesPerSeg to know where to create:
  ────────────────────────────────────────────────────────────────────────
  class Pathway(name: String, junc: Array[Junction], from: Component,
                to: Component, motion: Dynamics, ...,
                laneIndex: Int = 0,                        // NEW: which lane am I
                lanesPerSeg: Array[Int] = null)            // NEW: lane profile

      val seg = Array.ofDim[VTransport](points.length - 1)
      for i <- 0 until points.length - 1 do
          val exists = lanesPerSeg == null || laneIndex < lanesPerSeg(i)
          if exists then
              seg(i) = new VTransport(s"${name}_seg${i}", p1, p2, ...)
              subpart += seg(i)
          else
              seg(i) = null                                // lane doesn't exist here

      def existsAt(segId: Int): Boolean =
          segId >= 0 && segId < seg.length && seg(segId) != null

      // display: skip null segments
      override def display(): Unit =
          for s <- seg.indices if seg(s) != null do
              val segment = seg(s)
              director.animate(segment, CreateEdge, blue, ...)
```

**Backward compat:** When `lanesPerSeg = null` (default), `exists` is always
true → all segments created → identical to current behavior.

**Compile check:** `sbt compile`

---

### Task 8: Config Layer — Feed Lane Array

**Files:** `NetworkConfig.scala`, `EatonCorridorConfig.scala`

```
  CURRENT NetworkConfig.MainlineSpec (line 75):
  ─────────────────────────────────────────────
  case class MainlineSpec(id: String, segments: Int, lanesPerSegment: Int, ...)

  AFTER:
  ──────
  case class MainlineSpec(id: String, segments: Int, lanesPerSegment: Int,
                          lanesPerSeg: Option[Array[Int]] = None, ...)
                          // NEW: per-segment lane counts
                          // If None, all segments use lanesPerSegment (backward compat)

  CURRENT EatonCorridorConfig (lines 372-375):
  ─────────────────────────────────────────────
  val laneCounts = new Array[Int](nML)
  cfor(0, nML) { i => laneCounts(i) = mlStations(i).record.lanes }
  val entryIdx = if flowDir == FlowDirection.Descending then nML - 1 else 0
  val lanesPerSegment = laneCounts(entryIdx)    // ← DISCARDS per-segment data

  AFTER:
  ──────
  val laneCounts = new Array[Int](nML)
  cfor(0, nML) { i => laneCounts(i) = mlStations(i).record.lanes }
  val entryIdx = if flowDir == FlowDirection.Descending then nML - 1 else 0
  val lanesPerSegment = laneCounts.max                     // for maxLanes
  // Compute per-SEGMENT lane count (segment i spans station i to station i+1)
  val segLaneCounts = new Array[Int](nSegments)
  cfor(0, nSegments) { i => segLaneCounts(i) = min(laneCounts(i), laneCounts(i+1)) }
  // MainlineSpec now carries both:
  MainlineSpec(id, nSegments, lanesPerSegment,
               lanesPerSeg = Some(segLaneCounts), ...)
```

**Per-segment lane count rule:** A segment between station A (6 lanes) and
station B (4 lanes) has `min(6, 4) = 4` usable lanes. The lane reduction
happens physically between the two sensors.

**Backward compat:** CalRoute101_3 uses `Option[Array[Int]] = None` → unchanged.

**Compile check:** `sbt compile`

---

### Task 9: CorridorBuilder — Pass Lane Array to Route

**File:** `CorridorBuilder.scala`

```
  CURRENT (line 101, 146):
  ────────────────────────
  val nLanes = config.mainline.lanesPerSegment
  val route = Route(s"${pfx}Rte", nLanes, intermediateJunc,
                    junc(0), junc.last, motion)

  AFTER:
  ──────
  val nLanes = config.mainline.lanesPerSegment
  val lps    = config.mainline.lanesPerSeg.orNull          // Array[Int] or null
  val route  = Route(s"${pfx}Rte", nLanes, intermediateJunc,
                     junc(0), junc.last, motion,
                     lanesPerSeg = lps)                     // NEW param
```

**Backward compat:** When `lanesPerSeg = None` (CalRoute101_3), `lps = null`,
Route gets `null` → uses uniform `numLanes` everywhere.

**Compile check:** `sbt compile`

---

### Task 10: driveHighway — Lane-End Detection + Force Merge

**Files:** `CalRoute101_3.scala`, `EatonFireModel.scala`
**Goal:** Before advancing to `seg+1`, check if the car's lane exists there.
If not, force merge into an adjacent lane.

```
  AFTER (extends Task 4's driveHighway):
  ──────────────────────────────────────
  cfor (joinSeg, highway_length) { seg =>
      // lane change logic...
      route.pathway(laneID).seg(seg).move()

      // ── DLL hop ──
      route.pathway(laneID).seg(seg).removeFromAlist(this)

      // ── NEW: lane-end check before entering next segment ──
      if seg + 1 < highway_length then
          if !route.laneExistsAt(laneID, seg + 1) then
              // Lane ends! Force merge into a lane that exists at seg+1
              val avail = 0 until route.lanesAt(seg + 1)
              laneID = route.forceMerge(laneID, avail, this, seg)
          end if
          val nextVT = route.pathway(laneID).seg(seg + 1)
          val ahead  = nextVT.getLast
          nextVT.addToAlist(this, ahead)
      end if

      if junc(seg + 1).name.startsWith("sensor") then
          junc(seg + 1).jump()
  }
  sinks.head.leave()
```

**CalRoute101_3 (uniform lanes):** `laneExistsAt` always returns true →
the `if` block never fires → zero behavioral change.

**EatonFireModel (variable lanes):** Lane-end detection triggers forceMerge →
car shifts to an existing lane before crossing the boundary.

**Compile check:** `sbt compile`

---

### Task 11: Ramp / FFConnector Merge-Point Guards

**Files:** `EatonFireModel.scala` (actOnCorridor, driveHighway FF section)

```
  CURRENT ramp merge (EatonFireModel line 358-367):
  ─────────────────────────────────────────────────
  laneID = nLanes - 1
  val carAhead = route.pathway(laneID).seg(joinSeg).getLast
  route.pathway(laneID).addToAlist(this, carAhead)

  AFTER:
  ──────
  laneID = nLanes - 1
  // NEW: ensure the target lane exists at the join segment
  if !route.laneExistsAt(laneID, joinSeg) then
      laneID = route.lanesAt(joinSeg) - 1                // outermost existing lane
  end if
  val nextVT = route.pathway(laneID).seg(joinSeg)
  val carAhead = nextVT.getLast
  nextVT.addToAlist(this, carAhead)
  this.myPathway = route.pathway(laneID)
```

**Same guard at FFConnector merge point (line 400-403).**

**Compile check:** `sbt compile`

---

### Task 12: Validate — EatonFireModel With Real Lane Profile

```bash
sbt "runMain scalation.simulation.process.model.runEatonFireModel"
```

**Acceptance criteria:**
- Compiles without errors
- I-210 WB lanes: 5→6→4→2→4→5→4 (visible in animation — lanes start/end)
- SR-134 WB lanes: 4→5→5→4→4 (or whatever station_map says)
- Cars at lane-end boundaries merge correctly (no crash, no phantom lane)
- Ramps merge into existing lanes at narrow segments
- Vehicle count at sinks is reasonable
- CalRoute101_3 still works (regression check)

---

### Summary: Lines of Code Estimate

```
  ┌────────────────────────────────┬──────────┬────────────────────────────┐
  │  Task                          │ ~LOC     │ Risk                       │
  ├────────────────────────────────┼──────────┼────────────────────────────┤
  │  1. VTransport DLL             │  ~25     │ Low — additive only        │
  │  2. Pathway delegation         │  ~30     │ Med — overload signatures  │
  │  3. Dynamics cross-boundary    │  ~20     │ Med — car-following core   │
  │  4. driveHighway DLL hop       │  ~30     │ Med — two model files      │
  │  5. Checkpoint validate        │   0      │ —                          │
  │  6. Route lane profile         │  ~25     │ Low — additive, defaulted  │
  │  7. Pathway sparse seg         │  ~15     │ Low — null check           │
  │  8. Config lanesPerSeg         │  ~10     │ Low — Optional field       │
  │  9. CorridorBuilder pass-thru  │   ~5     │ Low — one line             │
  │ 10. Lane-end detection         │  ~15     │ Med — control flow         │
  │ 11. Ramp/FF guards             │  ~10     │ Low — guard clauses        │
  │ 12. Final validate             │   0      │ —                          │
  ├────────────────────────────────┼──────────┼────────────────────────────┤
  │  TOTAL                         │ ~185     │                            │
  └────────────────────────────────┴──────────┴────────────────────────────┘
```

### File Touch Order

```
  1. VTransport.scala          ← DLL added
  2. Pathway.scala             ← delegate to VTransport
  3. Dynamics.scala            ← cross-boundary lookup (IDM, Gipps, Krauss)
  4. CalRoute101_3.scala       ← DLL hop in driveHighway
  5. EatonFireModel.scala      ← DLL hop in driveHighway
  ── CHECKPOINT ──
  6. Route.scala               ← lanesPerSeg array
  7. Pathway.scala             ← sparse seg (second touch)
  8. NetworkConfig.scala       ← MainlineSpec.lanesPerSeg
  9. EatonCorridorConfig.scala ← feed real lane counts
  10. CorridorBuilder.scala    ← pass-thru
  11. EatonFireModel.scala     ← lane-end + ramp guards (second touch)
  12. CalRoute101_3.scala      ← regression (no code change)
```

### Ready Signal

When you give the green light, we start at **Task 0** (git branch) then
**Task 1** (VTransport DLL). Each task is a single commit. We compile
after every task. We don't proceed past the checkpoint until CalRoute101_3
runs clean.

