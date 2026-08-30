===============================================================================
SCALATION TRAFFIC SIMULATION — ARCHITECTURE BLUEPRINT
===============================================================================


1. HIGH-LEVEL STRUCTURE
-------------------------------------------------------------------------------

Route
= one complete corridor / named roadway

Pathway
= one longitudinal lane across the corridor

RoadLink
= one physical road section across all lanes

VTransport
= one lane × one road section

Junction
= shared boundary between adjacent RoadLinks / VTransports

Vehicle
= microscopic actor occupying exactly one VTransport at a stable instant

Recorder
= observation layer for traffic measurements


===============================================================================
2. SPATIAL MODEL
   ===============================================================================


Example: 3 lanes, 3 road sections


                         ROUTE / CORRIDOR
                               │
                               │
         RoadLink 0        RoadLink 1        RoadLink 2
          J0 -> J1          J1 -> J2          J2 -> J3
             │                 │                 │

Pathway 0   VT(0,0)  --->   VT(0,1)  --->   VT(0,2)
\               \               \
J1              J2              J3

Pathway 1   VT(1,0)  --->   VT(1,1)  --->   VT(1,2)

Pathway 2   VT(2,0)  --->   VT(2,1)  --->   VT(2,2)


ROWS:
Pathway = same lane longitudinally

COLUMNS:
RoadLink = same road section across lanes

CELLS:
VTransport = one lane × one road section


===============================================================================
3. ACTUAL VTRANSPORT/JUNCTION TOPOLOGY
   ===============================================================================


A Pathway is constructed from:

    points = from +: junctions :+ to


Example:

    points =
        [VSource, J0, J1, J2, Sink]


Therefore:

    seg(0) = VTransport(VSource -> J0)
    seg(1) = VTransport(J0      -> J1)
    seg(2) = VTransport(J1      -> J2)
    seg(3) = VTransport(J2      -> Sink)


Physical execution:

    VSource
       |
       v
    VTransport 0
       |
       v
    Junction 0
       |
       v
    VTransport 1
       |
       v
    Junction 1
       |
       v
    VTransport 2
       |
       v
      Sink


IMPORTANT:

There is NO need for:

    VT0.next = VT1
    VT1.prev = VT0

The topology is already expressed by shared boundaries:

    VT0.to   == J0
    VT1.from == J0


===============================================================================
4. VTRANSPORT
   ===============================================================================


Meaning:

    VTransport = authoritative microscopic traffic-state object
                 for one lane × one road section


VTransport
│
├── Identity
│   ├── segmentIndex
│   └── laneIndex
│
├── Geometry
│   ├── from
│   ├── to
│   ├── length
│   └── safeDistance
│
├── Authoritative vehicle ordering
│   ├── vehicleOrder : DoublyLinkedList[Vehicle]
│   └── vehicleCount
│
├── Occupancy
│   ├── accept(vehicle, leader)
│   └── release(vehicle)
│
├── Microscopic local queries
│   ├── downstreamVehicle
│   ├── upstreamVehicle
│   ├── leaderOf(vehicle)
│   └── followerOf(vehicle)
│
├── Local traffic state
│   ├── count
│   ├── density
│   ├── meanSpeed
│   └── instantaneousFlow
│
└── Physical movement
└── move()


DLL orientation:

    upstream                                      downstream

    tail                                             head
      ↓                                                ↓

    [C3]  <------>  [C2]  <------>  [C1]


Therefore:

    downstreamVehicle = DLL head

    upstreamVehicle   = DLL tail / last

    node.ahead        = immediate leader

    node.behind       = immediate follower


Density:

    density = vehicleCount / length


Mean speed:

    average velocity of vehicles currently in this VTransport


Instantaneous flow:

    q = density × meanSpeed


IMPORTANT:

    instantaneousFlow

is a STATE-DERIVED estimate.

It is NOT the same as:

    actual vehicles crossing a Junction during a time interval.


===============================================================================
5. VEHICLE LOCATION — SINGLE SOURCE OF TRUTH
   ===============================================================================


The VTransport that owns the vehicle should determine:

    current lane
    current segment


Eventually:

    Vehicle.currentVTransport : VTransport


Then:

    laneID = currentVTransport.laneIndex
    segId  = currentVTransport.segmentIndex


During transition, laneID and segId may remain as fields,
but they must be assigned from the VTransport.


CORRECT ownership point:

    VTransport.accept(vehicle)


Conceptually:

    def accept(vehicle, leader):

        insert vehicle into this VTransport DLL

        vehicle.currentVTransport = this

        vehicle.prevSegId = vehicle.segId

        vehicle.segId  = this.segmentIndex
        vehicle.laneID = this.laneIndex

        vehicleCount += 1


WHY accept(), NOT move():

A vehicle may change lanes without changing segment:

    VT(lane 1, seg 3)
            |
            v
    VT(lane 2, seg 3)

The physical location changed at insertion into the target VT.

Therefore location must change when ownership changes.


VTransport.move():

    moves a vehicle that is ALREADY owned by the VTransport.

It should not become the source of truth for lane/segment identity.


===============================================================================
6. PATHWAY
   ===============================================================================


Meaning:

    Pathway = one physical lane across an ordered sequence
              of VTransports


Pathway
│
├── laneIndex
├── from
├── to
├── junctions[]
├── points[]
├── seg[] : Array[VTransport]
├── segment(segId)
├── geometry
└── display


Responsibilities:

    - organize VTransports longitudinally
    - preserve lane identity
    - handle sparse/variable lane existence
    - provide lane-level structural access
    - visualize the lane


Example:

    Pathway lane 2

        VT(2,0)
          |
          J0
          |
        VT(2,1)
          |
          J1
          |
        VT(2,2)


Pathway DOES NOT OWN:

    - vehicle DLL
    - vehicleCount
    - density
    - mean speed
    - flow
    - leader/follower state


Those belong to VTransport.


Pathway may expose:

    def segment(segId): VTransport

because it is the longitudinal lane view.


===============================================================================
7. ROADLINK
   ===============================================================================


Meaning:

    RoadLink = one physical road section between two boundaries,
               containing all VTransports that exist across lanes.


Example:

    RoadLink 1 = J1 -> J2

        lane 0 : VT(0,1)
        lane 1 : VT(1,1)
        lane 2 : VT(2,1)
        lane 3 : VT(3,1)


RoadLink
│
├── index
├── from
├── to
├── lanes[] : Array[VTransport]
│
├── laneCount
├── lane(laneId)
│
├── vehicleCount
├── density
├── meanSpeed
├── instantaneousFlow
│
└── future operational state
├── capacity
├── speedLimit
├── activeLanes
├── open / closed
└── disruption / hazard state


IMPORTANT:

RoadLink does NOT create new VTransports.

It references the SAME VTransport objects already created by Pathway.


Pathway view:

    Pathway 0:
        VT(0,0), VT(0,1), VT(0,2)

RoadLink view:

    RoadLink 1:
        VT(0,1), VT(1,1), VT(2,1)


RoadLink density:

    total vehicles across its VTransports
    ------------------------------------------------
    total lane-length across its VTransports


That is:

    k_link =
        Σ N_lane
        --------
        Σ L_lane


This handles variable lane counts correctly.


===============================================================================
8. ROUTE
   ===============================================================================


Meaning:

    Route = top-level corridor organizer


Route
│
├── name
├── from
├── to
│
├── Pathways[]
├── RoadLinks[]
├── Junctions[]
│
├── lanesPerSegment[]
├── maxLanes
├── segmentOffsets[]
│
├── lanesAt(segment)
├── laneExistsAt(lane, segment)
├── path(lane)
├── roadLink(segment)
├── segments
├── cumulativePosition(...)
│
├── corridor geometry
└── display


Route binds together:

    longitudinal view:
        Pathways

    cross-sectional view:
        RoadLinks

    boundaries:
        Junctions


Route SHOULD NOT OWN:

    - vehicle DLL
    - leader/follower
    - accept/release
    - lane-local density
    - microscopic handoff
    - merge execution
    - lane-change execution


Route-level aggregate state may be computed by aggregating RoadLinks.


Example:

    route density =
        total vehicles in all RoadLinks
        --------------------------------
        total lane-length in all RoadLinks


===============================================================================
9. JUNCTION
   ===============================================================================


Meaning:

    Junction = shared boundary / connector between VTransports


Junction has NO singular:

    from
    to


because a Junction may represent:

    normal continuation
    merge
    diverge
    lane drop
    ramp connection


Normal shared junction:

    lane 0: VT(0,0) ---> J ---> VT(0,1)
    lane 1: VT(1,0) ---> J ---> VT(1,1)
    lane 2: VT(2,0) ---> J ---> VT(2,1)


Therefore Junction stores topology by lane:

    incomingByLane
    outgoingByLane


Current simple version:

    private[process] val incomingByLane =
        Array.ofDim[VTransport](nl)

    private[process] val outgoingByLane =
        Array.ofDim[VTransport](nl)


API:

    registerIncoming(lane, vt)

    registerOutgoing(lane, vt)

    incoming(lane)

    outgoing(lane)


Registration rule:

For:

    VTransport(from = p1, to = p2)

if p1 is Junction:

    p1.registerOutgoing(lane, vt)

if p2 is Junction:

    p2.registerIncoming(lane, vt)


Therefore:

    VT.to == Junction
        => VT is incoming to Junction

    VT.from == Junction
        => VT is outgoing from Junction


===============================================================================
10. JUNCTION DENSITY SEMANTICS
    ===============================================================================


IMPORTANT RULE:

    Density belongs to a spatial scope.

It does NOT intrinsically belong to Junction.


Possible density scopes:


A. Lane-level upstream density

    incoming(lane).density


B. Lane-level downstream density

    outgoing(lane).density


C. Incoming RoadLink / segment density

    aggregate all incoming VTransports


D. Outgoing RoadLink / segment density

    aggregate all outgoing VTransports


E. Corridor density

    Route / RoadLink aggregation


Avoid:

    junction.density

because it is ambiguous.


Example:

                    Junction J

lane 0:  VT00 --------> J --------> VT01
lane 1:  VT10 --------> J --------> VT11
lane 2:  VT20 --------> J --------> VT21


Possible queries:

    incomingDensity(0)
        = VT00.density

    outgoingDensity(0)
        = VT01.density


Incoming aggregate:

    N00 + N10 + N20
    ----------------
    L00 + L10 + L20


Outgoing aggregate:

    N01 + N11 + N21
    ----------------
    L01 + L11 + L21


===============================================================================
11. RECORDER
    ===============================================================================


Meaning:

    Recorder = observation layer

It should NOT control simulation behavior.


Recorder attached to Junction observes:

    vehicles crossing this boundary


Existing matrices:

    r_counts
    r_speeds
    r_density


For a shared Junction:

    rows    = time intervals
    columns = lanes


Example:

                 lane0  lane1  lane2  lane3

time 0
time 1
time 2
...


Crossing count:

    r_counts(time, lane)


Crossing speed:

    r_speeds(time, lane)


Density recorded at Junction:

    use incoming VTransport state

    r_density(time, lane)
        = incoming(lane).density


IMPORTANT DISTINCTION:


VTransport gives:

    STATE

        count
        density
        meanSpeed
        q = k × v


Junction Recorder gives:

    EVENTS

        vehicles crossing
        crossing speed
        crossing count per interval


Therefore:

    q_state = k × v

and:

    measured flow = crossing count / time interval

are related but NOT identical measurements.


===============================================================================
12. JUNCTION HANDOFF
    ===============================================================================


Target conceptual handoff:

    incoming VTransport
            |
            v
         Junction
            |
            v
    outgoing VTransport


Junction should eventually handle:

    - determine outgoing VTransport
    - inspect outgoing boundary
    - determine whether transfer is possible
    - release from incoming VTransport
    - perform Junction traversal delay
    - accept into outgoing VTransport


Conceptually:

    1. vehicle reaches downstream boundary of incoming VT

    2. Junction identifies:
           incoming VT
           outgoing VT

    3. Junction checks downstream space

    4. incomingVT.release(vehicle)

    5. vehicle traverses Junction

    6. outgoingVT.accept(vehicle, leader)


Exact release/currentVTransport semantics during the Junction traversal
still need to be finalized.


===============================================================================
13. LOCAL LEADER / FOLLOWER
    ===============================================================================


Inside one VTransport:

    leaderOf(vehicle)

    followerOf(vehicle)


These are O(1) through the DLL.


If vehicle is downstream-most:

    leaderOf(vehicle) == null


Cross-boundary leader lookup should NOT be performed directly by VTransport.

Instead:

    VTransport
        |
        v
    downstream Junction
        |
        v
    outgoing VTransport
        |
        v
    outgoing.upstreamVehicle


So:

    local leader
        = VTransport responsibility

    cross-boundary leader
        = Junction / boundary responsibility


===============================================================================
14. LANE CHANGE
    ===============================================================================


Lane change happens within one RoadLink:


Before:

    lane 1 VT ---- vehicle

    lane 2 VT ---- target position


Transfer:

    sourceVT.release(vehicle)

    targetVT.accept(vehicle, targetLeader)


Because accept() assigns:

    laneIndex
    segmentIndex
    currentVTransport

the vehicle's physical state remains synchronized.


Long-term lane-change DECISION should not belong to Route.

Possible separation:

    LaneChangePolicy
        decides whether/where to move

    RoadLink
        provides lateral lane structure

    VTransport
        performs release / accept


===============================================================================
15. MERGE / DIVERGE
    ===============================================================================


Merge:

    VT_A ----\
              J ----> VT_C
    VT_B ----/


Junction:

    incoming:
        VT_A
        VT_B

    outgoing:
        VT_C


Diverge:

              ----> VT_B
    VT_A --> J
              ----> VT_C


Junction:

    incoming:
        VT_A

    outgoing:
        VT_B
        VT_C


General merge/diverge may eventually require:

    multiple VTransports per lane entry

rather than:

    exactly one incomingByLane(lane)

but we do not need to complicate the current corridor implementation
until topology requires it.


===============================================================================
16. VARIABLE LANES / LANE DROPS
    ===============================================================================


Pathway may contain null segments:

    lane 0: VT0 VT1 VT2 VT3
    lane 1: VT0 VT1 VT2 VT3
    lane 2: VT0 VT1 VT2 null
    lane 3: VT0 VT1 null null


Therefore:

    Route.lanesAt(segment)

and:

    Route.laneExistsAt(lane, segment)

remain valid corridor structural queries.


RoadLink naturally sees:

    number of active VTransports in that section


Example:

    RoadLink 0 = 4 lanes
    RoadLink 1 = 4 lanes
    RoadLink 2 = 3 lanes
    RoadLink 3 = 2 lanes


===============================================================================
17. SINGLE-SOURCE-OF-TRUTH INVARIANTS
    ===============================================================================


These should become hard architectural invariants:


1.

One vehicle belongs to exactly one VTransport DLL
at a stable simulation instant.


2.

vehicle.myPathNode belongs to:

     vehicle.currentVTransport.vehicleOrder


3.

vehicle.currentVTransport.laneIndex == vehicle.laneID


4.

vehicle.currentVTransport.segmentIndex == vehicle.segId


5.

vehicleCount == vehicleOrder.size


6.

A vehicle may not occupy a VTransport for a lane
that does not exist.


7.

leader/follower relationships are local to one VTransport DLL.


8.

VTransport does not skip across Junction boundaries.


9.

Junction mediates VTransport-to-VTransport transfer.


10.

Pathway and RoadLink reference the SAME VTransport objects.

No duplicate traffic-state containers.


11.

Recorder observes simulation state/events.

Recorder never controls traffic behavior.


===============================================================================
18. CLASS OWNERSHIP SUMMARY
    ===============================================================================


+--------------+------------------------------------------------------------+
| Class        | Responsibility                                             |
+--------------+------------------------------------------------------------+
| Vehicle      | microscopic actor and current physical ownership           |
| VTransport   | one lane-segment, DLL, local microscopic traffic state     |
| Pathway      | one lane longitudinally across segments                    |
| RoadLink     | one road section across all lanes                          |
| Junction     | shared boundary, connectivity, handoff, sensing            |
| Route        | complete corridor organization                             |
| Recorder     | observation and measurement                                |
+--------------+------------------------------------------------------------+


===============================================================================
19. DATA / CONTROL FLOW
    ===============================================================================


Vehicle enters corridor

        |
        v

VTransport.accept(vehicle)
|
+--> DLL ownership
+--> currentVTransport
+--> laneID
+--> segId
|
v

VTransport.move()
|
+--> leader/follower
+--> car-following dynamics
+--> microscopic movement
|
v

downstream Junction
|
+--> record crossing
+--> observe incoming VT state
+--> choose/resolve outgoing VT
+--> check transfer
|
v

incomingVT.release(vehicle)

        |
        v

Junction traversal

        |
        v

outgoingVT.accept(vehicle)

        |
        v

next VTransport.move()


===============================================================================
20. AGGREGATION HIERARCHY
    ===============================================================================


MICROSCOPIC

    Vehicle
        |
        v
    VTransport
        one lane × one section


LOCAL MESOSCOPIC / LINK STATE

    RoadLink
        all lanes × one section


CORRIDOR STATE

    Route
        all RoadLinks


BOUNDARY OBSERVATION

    Junction
        crossing events
        upstream/downstream state access


So:

    Vehicle
        ↓
    VTransport
        ↓
    RoadLink
        ↓
    Route


while spatial execution is:

    VTransport
        ↓
    Junction
        ↓
    VTransport


===============================================================================
21. WHAT NOT TO DUPLICATE
    ===============================================================================


DO NOT have:

    Pathway DLL
    +
    VTransport DLL


DO NOT have:

    Ramp DLL
    +
    Ramp VTransport DLL


DO NOT have:

    RoadLink vehicle container
    +
    VTransport vehicle container


ONLY VTransport owns microscopic vehicle ordering.


RoadLink and Route aggregate state from VTransports.


===============================================================================
22. FUTURE AI / DIGITAL-TWIN VALUE
    ===============================================================================


This architecture naturally exposes structured state:


VTransport:

    microscopic/local state

        leader
        follower
        count
        density
        meanSpeed
        flow


RoadLink:

    mesoscopic/link state

        total occupancy
        density
        meanSpeed
        flow
        capacity
        active lanes
        disruption state


Route:

    corridor state


Junction:

    boundary events
    transfers
    flow conservation
    connectivity


This gives an AI agent explicit simulation tools such as:

    inspectVehicle(...)
    inspectVTransport(...)
    inspectRoadLink(...)
    inspectJunction(...)
    checkConservation(...)
    checkOwnership(...)
    runSimulation(...)
    validateRecorder(...)
    repair(...)
    rerun(...)


That supports the future simulation-repair-agent direction without
mixing AI concerns into the core simulator architecture.


===============================================================================
FINAL PRINCIPLE
===============================================================================


Route
organizes the corridor.

Pathway
organizes one lane longitudinally.

RoadLink
organizes one road section across lanes.

VTransport
owns the actual microscopic traffic state.

Junction
owns the boundary between VTransports.

Recorder
observes traffic state and boundary crossings.

Vehicle
always knows the VTransport that physically owns it.


=======================
The one part that is still intentionally not fully locked is the exact vehicle state while it is physically traversing a Junction—specifically whether currentVTransport remains the incoming VT until the outgoing accept(), becomes null, or whether we eventually represent the Junction as the current component during that short interval. Everything else above is the architecture we have converged on.