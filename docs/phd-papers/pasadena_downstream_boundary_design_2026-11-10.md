# Pasadena downstream boundary: design (2026-11-10, read-only, nothing implemented)

No edits. Here is the whole change, with today's code next to the proposed code.

## The problem in one picture

```
            S1        S2   S3              S4 (Hill)     Lake 1        Marengo
 cars -->   |---------|----|---------------|   sink      (detector)    (detector)
                                           ^
 today: the sink deletes each car the moment it reaches Hill.
        The last car in every lane has nobody ahead, so IDM gives it open road:
        Dynamics.scala:524   (car.t_disp + 1000.0, car.velocity)   free flow

 real:  the queue head is below Lake (35k veh/day merge in, dead detectors).
        The queue reaches back through Lake (11.7 m/s) and Hill (8.6 m/s) to S2.
        Our model can never build that queue because nothing at the end ever slows.
```

The fix is the textbook downstream boundary: feed the measured Lake speed in at the end, and let IDM do the rest. The mechanism is a phantom leader.

## The mechanism, step by step

Car A reaches the end of the last segment in its lane. Today the model removes A from the lane list and the sink terminates it. Proposed:

```
A reaches lane end
  -> sinks.head.leave ()
       tally time in system          (unchanged, measured now, at the real exit)
       wake the previous phantom P in this lane, if any
       A.velocity     = Lake speed for A's lane and this 5-min bin
       A.boundaryTime = clock
       A sleeps                      (yield without quit; A stays in the lane list)

Follower B behind A, every 0.5 s tick:
       B.leader = A                                 Vehicle.scala:109   unchanged
       spacing  = A.corridorPos - B.corridorPos      Dynamics.scala:467  unchanged
       A.corridorPos = lane end + Lake speed x (clock - boundaryTime)     NEW
       B follows A with IDM at the Lake speed, or ignores A once it is 150 m away
                                                    Dynamics.scala:524  unchanged

B reaches lane end
  -> leave (): wakes A, B becomes the phantom
A resumes after its sleep:
       remove A from the lane list, clear boundaryTime, animate, retire, terminate
```

So each lane holds exactly one phantom at a time, the last car that exited. In the evening a car exits every 3 to 4 seconds per lane, so the phantom is only 40 to 50 m ahead when the next car arrives, and the chain of followers settles at the Lake speed. At night cars exit a minute apart, the phantom is far beyond 150 m, and the boundary has no effect. That is the behaviour the book asks for: a free boundary when the downstream is in free flow, page 466.

## The code

**`Vehicle.scala`**, lines 74 and 125. One runtime field and one derived position.

Today:
```scala
    var myPathway     : Pathway     = null   // current pathway/lane this vehicle is on (null if not on a pathway)
    ...
    def corridorPos: Double =
        if myPathway != null then myPathway.corridorPos (segId, disp)
        else disp                          // ramp / FF connector: one segment
    end corridorPos
```

Proposed:
```scala
    var myPathway     : Pathway     = null   // current pathway/lane this vehicle is on (null if not on a pathway)
    private [process] var boundaryTime: Double = Double.NaN   // clock when a boundary sink began holding this car as a phantom leader; NaN while driving
    ...
    def corridorPos: Double =
        val pos = if myPathway != null then myPathway.corridorPos (segId, disp)
                  else disp                // ramp / FF connector: one segment
        if boundaryTime.isNaN then pos
        else pos + velocity * (director.clock - boundaryTime)   // phantom: drifts past the lane end at the boundary speed
    end corridorPos
```

Nothing is stored except the time the hold began. The virtual position is derived on every read from velocity and the clock.

**`Sink.scala`**, class header line 28 and `leave` line 57.

Today:
```scala
class Sink (name: String, at: Array [Double])
      extends Component:
    ...
    def leave (): Unit =
        val actor = director.theActor
        val ctime = director.clock                                     // clock timeA
        tally (ctime - actor.arrivalT)                                 // tally actor time in system
        director.animate (actor, MoveToken, null, null, Array (at(0) + DIAM, at(1) + at(3) / 2.0 - RAD))
        director.retireLiveToken (actor)
        actor.yieldToDirector (true)                                   // yield and terminate
    end leave
```

Proposed:
```scala
class Sink (name: String, at: Array [Double],
            boundarySpeed: (Int, Double) => Double = null)             // (laneID, clock) -> m/s; null = free boundary (today)
      extends Component:

    private val held = scala.collection.mutable.Map [Pathway, Vehicle] ()   // the phantom leader per lane
    ...
    def leave (): Unit =
        val actor = director.theActor
        val ctime = director.clock                                     // clock timeA
        tally (ctime - actor.arrivalT)                                 // tally actor time in system (at the real exit)

        actor match
            case car: Vehicle if boundarySpeed != null && car.myPathway != null =>
                val lane = car.myPathway
                held.get (lane).foreach (_.schedule (0.0))             // release the phantom this car replaces
                car.velocity     = boundarySpeed (car.laneID, ctime)   // drive on at the measured downstream speed
                car.boundaryTime = ctime
                held (lane)      = car
                car.yieldToDirector ()                                 // sleep as the phantom until the next car arrives
                lane.removeFromAlist (car)                             // woken: now leave the lane list
                car.boundaryTime = Double.NaN
            case _ =>                                                  // free boundary, or the car came off a meso link

        director.animate (actor, MoveToken, null, null, Array (at(0) + DIAM, at(1) + at(3) / 2.0 - RAD))
        director.retireLiveToken (actor)
        actor.yieldToDirector (true)                                   // yield and terminate
    end leave
```

The sleep-then-resume inside a component is the pattern the meso link already uses to hold the FIFO head, `MesoVTransport.scala:254-257`. The time-in-system tally stays at the real exit, so the travel-time metric does not change meaning.

**`I210W.scala`**, two lines. The main sink gets the boundary; the car stops removing itself before the sink.

Today, line 155 and line 224:
```scala
    private val sinks = Sink.group ((0, 0),
        ("sinkMain", (x0.toInt, y0.toInt)),
        ("sinkOff1", I210WConfig.getOffRampEnd ((w, h), 0)), ...
    ...
                driveSegments (seg, highway_length)
                route.pathway(laneID).removeFromAlist (this)
                sinks.head.leave ()
```

Proposed:
```scala
    private val boundary = I210WConfig.boundarySpeeds (day)                     // 205 x 5, m/s, Lake 1
    private val sinkMain = new Sink ("sinkMain", Array (x0, y0, 20.0, 20.0),
                                     (lane, clock) => boundary (getCurrentRow (clock) min (nt - 1), lane))
    private val sinks = sinkMain :: Sink.group ((0, 0),
        ("sinkOff1", I210WConfig.getOffRampEnd ((w, h), 0)), ...
    ...
                driveSegments (seg, highway_length)
                sinks.head.leave ()                                    // the sink removes the car from the lane when it releases it
```

**`I210WConfig.scala`**, after `mainlineFiles` at line 133. The loader already exists and returns speeds in m/s.
```scala
    val boundaryFile = "5-717634ML.csv"                     // Lake 1, first detector downstream of the sink (not a validation station)

    /** Lane speeds at Lake 1 per 5-min row, m/s: the downstream boundary of the sink. */
    def boundarySpeeds (day: String = DEFAULT_DAY): MatrixD =
        PeMSDataLoader.loadMainlineSensor (s"${mainlineDir (day)}/$boundaryFile", window, layout)._2
```

**`PeMSDataHelper.scala`**, line 209, one more entry so the extractor writes the Lake file:
```scala
                          ("3", "717642", "Altadena"),           ("4", "717637", "Hill"),
                          ("5", "717634", "Lake 1"))
```
`I210WConfig.mainlineFiles` keeps four entries, so Lake is never counted as a validation station.

## What does not change

`Dynamics`, `Pathway`, the leader lookup, `VTransport`, the junction counters, the recorders, the replication runner, the off-ramp sinks. Every model that passes no boundary function behaves exactly as today, because the field is NaN and the sink takes the `case _` branch.

## Limits you should know before saying yes

- **Mixed and full meso end on a meso segment.** Segment 9 is meso in both, so the car reaches the sink with no pathway and takes the free branch. This step fixes the micro model only. The meso side is step 4, a release-headway floor on the exit link from the Lake speed through `IdmFlux`. The micro result decides whether step 4 is worth doing.
- **End of day.** The last car in each lane stays held because nothing wakes it. Five cars per run are missing from the sink tally, none from the station counts.
- **Gipps and Krauss** read `disp` directly, not `corridorPos`, so they would not see the phantom. Pasadena runs IDM.

If it works, S4 speed at 15 to 18 h drops from the current 19 to 26 m/s towards the observed 9, and S2 and S3 follow because the queue now has something to grow back from.

## Data behind the diagnosis (2024-12-03, hourly means, veh per 5 min / m/s)

| hour | Altadena S3 | Hill S4 | Lake 1 | Marengo |
|---|---|---|---|---|
| 10 | 556 / 15.4 | 515 / 10.6 | 437 / 12.8 | 579 / 27.0 |
| 15 | 503 / 14.6 | 453 / 9.9 | 365 / 12.2 | 630 / 20.4 |
| 16 | 516 / 15.7 | 469 / 8.9 | 383 / 11.7 | 620 / 17.2 |
| 17 | 490 / 9.7 | 471 / 8.6 | 392 / 11.7 | 615 / 15.6 |
| 18 | 474 / 11.8 | 474 / 9.0 | 383 / 11.8 | 618 / 18.3 |

Day totals: Hill 97,056, Lake 1 84,490, Marengo 119,395. Source: `data/WSC-Pems-Data-Eaton-Fire/data-eaton/pems/eaton-corridor/3rdData/eaton-i210/eaton_i210_W_2024_12_03_ML_HV.csv`.
