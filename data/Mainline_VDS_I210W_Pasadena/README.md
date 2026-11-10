# I-210 Westbound, Pasadena: Sierra Madre Villa to Hill

PeMS detector data for the mixed-resolution study, one folder per day. Westbound
means decreasing postmile, so station 1 is the most upstream.

| N | VDS | Station | Abs PM | Lanes | Role |
|---|---|---|---|---|---|
| 1 | 717649 | Sierra Madre Villa | 29.17 | 5 | mainline, anchor (demand) |
| 2 | 717644 | San Gabriel | 28.27 | 5 | mainline |
| 3 | 717642 | Altadena | 28.03 | 5 | mainline |
| 4 | 717637 | Hill | 26.80 | 5 | mainline, last station before the sink |

Ramps are in `../Ramps_VDS_I210W_Pasadena/<day>/`:

| N | VDS | Ramp | Abs PM | Type |
|---|---|---|---|---|
| 1 | 716594 | Sierra Madre Villa on | 29.17 | OR |
| 1 | 717648 | Sierra Madre Villa off | 29.17 | FR |
| 2 | 768927 | San Gabriel off | 28.58 | FR |
| 2 | 716592 | San Gabriel on | 28.27 | OR |
| 3 | 716591 | Altadena on | 28.03 | OR |
| 3 | 764346 | Allen off | 27.64 | FR |
| 4 | 716588 | Hill on | 26.80 | OR |

Stretch length between stations 1 and 4: 2.37 mi (3.81 km).

## Days

`2024-12-03`, `2024-12-10`, `2024-12-17`: Tuesdays, baseline weekdays. The fire day
(2025-01-07) is not extracted. Not included: Hill off-ramp 717636, Lake 1 on-ramp
716586, Lake 2 off-ramp 768920, Marengo off-ramp 764349 and Rosemead on-ramp 716596,
whose detectors report no flow on these days, and everything from Lake downstream.

## File layout

Mainline `N-<VDS>ML.csv`, no header, 205 rows (06:00 to 23:00, 5-minute bins):

    HH:MM, total flow, avg speed, lane1 flow, lane1 speed, ..., lane5 flow, lane5 speed

This is `PeMSDataLoader.I210_MainlineLayout` (flow columns 3,5,7,9,11; speed 4,6,8,10,12);
the loader skips column 0. Ramp `N-<VDS>OR.csv` / `FR.csv`, no header, 205 rows:

    HH:MM, flow

Flows are veh / 5 min and speeds mph exactly as PeMS reports them. Nothing is scaled,
filled or averaged. The HOV lane is not extracted.

## Source and regeneration

Source files: `data/WSC-Pems-Data-Eaton-Fire/data-eaton/pems/eaton-corridor/{3rdData,10thData,17thData}/eaton-i210/eaton_i210_W_<date>_{ML_HV,OR,FR}.csv`.
Written by `scalation.simulation.process.config.extractI210WPasadena` in
`src/main/scala/scalation/simulation/process/config/PeMSDataHelper.scala`:

    sbt "runMain scalation.simulation.process.config.extractI210WPasadena"

The main checks every output for 205 rows and no blank cell and stops otherwise.
