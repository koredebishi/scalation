# OneWayVehicle2L DLL Structure and Debugging Guide

## Why Vehicles Use Different DLLs

The OneWayVehicle2L model implements a realistic highway system with multiple separate doubly linked lists (DLLs):

### 1. Highway Lane DLLs
- **4 separate highway lanes**: Each `Pathway` in the `Route` has its own `vList` DLL
- **DLL IDs**: `DLL_Rte_0_Lane`, `DLL_Rte_1_Lane`, `DLL_Rte_2_Lane`, `DLL_Rte_3_Lane`
- **Purpose**: Independent vehicle management per lane, enabling realistic lane changes

### 2. Ramp DLLs
- **On-ramps**: `DLL_onRamp1_OnRamp`, `DLL_onRamp2_OnRamp`
- **Off-ramp**: `DLL_offRamp_OffRamp`
- **Purpose**: Separate vehicle management for merging/diverging traffic

### 3. Vehicle DLL Transitions

Vehicles move between DLLs during their journey:

```
Entry → Highway Lane → [Lane Changes] → [Ramp Transfer] → Exit
  ↓           ↓              ↓               ↓            ↓
VSource → DLL_Rte_X_Lane → Other Lanes → Ramp DLL → Sink
```

## Enhanced DLL Labeling System

### Before (Poor Debugging)
```scala
// Generic pathway names
pathway(i) = new Pathway(s"${name}_$i", ...)
// Result: "Rte_0", "Rte_1", etc.

// No DLL operation logging
// Unclear vehicle transitions
```

### After (Enhanced Debugging)
```scala
// Clear DLL identification
val dllId = s"DLL_${name}_Lane"           // For Pathways
val dllId = s"DLL_${name}_${mode}Ramp"    // For Ramps

// Detailed operation logging
private def logDLLOperation(operation: String, vehicle: Vehicle, details: String = ""): Unit =
    debug(s"$operation", s"[$dllId] Vehicle ${vehicle.name} $details | DLL size: ${vList.size}")
```

## DLL Labels in OneWayVehicle2L

### Highway Lanes (4 lanes)
- `DLL_Rte_0_Lane` - Rightmost lane
- `DLL_Rte_1_Lane` - Lane 1
- `DLL_Rte_2_Lane` - Lane 2  
- `DLL_Rte_3_Lane` - Leftmost lane

### Ramps
- `DLL_onRamp1_OnRamp` - First on-ramp
- `DLL_onRamp2_OnRamp` - Second on-ramp
- `DLL_offRamp_OffRamp` - Off-ramp

## Debug Output Examples

### Vehicle Addition
```
[ADD_TO_DLL] [DLL_Rte_1_Lane] Vehicle c42 following c41 | DLL size: 15
```

### Lane Change
```
[LANE_CHANGE_ATTEMPT] Vehicle c42: DLL_Rte_1_Lane → DLL_Rte_2_Lane at segment 2
[LANE_CHANGE_SUCCESS] Vehicle c42: Executing lane change DLL_Rte_1_Lane → DLL_Rte_2_Lane
[ADD_TO_DLL] [DLL_Rte_2_Lane] Vehicle c42 following c38 | DLL size: 12
[REMOVE_FROM_DLL] [DLL_Rte_1_Lane] Vehicle c42 | DLL size: 14
```

### Ramp Transfer
```
[ADD_TO_DLL] [DLL_offRamp_OffRamp] Vehicle c42 following NONE | DLL size: 1
[REMOVE_FROM_DLL] [DLL_Rte_2_Lane] Vehicle c42 | DLL size: 11
```

### Enhanced Vehicle toString
```
Vehicle (c42 at 15.3:sec, actor_id=42, disp:245.7:m, lane:2, DLL:DLL_Rte_2_Lane)
```

## Key Benefits

1. **Clear DLL Identification**: Each DLL has a unique, descriptive name
2. **Operation Tracking**: All add/remove operations are logged with context
3. **Transition Visibility**: Lane changes and ramp transfers are clearly tracked
4. **Size Monitoring**: DLL sizes are reported for capacity analysis
5. **Vehicle State**: Each vehicle shows its current DLL in debug output

## Debugging Best Practices

1. **Enable Debug Output**: Set `debug = debugf("Pathway", true)` and `debug = debugf("Ramp", true)`
2. **Monitor DLL Sizes**: Watch for unexpected growth or empty lists
3. **Track Transitions**: Look for vehicles appearing in multiple DLLs (error condition)
4. **Verify Lane Changes**: Ensure vehicles properly transfer between highway lane DLLs
5. **Check Ramp Flows**: Monitor on-ramp merging and off-ramp diverging patterns

## Common Issues to Watch For

- **Vehicles in multiple DLLs**: Should never happen - indicates cleanup bug
- **Empty DLLs with traffic**: May indicate routing or merging issues  
- **DLL size mismatches**: Compare with expected traffic volumes
- **Failed lane changes**: Monitor blocked attempts vs. successful transfers
- **Ramp capacity**: Watch for excessive queuing in ramp DLLs
