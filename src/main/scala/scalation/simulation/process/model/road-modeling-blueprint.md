val osm = OsmSpec (
name = "CalRoute101",
bbox = BoundingBox (
south = ...,
west  = ...,
north = ...,
east  = ...
),
marginMeters = 500.0
)

println (osm)



val sensors = Array (
VirtualSensor ("sensor-1", lat, lon, direction = "W"),
VirtualSensor ("sensor-2", lat, lon, direction = "W")
)

the same projected network drives:
- simulation topology
- source/sink placement
- virtual/PeMS sensors
- animation