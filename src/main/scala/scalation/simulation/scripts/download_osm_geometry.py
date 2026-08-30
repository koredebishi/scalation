#!/usr/bin/env python3
"""
Download OpenStreetMap road network + place names for a geographic bounding box.

Fetches:
  1. Road polylines  — motorway, trunk, primary, secondary, tertiary (+ links)
  2. Place names      — city, town, suburb, neighbourhood, village, hamlet

Writes a single self-contained JSON file.  Works for ANY location worldwide.

Usage:
    python src/main/scala/scalation/simulation/scripts/download_osm_geometry.py \
        --south 34.13 --west -118.28 --north 34.19 --east -118.07 \
        --name eaton

Output:
    data/osm/eaton_roads.json

    {
      "bbox": { ... },
      "roads":  [ { "osm_id", "highway", "ref", "name", "points": [[lat,lon],...] }, ... ],
      "places": [ { "name": "Pasadena", "type": "city", "lat": 34.147, "lon": -118.144 }, ... ]
    }

The Scala loader reads this JSON generically — no model-specific logic.
No heavy dependencies — only requests + stdlib.
"""

import argparse
import json
import os
import sys
from datetime import datetime

try:
    import requests
except ImportError:
    print("ERROR: 'requests' package not found.  Install with:  pip install requests")
    sys.exit(1)

OVERPASS_SERVERS = [
    "https://overpass-api.de/api/interpreter",
    "https://overpass.kumi.systems/api/interpreter",
    "https://maps.mail.ru/osm/tools/overpass/api/interpreter",
]


def build_query(south, west, north, east):
    """Build an Overpass QL query for roads AND place names within a bounding box.

    Single query, two result sets:
      - way["highway"~"..."]  → road polylines
      - node["place"~"..."]   → city/suburb/neighbourhood labels
    """
    return f"""
[out:json][timeout:120];
(
  way["highway"~"motorway|trunk|primary|secondary|tertiary|motorway_link|trunk_link|primary_link|secondary_link|tertiary_link"]
    ({south},{west},{north},{east});
  node["place"~"city|town|suburb|neighbourhood|village|hamlet"]
    ({south},{west},{north},{east});
);
out body;
>;
out skel qt;
"""


def download_osm(south, west, north, east):
    """Download OSM data from Overpass API with fallback servers."""
    query = build_query(south, west, north, east)
    for server in OVERPASS_SERVERS:
        try:
            print(f"Trying {server} ...")
            resp = requests.post(server, data={"data": query}, timeout=180)
            resp.raise_for_status()
            data = resp.json()
            print(f"  Received {len(data.get('elements', []))} elements")
            return data
        except (requests.exceptions.RequestException, ValueError) as e:
            print(f"  Failed: {e}")
    print("ERROR: All Overpass servers failed.")
    sys.exit(1)


def resolve(elements):
    """Resolve OSM elements into roads (polylines) and places (point labels).

    Returns:
        roads  — list of dicts: osm_id, highway, ref, name, points
        places — list of dicts: name, type, lat, lon
    """
    # ── Build node lookup: id → (lat, lon) ──────────────────────────
    nodes = {}
    for el in elements:
        if el["type"] == "node" and "lat" in el and "lon" in el:
            nodes[el["id"]] = (el["lat"], el["lon"])

    # ── Resolve ways → road polylines ───────────────────────────────
    roads = []
    for el in elements:
        if el["type"] != "way":
            continue
        tags = el.get("tags", {})
        highway = tags.get("highway", "")
        if not highway:
            continue

        points = []
        for nid in el.get("nodes", []):
            if nid in nodes:
                lat, lon = nodes[nid]
                points.append([lat, lon])

        if len(points) < 2:
            continue

        roads.append({
            "osm_id": el["id"],
            "highway": highway,
            "ref":  tags.get("ref"),
            "name": tags.get("name"),
            "points": points,
        })

    # ── Resolve place nodes → labels ────────────────────────────────
    places = []
    for el in elements:
        if el["type"] != "node":
            continue
        tags = el.get("tags", {})
        place_type = tags.get("place", "")
        name = tags.get("name", "")
        if place_type and name and "lat" in el and "lon" in el:
            places.append({
                "name": name,
                "type": place_type,
                "lat":  el["lat"],
                "lon":  el["lon"],
            })

    return roads, places


def main():
    parser = argparse.ArgumentParser(
        description="Download OSM road network + place names for a bounding box"
    )
    parser.add_argument("--south", type=float, required=True, help="South latitude")
    parser.add_argument("--west",  type=float, required=True, help="West longitude")
    parser.add_argument("--north", type=float, required=True, help="North latitude")
    parser.add_argument("--east",  type=float, required=True, help="East longitude")
    parser.add_argument("--name",  type=str,   required=True, help="Area name (for output filename)")
    parser.add_argument("--outdir", type=str, default="data/osm", help="Output directory")
    args = parser.parse_args()

    # Download
    data = download_osm(args.south, args.west, args.north, args.east)

    # Resolve
    roads, places = resolve(data["elements"])

    # Summary — roads
    by_type = {}
    for r in roads:
        t = r["highway"]
        by_type[t] = by_type.get(t, 0) + 1
    print(f"\nResolved {len(roads)} road segments:")
    for t, c in sorted(by_type.items(), key=lambda x: -x[1]):
        print(f"  {t:20s}: {c}")
    total_pts = sum(len(r["points"]) for r in roads)
    print(f"  Total coordinate points: {total_pts}")

    # Summary — places
    by_place = {}
    for p in places:
        t = p["type"]
        by_place[t] = by_place.get(t, 0) + 1
    print(f"\nResolved {len(places)} place labels:")
    for t, c in sorted(by_place.items(), key=lambda x: -x[1]):
        print(f"  {t:20s}: {c}")
    for p in sorted(places, key=lambda x: x["type"]):
        print(f"    {p['type']:15s}  {p['name']}")

    # Build output
    output = {
        "bbox": {
            "south": args.south,
            "west":  args.west,
            "north": args.north,
            "east":  args.east,
        },
        "generated":  datetime.now(tz=None).isoformat(),
        "road_count": len(roads),
        "place_count": len(places),
        "roads":  roads,
        "places": places,
    }

    # Write
    os.makedirs(args.outdir, exist_ok=True)
    outpath = os.path.join(args.outdir, f"{args.name}_roads.json")
    with open(outpath, "w") as f:
        json.dump(output, f)
    size_mb = os.path.getsize(outpath) / (1024 * 1024)
    print(f"\nWritten: {outpath} ({size_mb:.2f} MB)")


if __name__ == "__main__":
    main()

