#!/usr/bin/env python3
"""
Build a clean, simulation-ready node/edge network for any declared footprint
using OSMnx, instead of cosmetic-only Overpass polylines.

Produces two GeoJSON files:
    data/network/<name>/nodes.geojson  -- one row per intersection/endpoint
    data/network/<name>/edges.geojson  -- one row per road segment between nodes

Also prints a summary: node/edge counts, lane-tag coverage (real vs
defaulted), and a strong-connectivity check (largest strongly connected
component vs total nodes) so gaps can be caught before this feeds any
CorridorLayout/NetworkConfig assembly.

Usage:
    python src/main/scala/scalation/simulation/scripts/build_osm_network.py \
        --name us101_donald_doyle --south ... --west ... --north ... --east ...

The default bounds and name preserve the original Eaton invocation.
"""

import json
import os
import argparse
import hashlib
import platform
import re
import math
from datetime import datetime, timezone
from collections import Counter

import networkx as nx
import osmnx as ox

# ── Validated bounding box (from the auto-download that was visually
#    checked against the running Eaton model -- covers both corridor
#    sink ends: La Tuna Canyon on I-210 WB, San Fernando on SR-134 WB) ──
DEFAULT_BBOX = (-118.304031, 34.131843, -118.061321, 34.244987)   # (west, south, east, north)

DEFAULT_NAME = "eaton"

# Rough per-direction lane defaults by OSM highway class, used ONLY when a
# road segment has no `lanes` tag at all.  Not calibrated -- a placeholder
# until real counts are available.  Edges that use this are flagged.
DEFAULT_LANES = {
    "motorway": 4, "motorway_link": 1,
    "trunk": 3, "trunk_link": 1,
    "primary": 2, "primary_link": 1,
    "secondary": 2, "secondary_link": 1,
    "tertiary": 1, "tertiary_link": 1,
    "residential": 1, "unclassified": 1,
    "living_street": 1, "service": 1,
}


def first(value):
    """OSM tags are sometimes a list when a way had conflicting/multiple
    values; take the first so every cell is a plain scalar for export."""
    return value[0] if isinstance(value, list) else value


def parse_int(value):
    """First integer found in an OSM tag value, or None.  Defends against
    real-world values like "2;3" (conflicting survey) or "2.5"."""
    if value is None:
        return None
    for part in str(first(value)).replace(",", ";").split(";"):
        try:
            return max(1, int(float(part)))
        except (ValueError, OverflowError):
            continue
    return None


def resolve_lanes(row):
    """Per-direction lane count for one directed edge.
    Returns (lanes_effective, provenance, ambiguous).

    `reversed` (set by osmnx when it builds the graph) tells us which
    direction this edge represents: non-reversed edges were added first,
    in the OSM way's own node order (forward); reversed edges were added
    second, walking the way backward (backward).  So non-reversed -> use
    lanes:forward, reversed -> use lanes:backward.
    """
    highway = first(row.get("highway", "unclassified"))
    reversed_ = bool(first(row.get("reversed", False)))
    lanes = parse_int(row.get("lanes"))

    if first(row.get("oneway", False)):
        if lanes is not None:
            return lanes, "tagged", False
    else:
        directional_tag = row.get("lanes:backward") if reversed_ else row.get("lanes:forward")
        directional = parse_int(directional_tag)
        if directional is not None:
            return directional, "directional_tag", False
        if lanes is not None:
            # Single lanes tag on a two-way street == total across both
            # directions by OSM convention; split it.
            return max(1, lanes // 2), "split_total", lanes % 2 != 0

    return DEFAULT_LANES.get(highway, 1), "default", False


def sha256_file(path):
    digest = hashlib.sha256()
    with open(path, "rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def parse_args():
    parser = argparse.ArgumentParser(
        description="Build a directed, simulation-ready OSM GeoJSON network."
    )
    parser.add_argument("--name", default=DEFAULT_NAME,
                        help="network/artifact name (default: eaton)")
    parser.add_argument("--outdir", default=None,
                        help="output directory (default: data/network/<name>)")
    parser.add_argument("--west", type=float, default=DEFAULT_BBOX[0])
    parser.add_argument("--south", type=float, default=DEFAULT_BBOX[1])
    parser.add_argument("--east", type=float, default=DEFAULT_BBOX[2])
    parser.add_argument("--north", type=float, default=DEFAULT_BBOX[3])
    parser.add_argument("--margin-meters", type=float, default=0.0,
                        help="expand the supplied raw bounds by this margin")
    args = parser.parse_args()
    if not (-180.0 <= args.west <= args.east <= 180.0):
        parser.error("longitude bounds must satisfy -180 <= west <= east <= 180")
    if not (-90.0 <= args.south <= args.north <= 90.0):
        parser.error("latitude bounds must satisfy -90 <= south <= north <= 90")
    if not re.fullmatch(r"[A-Za-z0-9_-]+", args.name):
        parser.error("name may contain only letters, digits, '_' and '-'")
    if args.margin_meters < 0.0:
        parser.error("margin-meters must be non-negative")
    return args


def expand_bbox(bbox, margin_meters):
    west, south, east, north = bbox
    mid_lat = (south + north) / 2.0
    lat_margin = margin_meters / 111320.0
    lon_scale = 111320.0 * max(math.cos(math.radians(mid_lat)), 1.0e-6)
    lon_margin = margin_meters / lon_scale
    return (
        max(-180.0, west - lon_margin),
        max(-90.0, south - lat_margin),
        min(180.0, east + lon_margin),
        min(90.0, north + lat_margin),
    )


def main():
    args = parse_args()
    raw_bbox = (args.west, args.south, args.east, args.north)
    bbox = expand_bbox(raw_bbox, args.margin_meters)
    out_dir = args.outdir or os.path.join("data/network", args.name)
    cache_dir = os.path.join("cache", f"{args.name}_model")
    os.makedirs(out_dir, exist_ok=True)

    # osmnx caches the raw Overpass response under a hash-named file; all repo
    # caches live in cache/<model>/.
    ox.settings.cache_folder = cache_dir

    # Pull lanes:forward / lanes:backward too -- not in osmnx's default
    # edge-tag list, and without them a two-way street's per-direction
    # lane counts can't be told apart from the total.
    ox.settings.useful_tags_way = list(
        set(ox.settings.useful_tags_way) | {"lanes:forward", "lanes:backward"}
    )

    print(f"Downloading drive network for name={args.name}, bbox={bbox} ...")
    # Download unsimplified, then simplify while forcing a break wherever
    # lanes/maxspeed change -- plain simplify=True would merge segments
    # with different lane counts into one edge and silently pick one.
    G = ox.graph_from_bbox(bbox=bbox, network_type="drive", simplify=False)
    G = ox.simplify_graph(
        G, edge_attrs_differ=["lanes", "lanes:forward", "lanes:backward", "maxspeed"]
    )
    print(f"Graph: {G.number_of_nodes()} nodes, {G.number_of_edges()} edges")

    # ── Strong connectivity check ───────────────────────────────────────
    if G.number_of_nodes() == 0 or G.number_of_edges() == 0:
        raise RuntimeError("OSM query returned no drivable network")

    sccs = list(nx.strongly_connected_components(G))
    largest = max(sccs, key=len)
    print(f"Strongly connected components: {len(sccs)}")
    print(f"Largest SCC: {len(largest)} / {G.number_of_nodes()} nodes "
          f"({100 * len(largest) / G.number_of_nodes():.1f}%)")
    if len(sccs) > 1:
        sizes = sorted((len(c) for c in sccs), reverse=True)
        print(f"  other component sizes (top 10): {sizes[1:11]}")
        print("  -> outside nodes may indicate clipped ramps, one-way "
              "subnetworks, disconnected components, or tagging issues.")

    # ── Resolve lanes per edge ──────────────────────────────────────────
    # Per-highway-class tagged/defaulted counts, for the paper's data table
    # -- computed once here (authoritative), not recomputed downstream.
    by_class = Counter()          # (highway, is_default) -> edge count
    n_default = 0
    lane_provenance = Counter()
    n_ambiguous = 0
    multivalue_tags = 0
    for u, v, k, data in G.edges(keys=True, data=True):
        multivalue_tags += sum(isinstance(value, list) for value in data.values())
        lanes_eff, provenance, ambiguous = resolve_lanes(data)
        data["lanes_effective"] = lanes_eff
        data["lanes_source"] = provenance
        data["lanes_ambiguous"] = ambiguous
        data["lanes_is_default"] = provenance == "default"  # compatibility field
        lane_provenance[provenance] += 1
        n_ambiguous += int(ambiguous)
        highway = first(data.get("highway", "unclassified"))
        by_class[(highway, provenance == "default")] += 1
        if provenance == "default":
            n_default += 1

    n_edges = G.number_of_edges()
    n_direct = lane_provenance.get("tagged", 0) + lane_provenance.get("directional_tag", 0)
    n_split = lane_provenance.get("split_total", 0)
    print(f"Lane data: {n_direct} directly tagged, {n_split} split-total, "
          f"{n_default} defaulted "
          f"({100 * n_default / n_edges:.1f}%)")

    highway_classes = sorted({hc for hc, _ in by_class})
    lane_coverage_by_class = []
    for hc in highway_classes:
        tagged = by_class.get((hc, False), 0)
        defaulted = by_class.get((hc, True), 0)
        total = tagged + defaulted
        lane_coverage_by_class.append({
            "highway": hc, "tagged": tagged, "defaulted": defaulted,
            "total": total, "pct_default": round(100 * defaulted / total, 1),
        })

    # ── Export ───────────────────────────────────────────────────────────
    nodes_gdf, edges_gdf = ox.convert.graph_to_gdfs(G)
    edges_gdf = edges_gdf.reset_index()   # u, v, key become plain columns
    edges_gdf["edge_id"] = (
        edges_gdf["u"].astype(str) + "_" +
        edges_gdf["v"].astype(str) + "_" +
        edges_gdf["key"].astype(str)
    )

    if edges_gdf.empty or nodes_gdf.empty:
        raise RuntimeError("OSM export contains no nodes or edges")
    if edges_gdf.geometry.isna().any():
        raise RuntimeError("OSM export contains edges with missing geometry")

    # Flatten any remaining list-valued cells (GeoJSON needs scalars)
    for col in edges_gdf.columns:
        if edges_gdf[col].apply(lambda x: isinstance(x, list)).any():
            edges_gdf[col] = edges_gdf[col].apply(
                lambda x: ";".join(str(v) for v in x) if isinstance(x, list) else str(x)
            )
    nodes_gdf = nodes_gdf.reset_index()

    nodes_path = os.path.join(out_dir, "nodes.geojson")
    edges_path = os.path.join(out_dir, "edges.geojson")
    nodes_gdf.to_file(nodes_path, driver="GeoJSON")
    edges_gdf.to_file(edges_path, driver="GeoJSON")
    print(f"\nWritten: {nodes_path} ({len(nodes_gdf)} nodes)")
    print(f"Written: {edges_path} ({len(edges_gdf)} edges)")

    # ── Summary JSON -- the one authoritative record of this build, for
    #    any downstream reporting/paper script to read without recomputing.
    summary = {
        "name": args.name,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "python_version": platform.python_version(),
        "platform": platform.platform(),
        "bbox": {"west": raw_bbox[0], "south": raw_bbox[1],
                 "east": raw_bbox[2], "north": raw_bbox[3]},
        "prepared_bbox": {"west": bbox[0], "south": bbox[1],
                          "east": bbox[2], "north": bbox[3]},
        "margin_meters": args.margin_meters,
        "crs": str(edges_gdf.crs),
        "osmnx_version": ox.__version__,
        "network_type": "drive",
        "n_nodes": G.number_of_nodes(),
        "n_edges": n_edges,
        "n_scc": len(sccs),
        "largest_scc_size": len(largest),
        "largest_scc_pct": round(100 * len(largest) / G.number_of_nodes(), 1),
        "lanes_tagged": n_edges - n_default,  # compatibility aggregate
        "lanes_directly_tagged": n_direct,
        "lanes_split_total": n_split,
        "lanes_defaulted": n_default,
        "lane_provenance": dict(lane_provenance),
        "ambiguous_lane_splits": n_ambiguous,
        "multivalue_osm_tags": multivalue_tags,
        "lanes_pct_default": round(100 * n_default / n_edges, 1),
        "lane_coverage_by_class": lane_coverage_by_class,
    }
    summary_path = os.path.join(out_dir, "summary.json")
    summary["nodes_sha256"] = sha256_file(nodes_path)
    summary["edges_sha256"] = sha256_file(edges_path)
    with open(summary_path, "w", encoding="utf-8") as f:
        json.dump(summary, f, indent=2)
    print(f"Written: {summary_path}")


if __name__ == "__main__":
    main()
