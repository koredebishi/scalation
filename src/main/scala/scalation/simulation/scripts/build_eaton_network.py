#!/usr/bin/env python3
"""
Build a clean, simulation-ready node/edge network for the Eaton corridor
footprint using OSMnx, instead of the cosmetic-only Overpass polylines in
download_osm_geometry.py.

Produces two GeoJSON files:
    data/network/eaton/nodes.geojson  -- one row per intersection/endpoint
    data/network/eaton/edges.geojson  -- one row per road segment between nodes

Also prints a summary: node/edge counts, lane-tag coverage (real vs
defaulted), and a strong-connectivity check (largest strongly connected
component vs total nodes) so gaps can be caught before this feeds any
CorridorLayout/NetworkConfig assembly.

Usage:
    python src/main/scala/scalation/simulation/scripts/build_eaton_network.py

Bounding box is the one already validated visually against the Eaton model
(both I-210 WB and SR-134 WB sink ends confirmed inside it).
"""

import json
import os
from collections import Counter

import networkx as nx
import osmnx as ox

# ── Validated bounding box (from the auto-download that was visually
#    checked against the running Eaton model -- covers both corridor
#    sink ends: La Tuna Canyon on I-210 WB, San Fernando on SR-134 WB) ──
BBOX = (-118.304031, 34.131843, -118.061321, 34.244987)   # (west, south, east, north)

OUT_DIR = "data/network/eaton"
CACHE_DIR = "cache/eaton_model"

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
        except ValueError:
            continue
    return None


def resolve_lanes(row):
    """Per-direction lane count for one directed edge.
    Returns (lanes_effective, is_default).

    `reversed` (set by osmnx when it builds the graph) tells us which
    direction this edge represents: non-reversed edges were added first,
    in the OSM way's own node order (forward); reversed edges were added
    second, walking the way backward (backward).  So non-reversed -> use
    lanes:forward, reversed -> use lanes:backward.
    """
    highway = first(row.get("highway", "unclassified"))
    reversed_ = bool(first(row.get("reversed", False)))
    lanes = parse_int(row.get("lanes"))

    if row.get("oneway", False):
        if lanes is not None:
            return lanes, False
    else:
        directional_tag = row.get("lanes:backward") if reversed_ else row.get("lanes:forward")
        directional = parse_int(directional_tag)
        if directional is not None:
            return directional, False
        if lanes is not None:
            # Single lanes tag on a two-way street == total across both
            # directions by OSM convention; split it.
            return max(1, lanes // 2), False

    return DEFAULT_LANES.get(highway, 1), True


def main():
    os.makedirs(OUT_DIR, exist_ok=True)

    # osmnx caches the raw Overpass response under a hash-named file; all repo
    # caches live in cache/<model>/.
    ox.settings.cache_folder = CACHE_DIR

    # Pull lanes:forward / lanes:backward too -- not in osmnx's default
    # edge-tag list, and without them a two-way street's per-direction
    # lane counts can't be told apart from the total.
    ox.settings.useful_tags_way = list(
        set(ox.settings.useful_tags_way) | {"lanes:forward", "lanes:backward"}
    )

    print(f"Downloading drive network for bbox={BBOX} ...")
    # Download unsimplified, then simplify while forcing a break wherever
    # lanes/maxspeed change -- plain simplify=True would merge segments
    # with different lane counts into one edge and silently pick one.
    G = ox.graph_from_bbox(bbox=BBOX, network_type="drive", simplify=False)
    G = ox.simplify_graph(
        G, edge_attrs_differ=["lanes", "lanes:forward", "lanes:backward", "maxspeed"]
    )
    print(f"Graph: {G.number_of_nodes()} nodes, {G.number_of_edges()} edges")

    # ── Strong connectivity check ───────────────────────────────────────
    sccs = list(nx.strongly_connected_components(G))
    largest = max(sccs, key=len)
    print(f"Strongly connected components: {len(sccs)}")
    print(f"Largest SCC: {len(largest)} / {G.number_of_nodes()} nodes "
          f"({100 * len(largest) / G.number_of_nodes():.1f}%)")
    if len(sccs) > 1:
        sizes = sorted((len(c) for c in sccs), reverse=True)
        print(f"  other component sizes (top 10): {sizes[1:11]}")
        print("  -> two-way dead ends are NOT the cause (osmnx adds both "
              "directions for two-way streets, so they stay in the main "
              "component).  Nodes outside the largest SCC are almost "
              "always one-way-only subgraphs -- ramps/couplets cut by the "
              "bounding box, or genuine oneway tagging errors.")

    # ── Resolve lanes per edge ──────────────────────────────────────────
    # Per-highway-class tagged/defaulted counts, for the paper's data table
    # -- computed once here (authoritative), not recomputed downstream.
    by_class = Counter()          # (highway, is_default) -> edge count
    n_default = 0
    for u, v, k, data in G.edges(keys=True, data=True):
        lanes_eff, is_default = resolve_lanes(data)
        data["lanes_effective"] = lanes_eff
        data["lanes_is_default"] = is_default
        highway = first(data.get("highway", "unclassified"))
        by_class[(highway, is_default)] += 1
        if is_default:
            n_default += 1

    n_edges = G.number_of_edges()
    print(f"Lane data: {n_edges - n_default} tagged, {n_default} defaulted "
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

    # Flatten any remaining list-valued cells (GeoJSON needs scalars)
    for col in edges_gdf.columns:
        if edges_gdf[col].apply(lambda x: isinstance(x, list)).any():
            edges_gdf[col] = edges_gdf[col].apply(
                lambda x: ";".join(str(v) for v in x) if isinstance(x, list) else x
            )
    nodes_gdf = nodes_gdf.reset_index()

    nodes_path = os.path.join(OUT_DIR, "nodes.geojson")
    edges_path = os.path.join(OUT_DIR, "edges.geojson")
    nodes_gdf.to_file(nodes_path, driver="GeoJSON")
    edges_gdf.to_file(edges_path, driver="GeoJSON")
    print(f"\nWritten: {nodes_path} ({len(nodes_gdf)} nodes)")
    print(f"Written: {edges_path} ({len(edges_gdf)} edges)")

    # ── Summary JSON -- the one authoritative record of this build, for
    #    any downstream reporting/paper script to read without recomputing.
    summary = {
        "bbox": {"west": BBOX[0], "south": BBOX[1], "east": BBOX[2], "north": BBOX[3]},
        "osmnx_version": ox.__version__,
        "network_type": "drive",
        "n_nodes": G.number_of_nodes(),
        "n_edges": n_edges,
        "n_scc": len(sccs),
        "largest_scc_size": len(largest),
        "largest_scc_pct": round(100 * len(largest) / G.number_of_nodes(), 1),
        "lanes_tagged": n_edges - n_default,
        "lanes_defaulted": n_default,
        "lanes_pct_default": round(100 * n_default / n_edges, 1),
        "lane_coverage_by_class": lane_coverage_by_class,
    }
    summary_path = os.path.join(OUT_DIR, "summary.json")
    with open(summary_path, "w", encoding="utf-8") as f:
        json.dump(summary, f, indent=2)
    print(f"Written: {summary_path}")


if __name__ == "__main__":
    main()
