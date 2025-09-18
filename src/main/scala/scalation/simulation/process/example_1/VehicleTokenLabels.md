# Vehicle Token Number Labels — Implementation Notes

This document summarizes the minimal implementation that displays compact, human‑readable vehicle numbers (e.g., M-1, R1-7) directly on animation tokens.

- Token drawing shows labels (DgAnimator).
- Token creation accepts explicit labels (Model).
- Vehicle sources supply M-#/R#-# labels (VSource).

---

## 1) DgAnimator.scala — Token label rendering

Purpose:
- Render the token’s `label` string centered over each token (node-bound, edge-bound, and free).

Used by:
- The animation pipeline whenever tokens are drawn; labels are provided via `AnimateCommand.label`.

Key addition:
- A tiny helper to draw a centered label with dynamic font sizing, called after each token is filled.

Example (helper and usage):

```scala
/** Draw a token label centered on the token shape.
  * @param g2d   hi-res graphics context used by the animator canvas
  * @param token the token whose label should be rendered (uses token.label)
  * Notes:
  * - Font size adapts to token height for readability (clamped 10..24).
  * - Centers text over the token "head"; does not rotate with heading.
  * - Used by: Canvas.paintComponent after each token is filled (node-bound,
  *   edge-bound, and free tokens).
  */
private def drawTokenLabel (g2d: Graphics2D, token: graph.Token): Unit =
    val lbl = token.label
    if lbl != null && lbl.nonEmpty then
        val b      = token.shape.getBounds2D
        val size   = math.max(10, math.min(24, (b.getHeight * 0.8).toInt)) // clamp font size
        val prevF  = g2d.getFont
        val dynF   = new Font ("SansSerif", Font_BOLD, size)
        g2d.setFont (dynF)
        val fm     = g2d.getFontMetrics
        val x      = (b.getCenterX - fm.stringWidth(lbl) / 2.0).toFloat    // center horizontally
        val y      = (b.getCenterY + fm.getAscent / 3.5).toFloat           // center vertically
        g2d.setPaint (black)
        g2d.drawString (lbl, x, y)
        g2d.setFont (prevF)
    end if
end drawTokenLabel

// After filling each token shape:
g2d.setPaint (token.color)
g2d.fill (token.shape)
drawTokenLabel (g2d, token)   // render the vehicle number/label on token head
```

---

## 2) Model.scala — Explicit label animation API

Purpose:
- Provide a clear, unambiguous API to enqueue animation commands with a caller-supplied label.
- Avoid overload confusion with edge animation.

Used by:
- `VSource` when creating vehicle tokens to ensure the token shows the per-source label (e.g., M-7, R1-12).

Method:

```scala
/** Enqueue an animation command with an explicit display label.
  * @param who    Identifiable being animated (provides unique id used as eid)
  * @param what   Animation command (e.g., CreateToken)
  * @param color  Paint color for the node/token
  * @param shape  Shape to render (Ellipse, Rectangle, etc.)
  * @param at     Location/size array for the shape (x, y, w, h) or variant
  * @param label  The display label to show on the token/node (overrides name)
  * Behavior:
  * - Uses the supplied label for AnimateCommand.label; falls back to who.name
  *   if the supplied label is null/empty.
  * - Leaves internal ids and simulation logic unchanged; this is display-only.
  * Used by:
  * - VSource when creating vehicle tokens so the animator shows compact
  *   per-source labels (e.g., M-1, R1-7) on token heads for visual validation.
  */
def animateWithLabel (who: Identifiable, what: CommandType, color: Color, shape: Shape,
                      at: Array [Double], label: String): Unit =
    if animating then
        val eid = who.id
        val lbl = if label != null && label.nonEmpty then label else who.name
        debug ("animateWithLabel", s"$lbl.$eid, $what, $color, $shape, ${stringOf (at)}")
        aniQ.add (AnimateCommand (what, eid, shape, lbl, true, color, at, _clock))
    end if
end animateWithLabel
```

Notes:
- Existing `animate(...)` overloads remain for nodes/edges and default behaviors.

---

## 3) VSource.scala — Per-source display labels

Purpose:
- Generate compact, origin-aware labels for vehicles as they are created:
  - Mainline → `M-#`
  - Ramp 1 → `R1-#`
  - Ramp 2 → `R2-#`
- Keep labels readable and stable for visual validation.

Used by:
- The vehicle creation loop in `VSource.act()` before enqueuing `CreateToken`.

State and label construction:

```scala
/** Monotonic per-source counter used solely for display labels on tokens.
  * Example: for mainline (M), the sequence produces M-1, M-2, ...; for ramp 1,
  * R1-1, R1-2, ...
  * Notes:
  * - This does NOT affect simulation ids or logic (purely presentation).
  * - Reset occurs on JVM restart; not reset between replications by default.
  */
private var seq = 0

/** Derive a short source prefix from the entity subtype.
  * @return "M" for mainline (subtype == 0), else "R<subtype>" (e.g., R1, R2)
  * Used by: label construction below to preserve origin after merging.
  */
private def srcPrefix: String = esubtype match
    case 0 => "M"
    case n => s"R$n"
```

During vehicle creation:

```scala
// Inside VSource.act() when a new vehicle is made:
seq += 1                                           // advance per-source counter (display only)
val displayLabel = s"${srcPrefix}-$seq"            // token label shown by animator (e.g., M-3)

// Enqueue the token with an explicit label so the animator paints "M-#/R#-#".
director.animateWithLabel(
  actor,                                           // vehicle (provides unique id)
  CreateToken,                                     // action to create the token
  randomColor(actor.id),                           // color (unchanged logic)
  Ellipse(),                                       // shape (unchanged)
  Array(                                           // token position near source
    at(0) + at(2) + RAD / 2.0,
    at(1) + at(3) / 2.0 - RAD
  ),
  displayLabel                                     // the visible token label
)
```

Inline rationale:
- `seq` is per source to keep numbers compact and ordered locally.
- `srcPrefix` preserves origin after vehicles merge (e.g., ramp cars remain `R1-*`).
- Internal ids and simulation logic are not changed; this is only for display.

---

## Optional hybrid (not implemented)
If desired later, keep the prefix and clamp the numeric part to two digits:

- First emission per source establishes a base (so first car shows 00).
- For each car: `rel = seq - base`, `num = (rel % 100 + 100) % 100`, label `= s"$prefix-${num.formatted("%02d")}"`.
- Trade-off: short labels, but wrap every 100 per source.

---

## Summary

- DgAnimator renders the token label centered on the token.
- Model adds an explicit-label enqueue API to avoid overload ambiguity.
- VSource supplies readable per-source labels and passes them explicitly for rendering.

