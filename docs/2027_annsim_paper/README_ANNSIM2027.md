# ANNSIM 2027 paper (working folder)

Cloned from `docs/2026_annsim_paper_2026/` on 2026-10-30. Nothing here has been changed yet.
`bishi_annsim2026.tex` is still the ANNSIM 2026 paper. It stays until we are ready to start the 2027 paper.

## Conference
- ANNSIM '27, May 17-21, 2027, Wroclaw University of Science and Technology, Poland
- Paper submission: December 18, 2026 (a one-week extension is expected, not announced)
- Acceptance notification: February 9, 2027
- Camera-ready version: March 9, 2027
- Dates from https://scs.org/annsim, checked 2026-10-30. Check again before submitting.
- Check the page limit in the call for the chosen track.

## Plan
- Start after PADS 2027 (deadline November 13, 2026).
- Proposed topic, not confirmed: a micro-only study of which modeling choice matters most for
  lane-level accuracy (car-following law, lane-change rule, arrival process) on Donald Doyle and Redwood Creek.

## Before reusing the 2026 text
- The 2026 paper is published: cite it and write new prose.
- Its IDM values and fitness differ from the current code: `s_0` 5.0 m (now 2.0 m), `T` 3.0 s (now 1.0 s),
  fitness 0.5 flow + 0.5 speed (now 0.2 flow + 0.8 speed).
- `assets/myReference.bib` is missing 7 keys the paper cites. They are in `C:\Simulation\traffic-twin\myReference.bib`.
