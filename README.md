# AutoCAD LISP Routines

Custom AutoLISP (`.lsp`) routines for our AutoCAD drawings.

## Routines

| File | Commands | What it does |
|---|---|---|
| `LISPs/HFLAT.lsp` | `HFLAT`, `HFLATLIB` | Builds a 3D Hollywood flat (framing on edge plus lauan skin) as a block, or a library of standard widths. |
| `LISPs/KerfBend.lsp` | `KERFBEND`, `KERFPATH`, `KERFCALC`, `KERFHELP` | Plans and draws kerf bending: kerf depth, count and spacing, the flat cut layout, and a preview of the bent board. `KERFPATH` works from a curve you've already drawn. |
| `LISPs/VCPREP.lsp` | `VCPREP` | Step-by-step cleanup of a drawing for VCarve: explodes and flattens, removes junk and duplicate lines, joins and closes shapes, sorts them onto cut layers (or skips that), and saves a clean DXF. Leaves notes in the drawing (layer VCPREP-LOG) and a log file saying what changed where. |

Sizes can be typed in feet and inches (`8'`, `8'6"`, `3/4"`) in any drawing.

## Loading a routine

1. In AutoCAD, type `APPLOAD` and press Enter.
2. Browse to the `.lsp` file and click **Load**.
3. To load it automatically every session, add it to the **Startup Suite** in the same dialog.
4. Run the routine by typing its command name at the command line.
