# AutoCAD LISP routines

Custom AutoLISP (`.lsp`) tools for a scenery / carpentry shop. Users are
shop carpenters and drafters, not programmers: explain AutoCAD steps in
plain terms (which command to type, what to click).

## Repo and git

- GitHub repo: `drpa-dev/autocad-lisp`, **public**, branch `main`.
  Anyone can read it, so never put personal names, emails, client or
  project names, or local file paths in files or commit messages.
- Each routine is one file in `LISPs/`, e.g. `LISPs/HFLAT.lsp`. Keep file
  names and paths stable: users load them via APPLOAD / the Startup Suite.
- After a change is made and checked, commit it and push to `main`.
  Commit messages: a short summary line, then what changed in shop terms.
- Commit identity is set in this repo (GitHub noreply address); don't
  change it or set a global git identity.
- Tools on this PC: git at `%LOCALAPPDATA%\Programs\Git\cmd\git.exe`,
  GitHub CLI at `C:\Program Files\GitHub CLI\gh.exe` (may not be on PATH
  in Claude's shell).

## Testing

- Claude cannot run AutoCAD. Check parentheses balance (ignoring strings
  and `;` comments) after every edit, say plainly that the routine is
  untested in AutoCAD, and give the user specific test cases to try on a
  scratch drawing.
- Remind the user to reload the `.lsp` (APPLOAD or drag onto the drawing)
  after an update; AutoCAD keeps the old version in memory until then.
- If a routine reuses existing block definitions by name, warn that blocks
  already in a drawing keep the old geometry until purged or renamed.

## Coding conventions

Follow the patterns in `LISPs/HFLAT.lsp`:

- Prefix helper functions per routine (`hf:` for HFLAT) to avoid clashes
  with other loaded LISP; declare all locals in the `defun`.
- Wrap each command with `(*push-error-using-command*)` /
  `(*pop-error-mode*)` and a local `*error*` that restores everything.
- Save and restore system variables you change (CMDECHO, OSMODE,
  3DOSMODE, CLAYER). Turn snaps off while drawing geometry.
- Make each command a single UNDO step (`UNDO _BE` / `_E`, only when
  UNDOCTL allows it).
- Build geometry in the World UCS, then restore the user's UCS
  (`UCS _P`) before any insert or pick, so results follow their UCS.
- Handle layers that may be frozen or locked: thaw/unlock while building,
  then put them back.
- Size prompts: accept plain numbers in drawing units and feet/inches
  typed with `'` and `"` (`8'`, `8'6"`, `96"`) in any drawing units;
  reject zero/negative values. Scale defaults from INSUNITS.
- Use `._` / `_` prefixes on commands and options so routines work in
  non-English AutoCAD.
- Prefer plain AutoLISP and `vl-` string/list functions; avoid
  `vla-`/`vlax-` (ActiveX) unless needed, since AutoCAD LT doesn't
  support them.

## Theatre construction notes

- Hollywood flat = framing on edge (1x4: 3/4" against the skin, 3.5"
  deep), rails full width, stiles and toggles between the rails, 1/4"
  lauan skin. Broadway flat = framing laid flat; don't confuse the two.
- Sheet goods are 4'x8'. Seams should land on framing where possible.
