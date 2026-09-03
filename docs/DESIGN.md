# Cardano dbsync — design system

Written 2026-08-23.  

This file describes **what the system is**. `SPECIFICATION.md` states **what the build must
satisfy**. Where a value appears in both, this file is descriptive and the criterion is normative.

**Single origin.** `dbsync-project/design/tokens.source.json` is canonical. `tokens.css`, the 
token library, and **the colour tables in §3 of this file** are all generated from it. No hex in
this document was typed by hand; regenerate rather than edit. Penpot file: `Cardano dbsync`,
id `13f47616-a8b0-8079-8008-6819fda90bde`, verified at **revn 1330**, health CLEAN.

---

## 1. The argument

Flat geometry. No shadows, no gradients, no photography, no illustration-as-decoration. **Elevation
is declared once, by border** — the file runs ~516 hairlines at roughly 1.3:1 as its entire depth
system, which is a deliberate visual language rather than a contrast defect (§6).

Three families, thirteen styles, no more. Hexagon-and-dot form language. Chain vocabulary is the
users' vocabulary and is never softened: block, epoch, slot, tip, era, rollback, snapshot, SMASH,
off-chain pool data, `tx_out`, `consumed_by_tx_id`. Schema identifiers always render in mono.

Four themes — light, dark, cream, intersect-navy — are **token values, not component copies**. A
theme overrides base by name only; it never introduces a name a theme-less build would miss.

---

## 2. Semantics

`accent.primary` is the single interactive colour: links, active nav, primary buttons, focus rings.
**It is a fill.** Text and links take `accent.primary-text`. `accent.secondary` is the unique accent
and carries *health* — synced, at tip, progress complete. It is never a second call-to-action.

Cardano Blue, Intersect Navy and Electric Blue are **lineage, not palette**. `brand.intersect-navy`
is never theme-overridden, which is exactly why no component may bind body text to it.

Teal = healthy · amber = warning · vermilion = error and destructive only, never decoration. Each
has a `*-subtle` surface companion used as the admonition background.

**Dark is a real mode, not an inversion.** Accents *lighten* so contrast holds. Never paint the
light accent on a dark surface.

**Text colour is not fill colour.** Only `text.*`, `code.text*` and `*-text` tokens may appear in a
text position; reach for the missing themed role rather than a fill.

**`code.*` surfaces are theme-invariant dark in all four themes.** Code gains weight by going
inverse, not by lifting.

---

## 3. Colour — generated from `tokens.source.json`

68 base tokens. **Bold** marks a value a theme overrides; `code` marks one inherited from base.
Overrides: dark 32 · cream 11 · navy 32.

### surface

| token | light | dark | cream | navy |
|---|---|---|---|---|
| `surface.base` | `#ffffff` | **#0f1117** | **#fcfbf5** | **#011555** |
| `surface.sunken` | `#f0f2f5` | **#161b22** | **#eae6d8** | **#0b1b45** |
| `surface.accent-subtle` | `#eaf2ff` | **#10233f** | `#eaf2ff` | **#0e2a6b** |
| `surface.warm` | `#f5f3eb` | **#0e2a25** | `#f5f3eb` | **#010e33** |
| `surface.inverse-selected` | `#10233f` | `#10233f` | `#10233f` | `#10233f` |
| `surface.selected` | `#17c3a5` | **#2dd4b5** | `#17c3a5` | **#2dd4b5** |
| `surface.inverse-tip` | `#0e2a25` | `#0e2a25` | `#0e2a25` | `#0e2a25` |
| `surface.navy-raised` | `#0b1b45` | `#0b1b45` | `#0b1b45` | `#0b1b45` |

### border

| token | light | dark | cream | navy |
|---|---|---|---|---|
| `border.default` | `#e1e4e8` | **#30363d** | **#e4dfd2** | **#21356e** |
| `border.strong` | `#d0d7de` | **#30363d** | **#d6cfbc** | **#2e4589** |
| `border.warm` | `#e4dfd2` | **#30363d** | `#e4dfd2` | **#21356e** |
| `border.warm-strong` | `#d6cfbc` | `#d6cfbc` | `#d6cfbc` | `#d6cfbc` |
| `border.selected` | `#0b7c68` | **#2dd4b5** | `#0b7c68` | **#2dd4b5** |
| `border.interactive` | `#7e8893` | **#6e7781** | **#857e6a** | **#8fa3d9** |
| `border.inverse` | `#30363d` | `#30363d` | `#30363d` | `#30363d` |

### text

| token | light | dark | cream | navy |
|---|---|---|---|---|
| `text.primary` | `#1a1a2e` | **#e6edf3** | **#2a2419** | **#e6edf3** |
| `text.secondary` | `#57606a` | **#9aa4b2** | **#6e6656** | **#8fa3d9** |
| `text.tertiary` | `#686f77` | **#8b949e** | **#6e6656** | **#748dd0** |
| `text.placeholder` | `#6e7781` | **#8b949e** | **#6e6656** | **#748dd0** |
| `text.on-navy` | `#8fa3d9` | `#8fa3d9` | `#8fa3d9` | `#8fa3d9` |
| `text.warm-tertiary` | `#6e6656` | **#8b949e** | `#6e6656` | **#8fa3d9** |
| `text.on-selected` | `#1a1a2e` | **#0f1117** | **#2a2419** | **#0f1117** |
| `text.disabled` | `#9aa4b2` | **#59616a** | **#9e9482** | **#4364bf** |

### accent

| token | light | dark | cream | navy |
|---|---|---|---|---|
| `accent.primary` | `#0e6ff7` | **#4493f8** | `#0e6ff7` | **#4493f8** |
| `accent.primary-hover` | `#0b57c4` | **#79b8ff** | `#0b57c4` | **#79b8ff** |
| `accent.primary-active` | `#0a4ba8` | **#79b8ff** | `#0a4ba8` | **#79b8ff** |
| `accent.primary-text` | `#0b57c4` | **#4493f8** | `#0b57c4` | **#79b8ff** |
| `accent.primary-mid` | `#7fa8ff` | `#7fa8ff` | `#7fa8ff` | `#7fa8ff` |
| `accent.primary-tint` | `#c8d5fd` | **#10233f** | `#c8d5fd` | **#0e2a6b** |
| `accent.secondary` | `#17c3a5` | **#2dd4b5** | `#17c3a5` | **#2dd4b5** |
| `accent.secondary-tint` | `#b7e4d8` | **#0e2a25** | `#b7e4d8` | **#010e33** |
| `accent.secondary-deep` | `#06584a` | **#2dd4b5** | `#06584a` | **#2dd4b5** |
| `accent.primary-inverse` | `#4493f8` | `#4493f8` | `#4493f8` | `#4493f8` |
| `accent.primary-inverse-hover` | `#79b8ff` | `#79b8ff` | `#79b8ff` | `#79b8ff` |
| `accent.navy-primary` | `#5b82ff` | `#5b82ff` | `#5b82ff` | `#5b82ff` |
| `accent.navy-primary-hover` | `#85a3ff` | `#85a3ff` | `#85a3ff` | `#85a3ff` |
| `accent.secondary-inverse` | `#2dd4b5` | `#2dd4b5` | `#2dd4b5` | `#2dd4b5` |

### brand

| token | light | dark | cream | navy |
|---|---|---|---|---|
| `brand.cardano-blue` | `#0033ad` | **#6685ce** | `#0033ad` | **#6685ce** |
| `brand.intersect-navy` | `#011555` | `#011555` | `#011555` | `#011555` |
| `brand.electric-blue` | `#2353ff` | `#2353ff` | `#2353ff` | `#2353ff` |

### status

| token | light | dark | cream | navy |
|---|---|---|---|---|
| `status.warning` | `#e5a100` | `#e5a100` | `#e5a100` | `#e5a100` |
| `status.warning-subtle` | `#fff6de` | **#2e2410** | `#fff6de` | **#2e2410** |
| `status.warning-text` | `#8a6100` | **#e5a100** | `#8a6100` | **#e5a100** |
| `status.danger` | `#fd551f` | **#ff8a65** | `#fd551f` | **#ff8a65** |
| `status.danger-subtle` | `#ffe3d9` | **#331410** | `#ffe3d9` | **#331410** |
| `status.danger-text` | `#a83208` | **#ff8a65** | `#a83208` | **#ff8a65** |

### code

| token | light | dark | cream | navy |
|---|---|---|---|---|
| `code.surface` | `#0f1117` | `#0f1117` | `#0f1117` | `#0f1117` |
| `code.surface-raised` | `#161b22` | `#161b22` | `#161b22` | `#161b22` |
| `code.border` | `#30363d` | `#30363d` | `#30363d` | `#30363d` |
| `code.text` | `#e6edf3` | `#e6edf3` | `#e6edf3` | `#e6edf3` |
| `code.text-muted` | `#8b949e` | **#9aa4b2** | `#8b949e` | **#8fa3d9** |
| `code.prompt` | `#79b8ff` | `#79b8ff` | `#79b8ff` | `#79b8ff` |
| `code.accent` | `#2dd4b5` | `#2dd4b5` | `#2dd4b5` | `#2dd4b5` |

### The ladder, per theme

The four-step text ladder is **honest per theme, not uniform** — see SPECIFICATION TOK-11. Cream's
two steps are forced, not chosen: its `text.secondary` sits at 4.55 on `surface.sunken`, leaving no
headroom for a distinct third step that still clears AA.

| theme | steps | secondary | muted | disabled |
|---|---|---|---|---|
| light | 4 | `#57606a` | `#686f77` / `#6e7781` | `#9aa4b2` |
| dark | 3 | `#9aa4b2` | `#8b949e` | `#59616a` |
| cream | 2 | `#6e6656` | — | `#9e9482` |
| navy | 3 | `#8fa3d9` | `#748dd0` | `#4364bf` |

`text.disabled` is exempt from 1.4.3 as an inactive component; it is themed for legibility and to
keep a disabled field from being pixel-identical to an active one.

---

## 4. Typography

Thirteen styles. **Metrics live here, not in the Penpot file** — every one of the 2,601 text shapes
on a theme page is a hand-positioned single-line box, so raising line height there would re-measure
2,601 box heights and break the drawn geometry. The file specifies colour and composition; leading
lives in this table.

| style | family | size | weight | line-height | tracking |
|---|---|---|---|---|---|
| `display-hero` | Space Grotesk | 58 | 700 | 1.1 | -0.02em |
| `heading-lg` | Space Grotesk | 38 | 700 | 1.2 | -0.02em |
| `heading-md` | Space Grotesk | 26 | 700 | 1.25 | -0.01em |
| `title` | Space Grotesk | 20 | 600 | 1.3 | 0 |
| `wordmark` | Space Grotesk | 19 | 300 | 1.2 | 0 |
| `body` | Inter | 15 | 400 | **1.65** | 0 |
| `body-sm` | Inter | 13 | 400 | 1.6 | 0 |
| `label` | Inter | 14 | 600 | 1.2 | 0 |
| `label-sm` | Inter | 12 | 700 | 1.2 | 0.04em |
| `caption` | Inter | 12 | 400 | 1.4 | 0 |
| `mono` | JetBrains Mono | 13 | 400 | 1.5 | 0 |
| `mono-sm` | JetBrains Mono | 11.5 | 400 | 1.5 | 0 |
| `eyebrow` | Inter | 13 | 600 | 1.2 | 0 |

Docs prose measure is **65–75ch** (the file draws 74ch and is right). No type size outside these
thirteen. Mono is for code, chain data and measurement only — never as a costume.

---

## 5. Layout, space and shape

**Space** — `4` 4px · `8` 8px · `12` 12px · `16` 16px · `24` 24px · `32` 32px · `48` 48px · `64` 64px · `96` 96px

**Radius** — `sm` 6px · `md` 8px · `lg` 10px · `xl` 12px · `xxl` 16px · `pill` 999px

Card radii 12–16px; pills for small controls only. Space and radius tokens carry units — they were
emitted unitless until 2026-08-23, which silently zeroed every declaration consuming them.

**Nothing below 1440px is drawn.** All 245 boards are 1440 wide; mobile, tablet and wide
compositions are the build's to author, along with the locale switcher and version selector, which
appear on no board. Text containers must survive **+35% string growth** — measured trap: the nav
links span 403px and reach x=964 at +35%, overlapping a search field that starts at x=880.

---

## 6. How contrast is measured here

Keep this. Three earlier verdicts were wrong because the method was wrong.

**A text's real backdrop is the smallest solid shape that geometrically CONTAINS it** — not its
ancestor. Many panels are *siblings* drawn behind their content. Ancestor-walking gives the wrong
backdrop and every verdict built on it is wrong.

**Button plates are `path` shapes, not rects.** A scanner considering only rect/frame/circle falls
through to the board and reports every button label as invisible — this produced the phantom "98
light failures". Include paths, using `selrect` for geometry.

**Measure per surface, never against white alone.** Three findings came from generalising a
white-background measurement. And measure each token against the surface it is **specified on** —
testing every token against every surface manufactures failures that cannot occur.

**Non-text (1.4.11) carries two judgements that must not be quietly reversed:**

1. **`control` vs `decorative`.** A finding counts only if the shape or an ancestor is named like an
   interactive element (`theme-ctl`, `choice-box`, `toggle*`, `cap:*`, `btn*`, `res-icon`,
   `copy-*`). 1.4.11 does not apply to a decorative hairline, and this file runs 516 of them as its
   depth system. Counting those as failures would mean redesigning the visual language.
2. **An outline counts as the boundary.** If a shape's own stroke clears 3:1 against the backdrop,
   the shape is bounded even when its fill does not.

**Ghost/flat elements that sit at ~1:1 with their surface by design** are not defects. They are
identified by their boundary, which must still clear 3:1.

**Cardano brand-blue logo marks are exempt** as logotypes under 1.4.3 — marks only, never running
text. **Disabled controls are exempt** under 1.4.3 as inactive components.

Scripts: `geo_contrast.py` (text), `nontext_contrast.py` (borders, icons, dividers),
`penpot_health.py` (structure and token bindings, on a schedule). Note the health check verifies
structure, **not off-system literals** — it returned CLEAN at revn 1330 with 204 raw `#000000` on
the prototype page.

---

## 7. Working on the file headlessly

Reads and writes both work through the Penpot API with no browser tab, using the standing token
grant (`PENPOT_API_TOKEN`, a developer-machine credential that never enters CI).

- **Read**: `POST /api/rpc/command/get-file` with `accept: application/json` returns plain JSON.
- **Write**: `POST /api/rpc/command/update-file` with `content-type: application/transit+json`;
  kebab-case keyword keys, real UUID values. A plain-JSON body fails schema validation on nested
  uuid fields.
- Recolouring text: remap `fills` entries carrying the whole fill object (so token bindings come
  with it), convert camelCase → kebab-case keywords, then send `mod-obj` with **two** set
  operations — `content` and `position-data` = nil. Without the second the screen keeps the old
  colour. Batches of 25–30 shapes.

**Diagnostic order, cheapest first:** visibility flags (`hidden`, `opacity`) on the shape *and every
ancestor* → dangling `fillColorRefId` (renders as nothing, looks correct in every dump) → z-order
occlusion → prototype overlay `action.position` (`manual` positions against the viewport; use
`center`) → render ghosting, as a last resort.

**Gotchas.** `clone()` inherits `hidden`. Mutating `action.position` does not persist — remove and
re-add the interaction. The plugin is page-scoped for geometry; the headless API is not.
Cross-page prototype links are impossible, so the full page cannot be split by theme without
severing the theme switcher.

**Verification standard: never say "fixed" from a property dump.** Export the board to PNG through
Penpot and read the pixels. That is the only proof that survives a reload.

**Superseded, never revive:** repainting navy panels to `#0b1b45` — 1.014:1 on `#011555`, still
invisible. Correct navy flat-panel values are `surface.warm` `#010e33` for footer plates and
`border.strong` `#2e4589` for margin rails.

---

## 8. Components

96 components in the file are **27 unique definitions** — 23 forked four ways per theme, plus four
deliberately theme-invariant. **Build 27, not 96**: themes are token values, not component copies.

Button (Primary, Secondary, Ghost, Danger, Disabled) · Card (Choice, Data option, Option, Query,
Sample query, Stat, Team, Tool, Use case) · Chip (Filter, Tag) · Navbar (Full, Minimal) · Section
(Cross, Divider) · Key/Cap · Search/Result · Pagination/Prev-Next · Code/Copy · Avatar/Hexagon ·
Footer/Navy · Metric/Dark.

`Code / Copy` and `Footer / Navy` are correctly theme-invariant (code surfaces are invariant by
decision; the footer binds `brand.intersect-navy`). `Metric / Dark` and `Avatar / Hexagon` are
unruled and both encode a theme in their name — decide and rename.

**States are drawn for three primitives only** (button, link, input). The other eight controls have
no drawn states, and **focus indicators appear nowhere across 212 theme-page boards**. Both are the
build's to author — see SPECIFICATION §20.

---

## 9. Prohibitions

- No shadows, gradients, photography, or illustration-as-decoration.
- No coloured `border-left`/`border-right` above 1px on cards, callouts or list items.
- No gradient text. No emoji or unicode glyph standing in for an icon — icons come from one drawn
  set at consistent stroke and weight.
- No monospace as a costume.
- No raw hex in a component. No second palette. No colour invented outside `tokens.source.json` —
  if it is not in the token source, it does not exist; ask.
- No theme override byte-identical to base.
- The prototype page (`Site Pages Designs - full`) is **never** a colour source: it carries 204
  off-system `#000000` against one per theme page.
