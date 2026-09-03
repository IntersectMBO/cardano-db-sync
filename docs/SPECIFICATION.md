# Cardano dbsync docs — build specification

Written 2026-08-21, last revised **2026-08-23**. **Supersedes and replaces `REBUILD_SPEC.md`,
`ARCHITECTURE.md`, `THEME_STATE.md` and `IMPECCABLE_AUDIT.md`**, none of which are maintained
separately any longer. Companion: `DESIGN.md` (the design system) and `CHECKLIST.md` (build items).

Verified against: Penpot file `Cardano dbsync` at **revn 1337** (health check CLEAN),
`cardano-db-sync` at 115 release tags, `dbsync-project/design/tokens.source.json`.

Authorities, in order: this file → `DESIGN.md` (the design system) → `PRODUCT.md` (durable
product truth) → the Penpot file (colour, geometry and composition only — see §14.5).

---

## 1. How to read this

The document has two halves, and every numbered criterion belongs to one of them.

- **§§4–12 are mechanism** — how the site is built. `ENV`, `CON`, `GDE`, `SCH`, `VER`, `SRCH`,
  `SUR`, `NUM`, `BLD`, `THM`.
- **§§13–19 are the quality floor** — what the built site must satisfy regardless of how it is
  built. `FLOOR`, `TOK`, `A11Y`, `CONTENT`, `NAV`, `PERF`, `DF`. These were graded out of 42 audit
  findings; the outstanding ones are listed in Appendix A.

The two halves complement rather than compete: the floor states a requirement, the mechanism
states how it is met and enforced. `TOK-1`, `TOK-5`, `TOK-12` and `TOK-14` are requirements;
`THM-4`'s CI token lint is what makes them fail loudly. `A11Y-1` is a requirement; `BLD-2` runs the
axe pass. Where both speak, both apply.

Criterion IDs are stable and namespaces are disjoint. Cite them as `SUR-16`, not by section.

---
## 2. Decisions register — settled, do not relitigate

### 2.1 Build decisions

| # | Decision | Why |
|---|---|---|
| D1 | **Docusaurus**, replacing the Astro site in `docs/` | i18n, docs-versioning and a real component layer; conventional for an IntersectMBO-bound repo. Makes the `--ifm-*` bridge in `tokens.css` load-bearing, not dead weight. Authoring stays CommonMark (§4.4) |
| D2 | **Docs are authored as annotations in Haskell source**, extracted to a committed JSON artifact | Reference content cannot drift from the code it describes |
| D3 | `doc/` **is left untouched** as reference until the new workflow reaches parity | Upstream PRs keep landing there; parity is measured, not asserted (§4.4) |
| D4 | **Schema explorer generated** from `cardano-db` schema types | The old generator was removed in the Persistent→Hasql migration; `doc/schema.md` has rotted by hand ever since |
| D5 | **Survey is on-chain, CIP-179, metadata label 17** | No backend exists to receive a form (D7). Makes the site a reference CIP-179 consumer |
| D6 | **Hosting is GitHub Pages.** Compute is GitHub Actions (build) and the browser (runtime). No server, ever | Hard constraint |
| D7 | **No backend.** Every dynamic surface is a build-time artifact or a client-side computation | Follows from D6 |
| D8 | **Prose docs version at minor lines**, 5 rolling; **schema versions independently at migration boundaries** | Two axes sized to their cost: ~130 pages vs 7.5 KB (§6) |
| D9 | **Search is two layers: a purpose-built entity index, plus Pagefind for prose** | The design's faceted rows and its drawn `tx` &rarr; `tx_out` partial matching need a tokenizer we control; prose search is a separate, solved problem (§7) |
| D10 | **Fork = developer site on preview; upstream = production on mainnet** | The only safe way to rehearse immutable on-chain operations (§3) |
| D11 | **Survey ownership is a native-script multisig** on mainnet | A single key can void an entire survey's responses (§8.6) |
| D12 | `tokens.source.json` is the single origin. `tokens.css` and the Penpot token library are generated from it | Carried from §2.2 item 1–5.2 |

---

### 2.2 Design-system decisions

Carried from the design cycle closed 2026-08-20. Referenced elsewhere as *§2.2 item N*.

1. **`tokens.source.json` is canonical.** Not `DESIGN.md` frontmatter, not `research/a11y-contrast.md`,
   not the Penpot library. The Penpot library is generated *from* the token source by flattening
   themes into prefixed names (`inverse-*` dark, `warm-*` cream, `navy-*` navy).
2. **`tokens.css` is generated, never hand-edited.** It regenerates byte-identically from the
   source through `design/tools/tokens-to-css.py`. Any change starts in the JSON.
3. **Four themes: light (`:root`), dark, cream, navy** — switched by `data-theme` on `<html>`.
   Theme sets override base **by name only**; a theme never introduces a name a theme-less build
   would miss. That discipline currently holds at zero violations — keep it.
4. **The primary blue is `#0e6ff7` (`accent.primary`), and it is a fill.** Text and links use
   `accent.primary-text` `#0b57c4`. `#0f73ff` is not in the system and never was; the widely-cited
   "4.27:1 link failure" was computed from it.
5. **The selected state is a named semantic role, not a repeated hex:** `surface.selected`
   (`#17c3a5` base, `#2dd4b5` dark/navy), `border.selected` (`#0b7c68` base — a real 1px boundary
   where the fill alone is only 2.24:1), `text.on-selected` (`#1a1a2e` base, `#2a2419` cream,
   `#0f1117` dark/navy). Use the role.
6. **Dark labels on blue plates.** A label on `accent.primary-inverse` `#4493f8` is dark, not
   light: `#0f1117` gives 6.09:1. This was chosen over darkening the plate, which would have
   repainted every button in navy. Normalise on `#0f1117` — it is the only one of the three
   in-file variants (`#000000`, `#161b22`, `#0f1117`) that is a real token.
7. **`code.*` surfaces are theme-invariant dark in all four themes.** Code blocks gain weight by
   going inverse, not by lifting. `code.text-muted` is `#8b949e` at base (6.14:1) — do not
   "fix" it back to a grey that fails.
8. **`brand.intersect-navy` is a lineage colour and is never theme-overridden.** That is why no
   component may use it as body text (TOK-4).
9. **Muted colours are validated against every surface, never against white alone.** Three
   findings came from generalising a white-background measurement.
10. **Ghost/flat elements that sit at ~1:1 with their surface by design** (e.g. `Button / Ghost`)
    are not contrast defects. They are identified by their boundary, which must still clear 3:1.
11. **Cardano brand-blue logo marks are exempt** as logotypes under WCAG 1.4.3. This exemption
    covers marks only — never running text.
12. **No shadows, no gradients, no photography, no illustration-as-decoration.** Elevation is
    declared once, by border. Card radii 12–16px; pills for small controls only.

---

## 3. What carries forward

**Carries forward, unchanged and canonical:**

- `dbsync-project/design/tokens.source.json` — 54 base colour tokens + `space`/`radius`
  scales; theme sets `dark` (30 overrides), `cream` (14), `navy` (30).
- `dbsync-project/design/tokens.css` — generated from it, byte-identically, by
  `design/tools/tokens-to-css.py`. Never hand-edited.
- The four themes and their switch mechanism: `:root` (light) + `[data-theme="dark"|"cream"|"navy"]`.
- The design argument itself: flat geometry, no shadows/gradients/photography, three type
  families (Space Grotesk headings / Inter UI / JetBrains Mono for chain data), hexagon-and-dot
  form language, chain vocabulary held un-softened.
- `DESIGN.md` — the design system: palette, type scale, components, measurement method and the
  Penpot working recipes. Its colour tables are generated from `tokens.source.json`.

**Does NOT carry forward:** `docs/src/styles/global.css` and its seven-name parallel palette;
the former `dbsync-project/DESIGN.md` and its frontmatter (stale against the tokens — see
§5); `research/a11y-contrast.md`'s tables (computed from `#0f73ff`, a colour not in the system);
the Astro/Cloudflare wiring; `ERD.png` unless §4 CONTENT-1 is satisfied.

**One open architecture question the rebuild must answer on day one:** `tokens.css` ends with an
**Infima bridge** (`--ifm-*`), i.e. it assumes Docusaurus. The old site was Astro. Either the new
stack is Docusaurus and the bridge is load-bearing, or the bridge is dead weight and must be
deleted from the generator, not left to rot. Do not ship a bridge to a framework you are not using.

---

---

## 4. System shape

```
  DEVELOPER MACHINE — never CI
  ─────────────────────────────────────────────────────────────
  Penpot file ──► tokens.source.json ──► tokens.css ──┐
  (PENPOT_API_TOKEN lives only here)                  │ committed
                                                      │
  ══════════════════════════ repo boundary ═══════════╪══════════
                                                      │
  GITHUB ACTIONS                                      │
  Haskell sources ──┐                                 │
  git history ──────┼──► extractors ──► JSON ─────────┤
  chain provider ───┘                    (committed)  │
                                                      ▼
                                            Docusaurus + React build
                                                      │
                                    ┌─────────────────┴─────────────────┐
                                    ▼                                   ▼
                          static site → Pages                 search index (JSON)
                                    │
  BROWSER                           ▼
              search · theme switch · CIP-30 transaction construction
```

**The repository is self-sufficient for styling.** Penpot is a *design-time* source on a
developer machine, not a build input. Actions has no Penpot access and needs none: every CSS
custom property and style rule required to build the Docusaurus and React pages is already
committed. This is the same treatment the generated JSON gets (CON-1) — and it means the design
file's own hygiene problems (THM-7) can never reach a build.

Three compute fields, and nothing else exists:

- **GitHub Actions** — extraction, aggregation, tally, build, deploy. Everything that needs to
  read the chain, parse Haskell, or touch git history happens here.
- **GitHub Pages** — static file serving. No headers, no redirects, no server code.
- **The browser** — search, theme switching, and CIP-30 transaction construction.

---

---

## 5. Environments

| | Developer | Production |
|---|---|---|
| Repo | `lidonation/cardano-db-sync` (fork) | IntersectMBO upstream |
| URL | `lidonation.github.io/cardano-db-sync` | `dbsync.cardano.intersect.org` |
| `baseUrl` | `/cardano-db-sync/` | `/` |
| Cardano network | **preview** | mainnet |
| Survey owner credential | single disposable key | native-script **3-of-5** (§8.6) |
| Indexed by search engines | **no** | yes |

**One codebase, two profiles.** Network and baseUrl are configuration, never hardcoded. Dev
deliberately stays on a `github.io` **subpath** rather than a custom domain: the URL itself signals
"not production" to a human, there is no DNS or certificate to run, no lidonation-owned domain
enters the architecture for upstream to inherit — and, most usefully, **the `baseUrl` divergence is
continuously exercised**. With both environments at `/`, ENV-1 would sit dormant and the first
subpath deployment would break; with dev on a subpath, every dev deploy tests the harder case and
production's `/` is the easy one. The dev site is disposable by virtue of running on **preview**
and on a throwaway URL.

Preview runs **1-day epochs** against mainnet's 5-day. The epoch-boundary refresh (NUM-3) therefore
fires ~30&times;/month on dev and ~6&times;/month on production — still trivial, but the cadence and
the "next boundary in ~2d 14h" copy are both network-derived, never hardcoded.

### 5.1 Rules this forces

- **ENV-1** `baseUrl` is a variable at every use site. The search index fetch, the 404 path
  parser and every asset reference resolve through it. A hardcoded `/cardano-db-sync/` anywhere
  is a build failure.
- **ENV-2** **Wallet network mismatch is a hard block.** The site calls CIP-30 `getNetworkId()`
  and refuses to construct a transaction when it disagrees with the build's network, with a
  named error state. A mainnet wallet on the dev site would otherwise spend real ADA on a
  response referencing preview anchors that nothing will ever tally.
- **ENV-6** The GitHub Pages `CNAME` file is emitted **only for production**, from the build
  profile. Dev publishes on the default `github.io` path and must not carry a `CNAME` — a committed
  one would send it to production's host.
- **ENV-3** Non-production builds serve a **`noindex` meta on every page, and must NOT disallow
  crawling in `robots.txt`.** The two are mutually defeating: Google states that for `noindex` "to
  be effective, the page or resource must not be blocked by a robots.txt file", and that a blocked
  page "can still appear in search results, for example if other pages link to it" because the
  crawler never reads the directive. Blocking therefore produces the exact outcome it appears to
  prevent. `noindex` is also the **strongest control available** — Pages permits no custom headers,
  so there is no `X-Robots-Tag`, no basic auth and no access control. Do not add a `canonical` to
  production alongside it; `noindex` plus `canonical` is a conflicting signal.
- **ENV-4** Non-production builds display a persistent environment banner. It is not in the
  Penpot file; it is authored, and it ships all of FLOOR-5's states like any other component.
- **ENV-5** The site **builds green with no secrets configured.** A fresh upstream clone with no
  provider key must produce a working site with the chain-data surfaces in a named empty state
  (§9.3). A red CI on first clone is the worst possible first impression for a contribution.

---

---

## 6. Content architecture

### 6.1 Two content classes

The design draws 53 boards, and they do not all come from the same place.

- **Reference** — generated, symbol-attached: schema tables and columns, config options, CLI
  flags. Authored as Haddock comments and `-- | @docs:` annotations on the Haskell declarations
  they describe. Never hand-written twice.
- **Guides** — hand-authored **CommonMark (`.md`) with YAML frontmatter** in `docs/`: narrative
  that is not attached to any symbol. Not MDX — see §4.4.
  Installation, troubleshooting, and the eight designed guides.

Pretending everything is an annotation is how this kind of pipeline fails. Of the eight guides
in the design, six are task-shaped reframings of existing `doc/` material (`Restore from a
snapshot` ← `state-snapshot.md`, `Monitoring & rollback recovery` ← `manual-rollbacks.md`,
`Your first SQL queries` ← `interesting-queries.md`, `Trimming with tx_out modes` ←
`configuration.md`); only **Running on Kubernetes** and **Ubuntu systemd service** are net-new.

### 6.2 The pipeline

```
Haskell sources ──► ghc-lib-parser extractor ──► docs/generated/*.json (committed) ──► Docusaurus
```

**Parse, do not compile.** Building cardano-db-sync from source on a GitHub-hosted runner is
hours of compute and a large RAM footprint for a docs build that should take minutes.
`ghc-lib-parser` reads the source and its Haddock comments straight from the AST without
building the package or resolving a single dependency.

- **CON-1** The generated JSON is **committed**. `npm run build` must succeed with no Haskell
  toolchain present. A docs-only PR never pays for GHC.
- **CON-2** Regeneration runs in a **path-filtered** workflow (triggered by changes under
  `cardano-db/`, the config types, or the schema) and **CI fails on drift** between the
  committed artifact and a fresh extraction.
- **CON-3** Titles and descriptions are derived by a remark plugin from the `<h1>` and lead
  paragraph, plus sidebar config — `doc/` files stay plain GFM and render correctly on GitHub.
  This satisfies CONTENT-4 without forcing frontmatter into files upstream reviews.

### 6.3 The five generated surfaces

One pipeline, five consumers. This is the central economy of the build.

| Surface | Generated from | Cost |
|---|---|---|
| Docs reference pages | Haddock + `@docs:` annotations | cheap |
| Schema explorer | `cardano-db` schema types | cheap |
| Config wizard | `Cardano.DbSync.Config.Types` | cheap |
| Updates feed | git history | cheap |
| Numbers · Mainnet | chain provider (Actions cron) | needs a secret (§9) |

The **config wizard** is the strongest case for D2. `Config · 6 Result` emits a real
`config.json` — `ledger_backend`, `insert_options.preset`, `tx_out.value`, `force_tx_in`,
`ledger`, `governance`, `offchain_pool_data`, `disable_epoch` — and then an
"Available data / Not available with this config" matrix mapping options to populated tables.
Annotate the config types once and you get the reference page, the wizard's option set, the
availability matrix, **and** a CI check that every option the wizard can emit actually exists
with those valid values.

- **CON-4** Every config key and value the wizard can produce is asserted, at build time, to
  exist in the extracted config schema. An option the binary would reject is a build failure.
- **CON-5** Every table named in the availability matrix is asserted to exist in the extracted
  schema for that version.

### 6.4 Authoring format

Docusaurus v3 compiles **all** files as MDX by default, including `.md`. The alternative
(`markdown.format: 'detect'`, parsing `.md` as CommonMark) is documented as **experimental with
limitations** — the wrong foundation for an upstream-bound repo.

Measured against the real corpus: with fenced, indented and inline code stripped, `doc/*.md`
contains **zero** bare `{` and **three** JSX-like angle brackets, all on one line of `smash.md`
(`<FILE>`, `<username>`, `<password>`). One line of backticks makes the entire corpus
MDX-compilable. So we compile as MDX and author as CommonMark.

- **CON-8** Authored content is plain **CommonMark in `.md` with YAML frontmatter**. MDX is the
  compiler, never the authoring format. **No JSX in any content file**, enforced by lint so a
  contributor gets a clear message rather than an MDX parse error. Every content file continues
  to render correctly on GitHub.
- **CON-9** `markdown.format` stays at the `mdx` default. CommonMark parsing is not adopted while
  it is experimental.
- **CON-10** Richness comes from **remark plugins that degrade on GitHub and upgrade in
  Docusaurus**: GitHub's native `> [!NOTE]` alerts become admonitions (not `:::note`, which
  GitHub renders as literal text); consecutive labelled code fences become the DF-9 tabbed
  Docker/nix/cabal block, degrading to sequential code blocks on GitHub.
- **CON-11** App-like surfaces — landing, ecosystem, survey, numbers, config wizard — are
  **React pages under `src/pages/`**, not MDX documents. React for routes, CommonMark for content.

### 6.5 Guides declare schema support

A guide references schema and config entities that exist only in some versions. It declares the
**entities it depends on**; the build **derives** the applicable range. A hand-typed version range
rots exactly the way `doc/schema.md` rotted.

```yaml
---
title: Trimming with tx_out modes
description: Cutting database size by pruning spent outputs.
requires:
  tables: [tx_out, tx_in]
  columns: [tx_out.consumed_by_tx_id]
  config: [tx_out.value, tx_out.force_tx_in]
---
```

- **GDE-1** Guides declare `requires` (tables / columns / config), **never** a version range.
- **GDE-2** The build computes the applicable schema range as the **intersection** of the versions
  containing every declared entity. An unknown entity, or an empty intersection, is a **build
  error** — the same discipline as CON-4 and CON-5.
- **GDE-3** A lint warns when the body references a known schema identifier absent from
  `requires`, catching drift in the other direction.
- **GDE-4** An explicit range override exists for guides that deliberately document removed
  behaviour, and requires a stated `reason:`. It may not be used to paper over a stale declaration.
#### Modification, not just existence

Intersection over *existence* catches added and dropped entities. It does not catch a column that
is **modified in place** — and this schema modifies in place often: across the migrations there are
94 `ADD COLUMN`, 33 `DROP COLUMN`, but also **21 `ALTER COLUMN`** (5 type changes, 12 nullability
flips), **8 renames**, and **11 `ALTER DOMAIN … DROP CONSTRAINT`**.

The domain case is decisive. One migration dropped the CHECK constraint from every domain —
`lovelace` went from `numeric(20,0) CHECK (VALUE >= 0 AND VALUE <= 18446744073709551615)` to bare
`numeric(20,0)`. Those domains type **179 of the 580 documented columns**, so 179 column contracts
changed with **zero textual change to any column definition**. `tx_out.value` reads as `lovelace`
before and after. Existence checks see nothing; `doc/schema.md` shows nothing.

- **GDE-6** Entities are tracked by **fingerprint**, not existence. A column's fingerprint is
  `(name, resolved_type, nullable, default)` where `resolved_type` **resolves domains
  transitively** to base type plus CHECK set (`outsum` &rarr; `word128type` &rarr;
  `numeric(20,0) + checks`). Stopping at the domain name is what makes a domain change invisible.
- **GDE-7** A generated **lock** (`docs/generated/guide-locks.json`) records each guide's declared
  entity fingerprints as last reviewed. A changed fingerprint **fails CI with a readable diff**;
  the author edits the prose or re-affirms the lock. Because the lock covers only *declared*
  entities, an unrelated `ADD COLUMN` elsewhere in the table causes no churn.
- **GDE-8** Type changes, domain-constraint changes, nullability flips, renames and drops are all
  **breaking** and require re-affirmation. Nullability is breaking in both directions:
  `SET NOT NULL` falsifies "may be null", `DROP NOT NULL` falsifies "always present".
- **GDE-9** The extractor emits a **rename map** from migration `RENAME TO` / `RENAME COLUMN`
  statements (`pool_offline_data` &rarr; `off_chain_pool_data`, `deposits` &rarr; `deposits_stake`,
  `reward` &rarr; `reward_rest`). Without it a rename reads as remove-plus-add and GDE-2 fails with
  a misleading "unknown entity". Former names also become **search aliases** (SRCH-5), so an
  operator searching the old identifier still lands correctly.
- **GDE-10** Blast-radius changes are reported **as one grouped result**, not per entity. The
  domain migration above would otherwise emit 179 separate failures.
- **GDE-5** One declaration drives all of: the reader-facing badge (*"Schema 0048+ &middot;
  db-sync 13.7.1+"*), the search facet (SRCH-5), exclusion from sidebar and index on lines where
  the guide does not apply, and the **inverse link** — "guides that use this table" on every
  schema explorer page.

### 6.6 Retiring `doc/`

`doc/` is reference-only and is not built. It is retired when parity is *measured*, not asserted.

- **CON-6** A parity report runs in CI: every heading in every `doc/*.md` maps to either an
  annotated counterpart or an authored guide, or is listed as an accepted omission with a
  reason. `doc/` is deleted when the report reaches 100%, in a single PR, together with CON-7.
- **CON-7** Every retired **site route** for `doc/` content resolves, via the redirect manifest
  and stubs (VER-4, VER-5). Bare `doc/*.md` links in the wild are GitHub repository URLs and are
  outside the site's reach (VER-7) — consider leaving one-line pointer stubs in `doc/` through a
  transition period rather than hard-deleting.


#### The `doc/` migration

- **CON-13** Migrating **all of `doc/`** — 23 files, not only `schema.md` — is an explicit
  **phase 1–2 build step**, not background work. `doc/` is the sole source (CON-14). Each file is triaged to one of three
  destinations, and the triage is recorded per file because it determines the phase-1 breakdown:
  **&rarr; Haskell annotations** (reference, extracted per CON-1): `schema.md`, `configuration.md`,
  `command-line-options.md`, and the reference portions of `smash.md` and `pool-offchain-data.md`.
  **&rarr; authored guides** (CommonMark per CON-8): `installing`, `installing-with-nix`,
  `building-running`, `running`, `docker`, `manual-rollbacks`, `state-snapshot`, `migrations`,
  `schema-management`, `upgrading-postgresql`, `validation`, `troubleshooting`,
  `interesting-queries`, `database-encode-decode`, and the operational portions of `smash` and
  `pool-offchain-data`.
  **&rarr; neither**: `release-process` and `hlint-stylish-haskell` are contributor documentation
  (`CONTRIBUTING`), `community-tools` becomes Ecosystem page content, `Readme.md` is superseded by
  site navigation.
- **CON-14** **`doc/` is canonical and authoritative; the Astro copies are deleted outright.**
  `docs/src/content/docs/` was *derived from* `doc/` (snapshotted 2026-05-14) and has been stale
  since; `doc/` has moved on independently (`pool-offchain-data.md` last changed 2026-07-02).
  Lines present only in the Astro tree are therefore content `doc/` has since revised or removed
  upstream — not originals. No reconciliation, no merge: migrate from `doc/`, delete the Astro
  tree. Verified from git history, not from the diff, which on its own looks misleadingly like
  divergence.

#### Who writes the guides

- **CON-12** Guides are **open contribution: anyone may submit one as a PR.** The repository ships
  a guide template carrying the `requires` frontmatter (GDE-1) and the CommonMark rules (CON-8), so
  a contributor is not expected to infer either. Review checks the `requires` declaration and the
  derived schema range (GDE-2) alongside the prose.

  This closes guide authorship. It does **not** close the reference content: seeding ~81 table and
  580 column descriptions as Haskell annotations is a one-time migration out of `doc/schema.md`,
  not an ongoing contribution, and it still needs an owner (§21).

### 6.7 Metadata generation

Every content type emits structured metadata from the **same extraction artifacts** that produce
the page — never hand-typed, and never per-page boilerplate a contributor has to remember. This
extends CONTENT-4 (one `<h1>`, non-empty description) from a minimum to a full metadata contract.

- **META-1** Every page emits `<title>`, `<meta name="description">`, a **self-referential
  canonical**, and Open Graph + Twitter card tags, all derived from the content artifact.
- **META-2** Each content type emits the JSON-LD type that matches it, one per page:
  reference page &rarr; `TechArticle`; guide &rarr; `HowTo`; schema table &rarr; `Dataset` with its
  columns as `variableMeasured`; the FAQ &rarr; `FAQPage`; `Updates` &rarr; `DataFeed`; config
  wizard &rarr; `WebApplication`; the project itself &rarr; `SoftwareSourceCode`. Schema-table
  metadata comes from the same JSON as SCH-1, so it cannot drift from the documented schema.
- **META-3** `BreadcrumbList` on every page, derived from route data — not hand-authored, for the
  same reason NAV-1 derives the active nav item.
- **META-4** `hreflang` alternates for all five locales plus `x-default`, self-referential and
  reciprocal, emitted **only** for pages that actually exist in that locale.
- **META-5** Versioned pages are **self-canonical**. An older documentation version is legitimate
  content, not a duplicate — never canonicalise it to latest, which would deindex the pages an
  operator on 13.5 needs. Non-latest pages instead carry a visible "not the latest version" notice
  linking to current.
- **META-6** `sitemap.xml` is generated with `hreflang` alternates and `lastmod` from git. VER-4
  redirect stubs are **excluded** — they exist for callers, not for indexing.
- **META-7** The `Updates` surface emits **RSS and Atom** feeds, generated from the same git history
  as the on-page list.
- **META-8** Preview builds emit `noindex` (ENV-3) and **must not** emit a canonical pointing at
  production, `hreflang`, or a sitemap. Those are conflicting signals: a `noindex` page advertising
  itself as an indexable alternate is exactly the ambiguity ENV-3 exists to avoid.
- **META-9** Social preview images are generated at build **per content type**, not per page, from
  the design tokens — keeping PERF-1's asset budget honest. No text is baked into an image that is
  not also present as text on the page (1.4.5 Images of Text).
- **META-10** JSON-LD is **validated in CI**; a page whose structured data fails validation fails
  the build. Metadata that is silently wrong is worse than absent.

---

---

## 7. Schema explorer

`doc/schema.md` is 63.8 KB of hand-maintained markdown covering **81 tables and 580 documented
columns**, and its own first line admits the generator was removed. It is replaced by a
generated explorer: table index, per-table pages, per-column anchors, filter-as-you-type.

- **SCH-1** `doc/schema.md` is not the source. The schema JSON is extracted from the Haskell
  schema types, and the markdown page is deleted when CON-6 completes.
- **SCH-2** `ERD.png` (1.88 MB, currently unreferenced) is deleted. The ERD is generated as
  themed SVG from the same JSON — which makes it theme-aware and translatable, neither of which
  a raster file can be. This closes CONTENT-1.
- **SCH-3** The explorer's in-page filter and the global ⌘K search share **one** entity index
  (§7). Two consumers, one artifact, no divergence.
- **SCH-5** Every column carries a **change history** derived from the same fingerprints —
  *"nullable since 0048"*, *"CHECK constraint dropped in 00NN"*. It is free from GDE-6 and is the
  single most useful thing on the page for the practitioner audience.
- **SCH-4** Every table page states the schema version it describes, as `(stage_two, stage_four)`
  — the exact tuple an operator reads from `SELECT * FROM schema_version`.

---

---

## 8. Versioning and URL structure

### 8.1 Why two axes

GitHub Pages caps a published site at **1 GB** and times out a deployment at **10 minutes**.
The repo has **115 release tags**. At roughly 5–8 MB per version-locale (~130 pages), patch-level
versioning across three locales lands near 2.2 GB — over the ceiling, and the deploy timeout
bites first.

"Last 5 tags" is not the fix. The five most recent tags are `13.7.2.1`, `13.7.2.0`, `13.7.1.0`,
`13.7.0.5`, `13.7.0.4` — spanning **2026-04-28 to 2026-06-17, seven weeks**, all within 13.7. An
operator on 13.6.0.8 (released April 2026) would get nothing.

**Five minor lines** — 13.7, 13.6, 13.5, 13.4, 13.3 — covers **July 2024 to June 2026** for the
same storage budget (~100 MB).

### 8.2 The window rule

- **VER-1** Retain the **last 5 minor lines**, not the last 5 tags and not a date window.
  Release lines are interleaved: `13.6.0.8` (2026-04-04) shipped *after* `13.7.0.0` (2026-01-31),
  so any chronological rule would evict the newest release of a still-maintained line.
- **VER-2** Prose docs are versioned per minor line. Only the **current** line is actively
  translated; older lines freeze at whatever they were translated to, so i18n cost scales with
  release churn rather than with window size.

### 8.3 The schema axis

Schema migrations land at *patch* cadence inside a line, so "13.7" describes at least three
different databases. The schema axis is therefore independent and finer — and nearly free,
because the whole schema entity payload gzips to **7.5 KB**.

- **VER-3** Schema versions are derived **from the tag itself** — `git ls-tree <tag> schema/` —
  never from migration filename dates. Filenames are not date-ordered (`migration-2-0045` is
  dated *after* `migration-2-0046`), and a date-derived mapping produces wrong answers.

Derived from the tags, for the retained window:

| tag | stage_two | stage_four |
|---|---|---|
| 13.5.0.2 | 0043 | 0008 |
| 13.6.0.8 | 0044 | 0008 |
| 13.7.0.0 | 0045 | 0008 |
| 13.7.1.0 | 0048 | 0009 |
| 13.7.2.0 | 0050 | 0009 |
| 13.7.2.1 | 0050 | 0009 |

`13.7.2.0` and `13.7.2.1` share a schema — the axis collapses patch releases exactly as intended.
Roughly six distinct schema versions cover five prose lines.

### 8.4 Retirement and the path-aware 404

A rolling window kills URLs, and Pages offers no server-side redirects. Generating a stub per
retired path does not scale — the window rolls forever.

GitHub Pages cannot issue an HTTP 301. But Google's redirect guidance states that **instant
`meta refresh` redirects are interpreted as permanent redirects**, and names them the recommended
fallback where server-side redirects are impossible. That signal only survives from a page
returning **HTTP 200** — served from `404.html` the 404 status dominates and the signal is lost.
So retirement is handled in two tiers, not one.

- **VER-4** **Enumerable retired paths get a generated stub file at the old path**: HTTP 200,
  instant `meta refresh` (zero delay), and `rel="canonical"` to the target. This is what gives
  search engines and callers the permanent-redirect signal. Generated via
  `@docusaurus/plugin-client-redirects`. No JavaScript redirects — rendering can fail, and Google
  advises against them.
- **VER-5** The redirect map is a **committed, append-only manifest** of every path ever
  published and its current best target. A build knows only the 5 live lines and cannot enumerate
  the pages of a version retired years ago, so without a persisted manifest retirement is silently
  lossy. It is diffable and PR-reviewed, and it grows monotonically. Sizing is not a concern:
  ~130 pages &times; 3 locales &times; ~500 bytes &asymp; **200 KB per retired version**, so
  1&ndash;2 retirements a year is a few MB per decade against a 1 GB budget.
- **VER-6** **Unenumerable paths** — arbitrary deep links, typos, paths that never existed — fall
  to a single generated `404.html`, which returns a genuine **404**, carries `noindex` so retired
  paths are not indexed as soft 404s, and offers the nearest surviving equivalent plus a link to
  the archived docs at the git tag **as real links**, never an automatic redirect. It must resolve
  `baseUrl` correctly under both profiles (ENV-1).
- **VER-7** Scope: the manifest covers **site routes** (the old Astro site's paths and retired
  version paths). Links in the wild to `doc/*.md` are *GitHub repository* URLs and cannot be
  intercepted by the site at all; CON-7 is satisfied for site paths only.

---

---

## 9. Search

### 9.1 Why the entity layer is purpose-built

The design specifies **entity search, not prose search**. A result row reads:

```
// tables
  tx_out       core · 12 columns · indexed        ↵
// guides
  Running on Kubernetes    guide · advanced       ↵
```

`core · 12 columns · indexed` is not a text snippet — it is a structured record with facets, and
that fact does not exist in any prose. It exists in the Haskell schema type. Both candidates in
PRODUCT.md (`@easyops-cn/docusaurus-search-local`, Algolia DocSearch) index rendered page prose
and return page-or-heading hits with snippets. Either would mean abandoning the designed result
row. Algolia additionally fails the upstream-neutrality constraint.

We already build the pipeline that produces exactly these records. The index is one more
artifact off it, and the facets come free.

This is a statement about the **entity layer only**. Prose search is fully achievable under D6/D7
and ships as a second layer (§7.2). The two are not alternatives.

### 9.2 Two tiers

Measured against the real schema:

| Payload | Gzipped |
|---|---|
| All 302 unique identifiers | 1.5 KB |
| Identifiers + descriptions | 7.5 KB |
| Full prose index (est.) | 60–100 KB |

- **Tier 1 — entity index.** Tables, columns, guides, doc titles, config options, CLI commands.
  Names, facets, URLs. Bundled and always available; ~25–40 KB gzipped including the inverted
  index. This alone serves every result row the design draws.
- **Tier 2 — prose, via Pagefind.** Pagefind post-processes the built HTML in the build step and
  emits a **chunked** index, so the browser fetches only the shards a query needs rather than a
  monolith. That is what makes prose search affordable across 5 lines &times; N locales. Driven
  through its **headless API** (`pagefind.search()`), so the designed ⌘K UI is kept rather than
  inheriting a plugin's modal. Docs pages never pay for it unless someone searches prose, which
  satisfies PERF-4.

The two layers merge into one result list: facets where they exist, excerpts where they don't.

### 9.3 Rules

- **SRCH-1** Engine is MiniSearch with a **custom tokenizer**: identifiers index both whole and
  split on `_`, so `tx` finds `tx_out` while exact `tx_out` still outranks `tx_in`. Stemming is
  **off** for identifier fields — a stemmer collapses `prune` and `pruned`, which is right for
  prose and wrong for a config value.
- **SRCH-2** Ranking is explicit: exact identifier → prefix on identifier → title → body. Group
  order in the UI is fixed (tables, guides, docs); within a group, by score. Each group is capped
  with an explicit "show all". The design's own example — `tx_out` returning all eight guides,
  including `Ubuntu systemd service` — is an illustration, not a relevance model.
- **SRCH-3** The index is per `(line, locale)`, fetched from a **content-hashed filename**.
  Pages gives no control over `Cache-Control`; the filename is the only cache-busting mechanism.
- **SRCH-4** Search is **scoped to the version being read**, with an explicit opt-in to widen.
  An operator on 13.6 receiving 13.7 answers is a support burden.
- **SRCH-5** Facets are stored as **data, never rendered strings** — `{group:"core", columns:12,
  indexed:true}`, rendered through i18n at display time. Baking "core · 12 columns · indexed"
  into the index forces a full copy per locale.
- **SRCH-6** The combobox uses **`aria-activedescendant`, not roving focus** — focus stays in the
  input so typing continues while ↑↓ moves selection. `role="combobox"` + `aria-expanded` +
  `aria-controls` on the input; `role="listbox"` on the list; `role="option"` + `aria-selected`
  on rows; group headers `role="group"` + `aria-labelledby`; result count in a polite live
  region. Overlay traps focus, locks scroll, and restores focus to the trigger on Esc.
- **SRCH-7** `⌘K` binds and **renders** as `Ctrl+K` off macOS. A `/` shortcut, if added, is
  suppressed while focus is in any text input.
- **SRCH-8** A static browse fallback exists (schema index, guides index) so content is reachable
  without JavaScript.
- **SRCH-10** Pagefind is driven headlessly. Per-version bundles are selected with
  `options({bundlePath})` plus `destroy()`/`init()`, which is the mechanism behind SRCH-4.
  Heading-level **sub-results** are used so prose hits deep-link into a section, not just a page.
- **SRCH-11** **CLOSED 2026-08-23 — tested, not assumed.** Pagefind 1.5.2 indexes a compound
  identifier as **the whole token *and* its underscore-separated parts**: `tx_out` &rarr;
  `tx_out`/`tx`/`out`; `consumed_by_tx_id` &rarr; whole + `consumed`/`by`/`tx`/`id`; `ma_tx_out`
  &rarr; whole + `ma`/`tx`/`out`. Parts below a minimum length are dropped (`a_b_c_d` yields only
  the whole token), which does not affect real schema identifiers. `pagefind-entry.json` declares
  `"include_characters":["_", …]`, so underscore is a word character by design. **Query `tx`
  therefore reaches `tx_out` and `tx_in`** — the behaviour `Search · typing` draws — and Pagefind
  may carry identifier fields. This removes the tokenization risk; it does **not** remove the
  entity layer, which exists for the facets (`core · 12 columns · indexed`) no prose indexer can
  produce, and for ranking control (SRCH-2).

- **SRCH-9** Six states ship, not the four drawn: pre-query, typing, results, **zero results**,
  **error** (index fetch failed), **loading** (Tier 2 in flight). The `// no recent searches`
  empty state implies a populated variant and localStorage persistence — both are authored, with
  a clear-history control.

**Formerly-blocking token defect, CLOSED 2026-08-23:** the search field's placeholder sat on
`surface.sunken`, where `text.placeholder` `#6e7781` measured **4.05:1**. Lifting the token was
arithmetic-impossible without inverting the text ladder (`text.tertiary` passes sunken at just
4.54, so any lighter value fails), so the field moved instead: all four themes' input plates now
sit on `surface.base` (light `#ffffff` → 4.53:1), cream relinked to the `warm-raised` library
colour. Verified at revn 1337.

---

---

## 10. Survey — CIP-179

### 10.1 Why on-chain

Under D6/D7 there is no backend to receive a form. CIP-179 (metadata label `17`, spec version 5)
turns the write path into a wallet transaction and the read path into an indexing problem — and
the indexer is the software this site documents. The CIP's own "Block Explorer and dApp
Implementation Guide" is a 13-step algorithm over `tx_metadata`, `drep_registration`,
`pool_update`, `committee_registration` and `delegation`: every table is already in the schema
documented here.

### 10.2 Write path — browser only

Browser builds the label-17 payload, CIP-30 wallet signs, wallet submits. Nothing is hosted,
nothing is inherited by upstream. Subject to ENV-2.

### 10.3 Read path — Actions cron

- **SUR-1** A scheduled workflow runs the 13-step tally against a chain provider and commits
  `aggregate.json`. Tallies are deterministic by construction, so the result is reproducible and
  independently auditable. The site stays fully static.
- **SUR-2** The tally job takes **network** as an input. Preview and mainnet response sets are
  never merged, and a production build never renders preview data.
- **SUR-3** The canonical `survey_ref` values (the core under §8.8, plus every supplementary and
  historical one) are recorded in a **reviewed file in the repository**, each with its `end_epoch`
  — so the form composer drops expired surveys instead of collecting invalid responses — and its
  subsumption edges (SUR-26). Because `owner` is bound per definition and may legitimately change across the
  series (§8.6), the repo — PR-reviewed and diffable — is the authority record, not a credential.

### 10.4 Question mapping

| Survey question | CIP-179 type |
|---|---|
| use cases | multi-select (2) |
| team size | single-choice (1) |
| frequency | numeric-range (4) |
| pain points | ranking (3) |
| satisfaction | rating (6) |
| improvement wish | **custom (0)** — see below |
| schema `stage_two` | numeric-range (4), `[0, 9999]`, optional (SUR-25) |
| schema `stage_four` | numeric-range (4), `[0, 9999]`, optional (SUR-25) |

`role` in CIP-179 is *ledger-validated identity* (DRep / SPO / CC / Stakeholder / Keyholder), not
occupation. The design's "Who answered" breakdown (Developers / DevOps-SRE / Tech leads) is a
**question**, not the CIP-179 `role` field. Both exist; they are not the same field.

- **SUR-4** Free text ships as a **custom (tag 0)** question. Its `content_anchor` names a method
  schema *we* author and publish as a static file; the respondent's answer is chunked text inline
  in their own transaction. No respondent-side hosting, no upload target, no IPFS.
- **SUR-5** Presentation uses **external-content mode**: on-chain holds structure, constraints and
  option counts; the site hosts the hash-anchored English presentation JSON; translated labels are
  supplied by Docusaurus i18n keyed by question and option **index**. Indices are the interlingua
  — the same principle as SRCH-5.
- **SUR-6** The ranking question ships a keyboard and single-pointer path (up/down controls or a
  numbered select per row) with a live-region announcement. Drag is an enhancement only.
  Drag-only is WCAG 2.2 SC 2.5.7 failure (DF-5).

### 10.5 Moderation

Free text on-chain is permanent, public and unremovable. The dashboard is generated at build
time, which means **the build is the moderation point**: we cannot remove anything from chain,
but we fully control what the site renders.

- **SUR-7** Rendering of free-text responses passes through an explicit review gate in the
  generation step. It is designed in from the start, not retrofitted after an incident.

### 10.6 Ownership

CIP-179 accepts `[0, addr_keyhash]` or `[1, script_hash]` and explicitly rules out Plutus owners
(metadata carries no redeemer tag, so a Plutus credential can never prove itself). Native scripts
give `atLeast M of N` directly.

The keys are needed for **exactly two operations**: publishing a definition, and cancelling one.
Responses are signed by respondents, tallying is a pure read, and the docs build touches nothing.
Under the standing-survey model (§8.8) that is **one signing ceremony**, plus cancellation only if
the instrument turns out broken.

- **SUR-8** Mainnet ownership is a native-script **`atLeast 3 of 5`**. The threat model is not
  governance etiquette: cancelling a survey means *"tooling MUST NOT tally any of its
  responses"*, so a single compromised key can irreversibly void a year of collected data.
- **SUR-9** **No signing key ever enters a GitHub Actions secret.** The provider API key (§9) is
  categorically different from the survey owner key; the latter is cold and offline.
- **SUR-10** The ownership script carries **no time locks**. Native scripts support `before`/
  `after` bounds; a time-locked owner can be unable to cancel exactly when it must.
- **SUR-11** The native script is included in the witness set of the definition transaction, so
  it is both proven and published — verifiers resolve it by chain indexing.
- **SUR-12** Handover requires no key ceremony. `owner` binds per definition, so the next survey
  in the series simply uses an Intersect-controlled script. Old surveys keep their old owner and
  their cancellation right decays to nothing at `end_epoch`.

### 10.7 Lifecycle: one standing survey, not a series

CIP-179's whole lifecycle is three operations — define (0), respond (1), cancel (2). **There is no
amend, extend or update**, and a definition is immutable. `end_epoch` is a bare `uint` whose only
constraint is that it exceed the current epoch at inclusion; there is **no upper bound**. A
long-running survey is therefore permitted.

It also fits PRODUCT.md better than a per-release series, which had the requirement backwards:

- **Usage measurement** asks for data "aggregate-honest and **comparable over time**". An
  instrument that changes its questions cannot produce comparable readings. Freezing is the
  requirement, not the cost.
- **Continuous feedback** asks that the survey be "**re-submittable over time**". CIP-179 delivers
  this natively through latest-valid-response-wins per `(survey_ref, role, credential)` — which the
  spec describes as giving "respondents a correction path". A series **breaks** it: each new
  `survey_ref` is a fresh identity tuple, so no prior answer ever carries over and nobody updates
  anything; they merely fill in another form.

- **SUR-15** A **core standing survey** carries the durable measurement questions, with an
  effectively open-ended horizon: `end_epoch` is **mandatory** in CIP-179 and has no null and no
  extend, so "no end date" is expressed as **epoch 2000 (&asymp; 2045)**. This is a **one-way
  commitment** — `end_epoch` can never be shortened. The core is never amended; a v2 arrives as a
  new definition that subsumes it (SUR-21, SUR-26) rather than requiring the core to expire.
- **SUR-30** Because tally-time re-verification at `end_epoch` will not arrive in practice, **every
  published figure is an interim tally permanently**, not temporarily. SUR-19's labelling is a
  standing state of the dashboard, not a transitional one.
- **SUR-31** Cancellation stays available for ~19 years, so the 3-of-5 holds that power across
  decades of key custody. Key **loss** is survivable — the survey simply continues and a v2
  supersedes it (SUR-26); only deliberate retraction becomes impossible.
- **SUR-16** Within any one survey, everything is frozen: question count, type tags, option counts,
  constraints, `eligible_roles`, `submission_mode`, and presentation (the anchor's
  `blake2b-256` is tamper-evidence, so the document cannot be re-authored). The **core**'s
  questions must therefore be evolution-proof: no version-specific options, no named-feature lists
  that date. Anything time-specific goes to a supplementary survey (SUR-21) or the free-text custom
  question (SUR-4).

#### Evolution is additive, not an amendment

The absence of an amend operation does **not** freeze the programme — only each definition. Three
mechanics make growth cheap:

*"Multiple responses MAY be batched in one transaction; each is validated independently"*, and each
response carries **its own `survey_ref`** (key 1) — so one transaction answers several surveys.
**`credential` (key 3)** is on every response, so a respondent is identifiable *across* surveys even
though latest-wins dedup is per-survey. And **cancellation is per-survey**.

- **SUR-21** New questions ship as a **new, concurrent survey definition**, never by amending the
  core. The core keeps running untouched.
- **SUR-22** The client composes **all active surveys into one form** and submits **one
  transaction** with batched responses. One wallet prompt, one fee, regardless of how many
  definitions are live.
- **SUR-23** `credential` is the **cross-survey join key**. The dashboard joins on it so a
  respondent's answers across definitions resolve to one person; respondent continuity survives
  the addition of new surveys.
- **SUR-24** Because cancellation is per-survey, a supplementary survey can be retired without
  touching the core. This bounds SUR-20's blast radius to the core alone — which is why only the
  core needs the conservative threshold.
#### Subsumption

As supplementary surveys accumulate, a newer one will eventually ask a better version of an older
question. That relationship has to be expressible — but the two directions are not equally
possible. A definition is immutable, so a **new** survey can declare what it subsumes, while an
**old** one can never point forward to a successor that did not exist when it was published.

- **SUR-26** Subsumption is declared **forward only** (new &rarr; old) and at **question
  granularity**, never whole-survey — a successor commonly supersedes one question while the rest
  of the older survey stays live. It is carried in two channels: the reviewed repo manifest
  (SUR-3), which is **authoritative for our build**, and the anchored presentation document, whose
  JSON profile states that tools "SHOULD ignore unrecognized fields" — so a `subsumes` field needs
  no CIP extension and no private metadata key. The second channel exists for third-party chain
  readers, who have no repo access; if an anchor is unavailable they lose the signal, we do not.
- **SUR-27** The reverse direction — "superseded by" — is a **derived index**, built by scanning
  all definitions. This mirrors CIP-179's own governance linkage, which is one-directional on-chain
  ("the action references the survey, not vice versa, avoiding circular dependencies") with the
  reverse built by tooling. No backward pointer is needed, and none is possible.
- **SUR-28** Every subsumption carries a **kind**, because merging can fabricate a trend:
  **`replaces`** — semantically equivalent, so a newer answer supersedes the older for the same
  credential, falling back to the older where no newer answer exists; **`supersedes`** — a
  different question, **never merged**, with the older closed and reported separately. Any
  cross-survey merge is our editorial view, not a CIP-179 tally, and is labelled as such
  (SUR-18, Tool Output Requirements).
- **SUR-29** The form composer (SUR-22) does not present a question that a live question subsumes.
  Supplementary surveys take **short horizons** so that subsumption resolves by expiry wherever
  possible, rather than needing the graph at all.

- **SUR-32** `eligible_roles` (definition key 4) is the set of **ledger-verified** identities
  permitted to respond — DRep (0), SPO (1), CC (2), Stakeholder (3), Keyholder (4) — and is frozen
  at signing (SUR-16). It is set to **`[Keyholder]`**: everyone with a payment credential qualifies,
  which is every wallet holder. Two reasons. SPO verification requires the pool's **cold**
  credential, which operators keep offline — CIP-151 calidus is flagged in the CIP as a future fix
  and is not yet specified. And `role` is part of the dedup tuple `(survey_ref, role, credential)`,
  with the spec permitting "separate responses for different roles" — so each extra admitted role
  is another double-count path, against the aggregate-honesty requirement. Stake-pool operation is
  captured as an ordinary **question** instead, the same move that kept job role out of `role`
  (§8.4).
- **SUR-25** **Schema version is captured as a numeric-range question**, not an option list:
  `[4, prompt, [0, 9999]]` over `stage_two`, and the same for `stage_four`. Integers never rot as
  new migrations land, and they are exactly what `SELECT stage_two FROM schema_version` returns —
  so the survey gives the operator the SQL rather than asking them to guess a version name. Both
  are optional (abstain costs zero bytes). This makes every answer sliceable by schema version,
  which tracks what respondents actually run rather than when they happened to answer.
- **SUR-17** The question set is **piloted on preview** (D10) before the mainnet definition is
  signed. With no amend path, preview is the only rehearsal that exists.
- **SUR-18** The time series is **derived, not structural**. Canonical totals are latest-wins per
  credential; the chain retains every response with its slot, so the dashboard additionally renders
  a historical view computed from response epochs, **labelled as derived** and not presented as a
  CIP-179 tally (Tool Output Requirements).
- **SUR-19** Interim tallies use **response-time validation only**; tally-time re-verification at
  `end_epoch` is the canonical snapshot. The dashboard states which of the two it is showing.
- **SUR-20** The core survey holds all durable data, and cancelling it voids **every response it
  ever collected**. SUR-8's `atLeast M of N` is set conservatively on that basis. Supplementary
  surveys (SUR-21) carry less and are independently disposable.

### 10.8 Copy corrections required in the design

- **SUR-13** `Survey · Overview` currently promises **"Anonymous · no email or account
  required."** Wallet-gated means **pseudonymous and permanently public** — every answer is
  on-chain forever, attributable to a credential, and cannot be deleted. This must be stated at
  the point of submission, not buried. It also settles PRODUCT.md's open "survey data retention /
  privacy" item outright: retention is permanent, exposure is total.
- **SUR-14** "Eight questions, about three minutes" contradicts the seven question boards drawn.
  Reconcile the copy with the built survey.

---

---

## 11. Chain data and the storage model

### 11.1 What a public provider can answer

`Numbers · Mainnet` says "live from a mainnet db-sync instance, refreshed each epoch boundary."
With a public provider (Koios/Blockfrost) as the source, the board splits:

- **Available** — block height, current epoch, transaction count, blocks this epoch, per-epoch
  block and transaction history.
- **Not available** — database size, largest tables, per-epoch database growth, rollbacks in 7d,
  instance health / synced-to-tip. These are properties of a Postgres instance, not of the chain.
  "Synced 100.00%" is meaningless from a public API.

### 11.2 One storage model, two consumers

Table sizes are a function of chain volume and of which tables a config populates — which is
exactly what `Config · 6 Result` must already compute.

- **NUM-1** A single storage model feeds both the config wizard's cost panel and the Numbers
  storage section. Both are **labelled as modelled**, with the basis stated. When a self-hosted
  instance appears later, the model is replaced by measurement in one place.
- **NUM-2** The design's `PLACEHOLDER FIGURES` / `STUB` markers are replaced by estimates with a
  visible, stated methodology. Nothing ships presented as measured that was not measured.
- **NUM-3** The board title and copy reflect the build's network. On a preview build it is not
  "Mainnet", and preview epochs are 1 day rather than 5 — the "next boundary in ~2d 14h" copy is
  network-derived, not hardcoded.

### 11.3 Operational

- **NUM-4** The refresh workflow is `schedule`-triggered **on the default branch only**. Fork PRs
  cannot read Actions secrets, so no PR-triggered path may depend on the provider key.
- **NUM-5** With no provider key configured, chain surfaces render a named empty state and the
  build stays green (ENV-5). At one run per epoch boundary the job makes ~6 calls a month —
  inside any free tier.

---

---

## 12. Build and deploy

| Workflow | Trigger | Does |
|---|---|---|
| `extract` | push touching `cardano-db/`, config types, `schema/` | regenerates JSON artifacts; fails on drift |
| `chain-refresh` | `schedule`, default branch only | queries provider, commits chain + tally JSON |
| `build-deploy` | push to default branch, and after the above | Docusaurus build → Pages |
| `verify` | every PR | token lint, axe, content lint, link check, asset budget |

- **BLD-1** The published site stays under **1 GB** and the deploy under **10 minutes**. Both are
  asserted in CI, not hoped for.
- **BLD-2** `verify` enforces the quality floor (§§13–19) mechanically — axe across four
  palettes on landing, a docs page, the schema explorer, search, and every survey step; CONTENT-4
  title/description; PERF-1 unreferenced-asset check.
- **BLD-3** No unreferenced asset ships (PERF-1). `ERD.png` is deleted, not merely unlinked.
- **BLD-4** **Visual regression** runs on every PR with Playwright — MIT, runs in Actions, no
  third-party service for upstream to accept. The matrix is bounded to ~8 representative pages
  (landing, a docs page, a schema table, the search overlay open, one survey step, one config
  wizard step, the numbers dashboard, the 404) × **4 palettes** × **3 breakpoints**, ≈96 shots.
  Determinism is the hard part and is engineered for: a **pinned container image** (font rendering
  differs across host OS and is the primary flake source), self-hosted fonts so there is no network
  variance, `prefers-reduced-motion: reduce` forced, and animation disabled. Baselines are committed
  to the repository — they are not part of the published site, so the 1 GB Pages limit does not
  apply, but the matrix stays bounded regardless. This catches the class axe cannot: a palette that
  renders unreadable while remaining semantically valid.

### 12.1 Supply-chain and delivery security

The survey route constructs a **financial transaction and hands it to a wallet for signing**. That
makes it the highest-consequence surface in the build, and it is the one the intuitive control does
not protect.

- **SEC-1** **CSP is `<meta http-equiv>` only.** Pages sets no headers. Accept the consequences
  explicitly rather than discovering them: `frame-ancestors`, `sandbox` and `report-uri`/`report-to`
  are ignored in meta form, and **report-only cannot be delivered by meta at all**. There is
  therefore **no clickjacking protection available on this host** — a known, accepted limitation,
  not an oversight to engineer around.
- **SEC-2** **All assets are self-hosted, fonts included.** This tightens the policy to
  `default-src 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:; font-src 'self';
  connect-src 'self'; object-src 'none'; base-uri 'none'`, removes a third-party dependency, and
  avoids the German GDPR exposure around Google Fonts — material given **DE is a launch locale**.
- **SEC-3** Inline script is eliminated except Docusaurus's colour-mode bootstrap, which is
  **hashed at build**. Nonces require server-side generation and are impossible on static hosting.
- **SEC-4** **SRI is the wrong control here, and is not the mitigation.** Subresource Integrity
  protects against a compromised third-party CDN. Under SEC-2 there is no CDN and every asset is
  same-origin, so SRI adds nothing. The real threat is a **compromised npm dependency baked into
  the bundle at build time**, which SRI cannot detect at all.
- **SEC-5** The actual controls are build-time: a committed lockfile with `npm ci` (lockfile
  integrity hashes), pinned versions, an `npm audit` gate in CI, and automated dependency PRs that
  require human review rather than auto-merge.
- **SEC-6** **The survey route runs a minimal, separately-reviewed dependency set.** Any dependency
  participating in CBOR encoding, transaction construction or wallet interaction is pinned, and its
  diffs are reviewed by a human on every update. A transitive dependency reaching that route is a
  review failure, not a routine bump.
- **SEC-7** **Narrowed 2026-08-23 — still needs a real browser.** The question is not "does a
  strict CSP break CIP-30" but **which injection method each target wallet uses**, because the two
  behave differently: a content script that appends `<script src="chrome-extension://…">` to the
  page performs a **page-context script load and is gated by `script-src`** (so `'self'` blocks
  it), whereas `chrome.scripting.executeScript` with `world: 'MAIN'` is injected by the browser
  rather than through a DOM element and is not gated. Most current wallets should use the second
  path under MV3, but that is inference from mechanism, not a cited guarantee — neither Chrome's
  `chrome.scripting` documentation nor the CIP-30 specification states the CSP interaction. Test
  each wallet the survey supports: load the survey route under the production CSP and confirm
  `window.cardano` is present. Minutes per wallet.
- **SEC-8** **CSP is scoped per route, so SEC-7's outcome cannot weaken the rest of the site.** Docs,
  schema, search and every static page keep the strictest policy (SEC-2) — none of them needs a
  wallet. Only the **survey route** may carry a relaxed policy, and if a wallet turns out to need
  it, the relaxation is a targeted allowance for the extension scheme
  (`script-src 'self' chrome-extension: moz-extension:`) on that route alone — never a blanket
  loosening. This is deliberately decided **before** the spike runs, so the test's result changes
  one route's policy rather than the site's security posture.

---

---

## 13. The contrast and control floor

- **FLOOR-1** Body and placeholder text ≥ **4.5:1** against the surface it actually sits on —
  measured per theme, per surface, not against white. The full cross-product (`surface.base`,
  `surface.sunken`, `surface.accent-subtle`, `surface.warm`, `code.surface`) is the test set.
- **FLOOR-2** Large text (≥ 24px, or ≥ 19px bold) ≥ **3:1**. A token may be licensed for large
  text only if that licence is written next to it; nothing is licensed by default.
- **FLOOR-3** Controls, icons and focus indicators ≥ **3:1**, **including a control's own
  boundary against the card it sits on**. A teal checkbox on a white card at 2.24:1 is a fail even
  though its glyph passes — the precedent fix is a 1px `border.selected` inner stroke (5.13:1).
- **FLOOR-4** Every interactive element has real `:focus-visible` styling drawn from
  `--dbs-focus-ring` / `--dbs-focus-offset`. Tab through every page; the ring is visible on every
  stop, in every theme, and is never removed by an `outline: none` anywhere in the codebase.
- **FLOOR-5** Every interactive component ships **hover, disabled, loading, error and empty**
  states. A component with only a default state is incomplete, not "minimal".
- **FLOOR-6** No information is carried by colour alone: every status, selection and validation
  signal has a glyph, a word or a shape alongside its colour. (The survey's `✓` and the
  admonition kicker words already satisfy this — keep them.)
- **FLOOR-7** Browser surfaces are themed from the palette: selection, caret, scrollbars, focus
  ring, underline offset, tabular numerals. Defaults belong to no design system.
- **FLOOR-8** No coloured `border-left`/`border-right` above 1px on cards, callouts or list items;
  no gradient text; no emoji or unicode glyph standing in for an icon; no monospace as a costume
  (mono is for code, chain data and measurement only — that rule the design already makes).

---

## 14. Tokens and theming

### 14.1 Token criteria

- **TOK-1** `tokens.css` is imported once, at the root layout. `grep -c 'var(--dbs-'` in the
  application stylesheets is > 0 and **no second palette exists**: no CSS custom property
  defining a colour outside `tokens.css`, and no raw hex in any component file.
- **TOK-2** All four themes are reachable at runtime via `data-theme` on `<html>`, with a working
  switch control and a persisted choice. Cream and navy must have a code path, not just a design.
- **TOK-3** **CLOSED 2026-08-23.** `--dbs-space-*` and `--dbs-radius-*` resolve to lengths with units. Test: computed
  `border-radius` of a card is 8px, not 0. Fix in `tokens.source.json` (store `"8px"`) or teach
  the generator to append `px` to the `space`/`radius` groups; then confirm `tokens.css`
  regenerates byte-identically.
- **TOK-4** No component binds body text to `brand.intersect-navy` (or any `brand.*` value).
  Badge and warm-card labels resolve ≥ 4.5:1 in all four themes — today they are 1.08 and 1.11 in
  dark, 1.26 and 1.11 in navy.
- **TOK-5** **Naming rule, mechanically checkable:** only `text.*`, `code.text*` and `*-text`
  tokens may appear in a text-colour position. Add the missing themed role rather than reaching
  for a fill token — the model is `text.on-selected`, added for the survey checkbox and themed
  four ways. A `text.on-tint` role is the likely answer for TOK-4.
- **TOK-6** `border.default` is split into a decorative role (may stay hairline) and a control role
  that clears 3:1 on its own surface in every theme. **No new colour is required:** the existing
  `border.interactive` already clears it in all four themes — light 3.60/3.21, dark 4.15/3.80,
  cream 3.90/3.24, navy 6.78/6.69 on `surface.base`/`surface.sunken`. It **is** the control role.
  What remains is the binding: inputs, search, choice cards and cards use `border.interactive`,
  never `border.default`. **Wider than first recorded:** `border.default` is not the only offender.
  Measured against the surfaces they are specified on, **`border.strong` also fails 3:1 in all four
  themes** (light 1.45/1.30, dark 1.55/1.42, cream 1.50/1.24, navy 1.88/1.85), as does
  **`border.warm`** on `surface.warm` (1.20–1.62) and **`border.warm-strong`** (1.40). Two tokens
  named as if one were the stronger boundary are both below the control threshold. All of them may
  remain as decorative hairlines; none may bound a control.
- **TOK-7** Every muted text token clears 4.5:1 on every surface it is specified on.
  One case was ever real, and it is closed: `text.placeholder` `#6e7781` measured 4.05 on light
  `surface.sunken`; the field moved to `surface.base` (4.53). *(The second case originally listed
  here — `text.tertiary` at 4.07 on cream sunken — could never occur: cream overrides that token
  to `#6e6656`, which measures 4.55. The 4.07 figure belongs to the light value.)*
  **CLOSED 2026-08-23.** `text.warm-tertiary` measured 3.08 on `surface.warm` in light and cream
  (4.45 dark). Rebound to existing system values — base `#6e6656`, dark `#8b949e`, navy `#8fa3d9`
  — with **no new colour invented**: light 5.11, dark 4.96, cream 5.11, navy 7.56. Note
  `surface.warm` is `#0e2a25` in dark and `#010e33` in navy, so those themes needed their own
  muted value rather than the warm one.
- **TOK-8** **DONE 2026-08-23.** Resolved via the first option: `accent.secondary-deep` now holds
  `#06584a` and `-strong` is retired. Verified on `accent.secondary-tint` in all four themes —
  light 6.03, dark 8.13, cream 6.03, navy 10.07. `border.selected` keeps `#0b7c68` as a boundary
  and is never text. The change had **zero shape bindings** in the design file, so no re-pointing
  was needed.
- **TOK-9** **CLOSED 2026-08-23 — the count-based half was measuring the wrong thing.** Override
  *count* is not a quality signal. A role-correct audit — every token tested against the surface it
  is actually specified on, not against all four — shows **no pairing passes in light and fails in
  cream by inheritance**, which is this criterion's real normative requirement. Cream's 10-vs-30
  gap is not a defect: of the 20 names dark and navy both override, only `border.warm` fails when
  inherited, and it fails in **light and dark too**. The genuine problems the count was gesturing
  at are system-wide and belong to TOK-6 and TOK-7, not to cream.
- **TOK-10** The `*-inverse` family (`surface.inverse-selected`, `surface.inverse-tip`,
  `surface.navy-raised`, `border.inverse`, `accent.primary-inverse`, `accent.primary-inverse-hover`,
  `accent.secondary-inverse`) is retired or is provably unreferenced by any component. Nothing
  that needs "the dark value" gets it from a base token.
- **TOK-11** **CLOSED 2026-08-23 — the ladder is now honest per theme, not uniform.** The
  criterion's own escape clause ("or the ladder is honestly reduced to the number of steps that
  exist") is the correct answer here, and for cream it is **forced, not chosen**: cream's
  `text.secondary` `#6e6656` already measures **4.55 on `surface.sunken`**, so every lighter
  candidate for a distinct third step fails AA (4.15, 3.57, 3.12). There is no headroom. Declared
  ladders — **light 4 steps** (unchanged), **dark 3** (`#9aa4b2` / `#8b949e` / `#59616a`, with
  `placeholder` bound to muted), **navy 3** (`#8fa3d9` / `#748dd0` / `#4364bf`), **cream 2**
  (`#6e6656` / `#9e9482`). A disabled field is never pixel-identical to an active one in any theme.

- **TOK-12** **DONE 2026-08-23.** The four no-op cream overrides (`surface.accent-subtle`,
  `surface.warm`, `border.warm`, `accent.primary-text`) are removed; resolved values are identical
  in all four themes. A test must now keep it at zero. Note the honest consequence for TOK-9: cream
  overrides read **10 vs 30**, not 14 — four of the fourteen were never real overrides.
- **TOK-13** **CLOSED 2026-08-23.** No component hard-codes a hex, and `text.disabled` is now
  themed: light keeps base `#9aa4b2` (2.52 on white — a reasonable disabled weight; the bug was
  that it was *unthemed*, so all four inherited it), plus overrides dark `#59616a` (3.00/2.75),
  cream `#9e9482` (2.89/2.40), navy `#4364bf` (3.08/3.03). Each is derived from that theme's own
  muted value shifted toward its background, so it reads as the same colour deactivated. All sit in
  the conventional disabled band and are **exempt from 1.4.3** as inactive components.

- **TOK-14** **CLOSED 2026-08-23 — verified.** `tokens.css` regenerates byte-identically from
  `tokens.source.json`, the generated CSS contains **zero** `--dbs-colors-*`, and the usage line
  matches the only working invocation. Original wording: the generator is clean: no `{colors.*}` → `--dbs-colors-*` path that emits variables
  the token tree cannot define, and the usage line matches the only working invocation.
  `tokens.css` still regenerates byte-identically after every change.

### 14.2 The `data-theme` collision

`tokens.css` keys four themes off `[data-theme="dark"|"cream"|"navy"]`. Docusaurus/Infima owns
`html[data-theme='dark']` for its own built-ins, and `useColorMode` only ever writes `light` or
`dark`. Left alone, navy — a dark palette — would render every Docusaurus-owned surface (navbar,
admonitions, code blocks, TOC, search modal, pagination) in **light** Infima styling.

- **THM-1** Two attributes. `data-theme` ∈ {light, dark} drives Infima; `data-dbs-palette` ∈
  {light, dark, cream, navy} drives our tokens. Cream → light, navy → dark. The generator emits
  `[data-dbs-palette="…"]` selectors; the swizzled switcher writes and persists both.

### 14.3 The token pipeline

- **THM-2** `tokens.source.json` → generator → `tokens.css`, regenerating byte-identically.
  The Penpot token library is a *derived* store, matching exactly at 143 tokens (base 69 / dark 30
  / cream 14 / navy 30) — verified at revn 1337. Its camelCase convention (`accent.primaryText`)
  versus the source's kebab (`accent.primary-text`) is pinned in the generator, not left to drift.
- **THM-3** The Infima bridge is **load-bearing** under D1, which is what made TOK-3 urgent:
  `--ifm-global-radius: var(--dbs-radius-md)` resolving to a unitless `8` zeroed every Docusaurus
  radius. **Fixed 2026-08-23** — `tokens.source.json` and the generated `tokens.css` now emit
  `4px`/`8px`, verified regenerating byte-identically. Two residues remain, neither affecting the
  build: the Penpot `tokensLib` still emits `"$value": "4"` unitless, and no shape in the design
  file binds a spacing or radius token at all.
- **THM-9** `tokens.source.json` **and** `tokens.css` are committed build inputs. Actions never
  reaches Penpot, and `PENPOT_API_TOKEN` never becomes a CI secret — it is a developer-machine
  credential, in the same excluded category as the survey signing key (SUR-9).
- **THM-10** Verification splits in two, because CI cannot see Penpot:
  **in CI, every build** — `tokens.css` regenerates byte-identically from `tokens.source.json`,
  no raw hex in components, text-position naming rule. All local, no network.
  **On a developer machine, periodically** — reconciling `tokens.source.json` against the Penpot
  file. That check already exists as `penpot_health.py` and its schedule; it stays there and is
  never a build gate.
- **THM-4** Token lint runs in CI: byte-identical regeneration, no theme override identical to
  base, no raw hex in any component file, and only `text.*` / `code.text*` / `*-text` tokens in a
  text-colour position.

### 14.4 Build 27 components, not 96

The Penpot file holds 96 components, which are **27 unique definitions**: 23 forked four ways per
theme, plus 4 deliberately theme-invariant. CONTEXT.md forbids the fork in code — "themes are
token values, not component copies."

- **THM-5** One component per definition, themed by token. Do not mirror the Penpot tree.
- **THM-6** The four theme-invariant components are ruled explicitly. `Code / Copy` and
  `Footer / Navy` are correctly invariant (§2.2 item 7; the Infima footer binds
  `brand.intersect-navy`). `Metric / Dark` and `Avatar / Hexagon` are unruled and both encode a
  theme in their name — decide and rename.

### 14.5 What the design file is authoritative for

Colour, geometry and composition. **Not** spacing (§10.2), **not** typography metrics
(DF-1 holds those), and **not** the prototype page.

- **THM-7** The `Site Pages Designs - full` prototype page is **never** a colour source. The four
  theme pages are clean (one `#000000` each); the prototype page carries **204** off-system
  blacks (`res-icon` ×57, `row-light`/`row-cream`/`row-dark` ×48 each). Four shapes on the cream
  page also still hold `#d8d2c0`, the pre-correction `border.strong`.
- **THM-8** `penpot_health.py` checks structure and token bindings, **not** off-system literals —
  it returned CLEAN at revn 1330 with all of the above present — which is exactly why off-system literals were added to its sweep afterwards. Because the design file is not a
  build input (§2), this is design-side hygiene rather than a build gate: worth adding to the
  scheduled check, but it can never break a deploy.

---

---

## 15. Accessibility

**Target: WCAG 2.2 Level AA**, in full. This section is organised as a conformance map rather than
as the audit list it grew from — A11Y-1…11 came from the 2026-08 audits and predate 2.2, so read
alone they cover what the auditors happened to look at, not the standard. Criterion IDs are stable;
the SC reference on each is what makes coverage checkable.

Criteria in other sections carry accessibility weight too and are not repeated here: **FLOOR-1/2**
(1.4.3 Contrast), **FLOOR-3** (1.4.11 Non-text Contrast), **FLOOR-4** (2.4.7 Focus Visible),
**FLOOR-5** (all interactive states), **FLOOR-6** (1.4.1 Use of Color), **NAV-3** (1.4.10 Reflow),
**NAV-4** and **DF-6** (2.5.8 Target Size), **DF-5** and **SUR-6** (2.5.7 Dragging Movements),
**SRCH-6** (4.1.2 Name Role Value, 4.1.3 Status Messages), **CONTENT-5** (3.3.3 Error Suggestion).

### 15.1 Perceivable

- **A11Y-26** **1.1.1 Non-text Content** for data visualisation. Every chart on `Numbers · Mainnet`
  and the survey dashboard — database growth, largest tables, pain-point bars, per-role shares —
  ships a text alternative: an accessible summary plus the underlying figures as a real table.
  Series are never distinguished by colour alone (FLOOR-6).
- **A11Y-27** **1.1.1** for generated vector art. The generated ERD (SCH-2) and any diagram carry
  `role="img"` with an accessible name and a longer description, or are marked decorative with the
  equivalent information in adjacent text. A generated SVG is not self-describing.
- **A11Y-19** **1.3.1 Info and Relationships** for generated tables. Schema explorer tables use real
  `<th>` with `scope`, and a caption naming the table. No layout tables.
- **A11Y-16** **1.3.5 Identify Input Purpose.** Survey inputs collecting information about the user
  carry the appropriate `autocomplete` token.
- **A11Y-9** **1.4.1 / 4.1.2.** A choice's selected state is exposed programmatically (native
  `:checked`), announced, and carries the `✓` glyph as its non-colour cue. Its box boundary clears
  3:1 on the card (FLOOR-3).
- **A11Y-15** **1.4.4 Resize Text.** 200% zoom loses no content or functionality — distinct from
  NAV-3's 320px reflow, and tested separately.
- **A11Y-14** **1.4.12 Text Spacing.** Content survives user overrides — line height 1.5×, letter
  spacing 0.12em, word spacing 0.16em, paragraph spacing 2em — with no loss. Fixed-height
  components are the usual casualty.
- **A11Y-13** **1.4.13 Content on Hover or Focus.** Any hover- or focus-triggered content is
  **dismissible, hoverable and persistent**. Governs DF-10's glossary component and search previews.

### 15.2 Operable

- **A11Y-2** **2.1.1 / 2.1.2 / 2.4.3.** Full keyboard traversal of every page: no trap, logical
  order, every action reachable without a pointer.
- **A11Y-5** **2.4.1 Bypass Blocks.** A skip link is the first focusable element on every page; the
  sidebar `<nav>` and top `<nav>` each carry a distinct accessible name.
- **A11Y-3** **2.4.7 Focus Visible.** FLOOR-4 satisfied globally, not per component.
- **A11Y-12** **2.4.11 Focus Not Obscured.** No focused element is hidden behind sticky content.
  The navbar is 72px (`--ifm-navbar-height`) and the ⌘K overlay is full-viewport, so
  `scroll-padding-top` accounts for the navbar and the overlay never leaves focus underneath it.
  Tab through every page with the header pinned.
- **A11Y-17** **2.5.3 Label in Name.** Every control's accessible name contains its visible label
  text, so voice control can address it by what it says.
- **A11Y-7** **2.5.7 Dragging Movements.** The pain-point ranking has a keyboard path — Up/Down
  buttons or arrow-key reordering with a live-region announcement — alongside any drag affordance.
  Drag-only is a fail.
- **A11Y-28** **2.1.1 / 4.1.2** for composite widgets. The DF-9 tabbed Docker/nix/cabal block and
  the `Updates` Week/Month/Total/Custom filter group implement the full tab pattern —
  `role="tablist"`/`tab`/`tabpanel`, arrow-key navigation, one tab stop into the group — rather than
  styled buttons. CON-10's remark transform emits this markup, so it is fixed once.
- **A11Y-4** `@media (prefers-reduced-motion: reduce)` sets `scroll-behavior: auto` and shortens
  transitions **without** a blanket `0.01ms` kill — state changes stay legible.

### 15.3 Understandable

- **A11Y-18** **3.1.1 / 3.1.2 Language of Page and Parts.** `lang` and `dir` are correct per locale
  on `<html>`, and any passage in another language carries its own `lang`. With five locales and
  English identifiers (`tx_out`, `consumed_by_tx_id`, config keys) running through translated prose,
  3.1.2 is a live case. Because identifiers are **generated**, mark them once in the renderer rather
  than relying on translators.
- **A11Y-21** **3.2.6 Consistent Help.** Where help, contact or repository links are offered, they
  appear in the same relative place on every page.
- **A11Y-11** **3.3.1 Error Identification.** Any submit path returns a generic message on failure
  — no internal or database error text reaches the client — and the client renders a named error
  state with a retry, plus a loading state while in flight.
- **A11Y-8** **3.3.2 Labels or Instructions.** The frequency slider and the free-text field have
  visible, associated labels. A placeholder is not a label.
- **A11Y-20** **3.3.4 Error Prevention (Legal, Financial, Data).** The survey submits an
  **irreversible transaction that costs the respondent a fee** — squarely financial and
  unreversible, so an explicit review-and-confirm step is **required**, not optional. The design's
  `Survey · 8 Review` board satisfies it; the fee and the permanence of the submission (SUR-13) are
  stated before the wallet is invoked, and answers survive a failed submit (DF-4).
- **A11Y-22** **3.3.7 Redundant Entry.** SUR-22's composed multi-survey form never asks for the same
  information twice across the surveys it merges.
- **A11Y-23** **3.3.8 Accessible Authentication (Minimum).** Wallet connection imposes no cognitive
  function test — no seed-phrase transcription, no puzzle. **Assessed and recorded**, not assumed.

### 15.4 Robust

- **A11Y-6** **1.3.1 / 4.1.2 — the survey is built on native form controls.** Every choice is an
  `<input type="radio">` or `<input type="checkbox">` with a real `<label>`, grouped in a
  `<fieldset>` with a `<legend>`. No `<div>` with a click listener anywhere in the survey.
- **A11Y-25** **The config wizard is held to the same standard as the survey.** `Config · 1…6` is a
  six-step interactive form and had no accessibility criteria at all until now. Native controls per
  A11Y-6; each step a `<fieldset>` with a `<legend>`; step position announced in a live region;
  `Config · 6 Result` reachable and readable by keyboard, with the generated `config.json` selectable
  as text and its Copy control announcing success (DF-4). Its choice cards inherit FLOOR-3 and
  A11Y-9.
- **A11Y-29** The theme/palette switcher (THM-1) exposes an accessible name and its **current
  state**, and is operable by keyboard. Writing two attributes (`data-theme`, `data-dbs-palette`)
  must not leave the control's announced state ambiguous.

### 15.5 Verification

- **A11Y-1** Zero WCAG 2.2 AA failures from an automated pass (axe or equivalent) on: landing, a
  docs page, the schema explorer, search, every survey step, **every config wizard step**, and the
  numbers dashboard — in all four palettes.
- **A11Y-10** A screen-reader pass (VoiceOver or NVDA) completes the whole survey and submits it,
  and completes the config wizard to a generated result.
- **A11Y-24** A **WCAG 2.2 Level A + AA conformance map** is maintained in the repository, marking
  every criterion met, or not-applicable with a stated reason. N/A today: all of 1.2.x (no
  time-based media), 2.2.x (no timing), 2.3.1 (no flashing). Note **4.1.1 Parsing is obsolete and
  removed in WCAG 2.2** — an older checklist will still list it. A11Y-1 does not substitute for this
  map: **axe cannot detect most of A11Y-12…29.**

---

## 16. Content and assets

- **CONTENT-1** `ERD.png` is either deleted from the build output, or it is referenced from the
  schema page **with** `alt` text, explicit `width`/`height`, `loading="lazy"` and a WebP/AVIF
  source. A 1.88 MB unreferenced PNG in the deployed bundle is a fail.
- **CONTENT-2** No emoji or unicode glyph is used as an icon. Icons come from one drawn set at a
  consistent stroke and weight — the design file's own hexagon-and-dot language.
- **CONTENT-3** Body measure is 65–75ch; real content is run at every breakpoint and nothing
  overflows.
- **CONTENT-4** Every docs page renders exactly one `<h1>` and a non-empty `<meta name="description">`;
  the build fails, or at minimum warns, when a content file lacks a title or description.
- **CONTENT-5** Error and empty copy names the problem and the recovery; controls name their
  action. No "Something went wrong".

---

## 17. Navigation

- **NAV-1** The active nav/sidebar item is derived from route data or the content collection —
  adding a page makes it highlight with no code change. Test: add a doc, confirm the sidebar marks
  it active. A hand-enumerated slug chain is a fail.
- **NAV-2** Below 768px the sidebar is a **disclosure** ("On this section") that is collapsed by
  default, keyboard-operable, and announces its expanded state — not a static block pushing
  content down.
- **NAV-3** At least three breakpoints (phone, ~1024px tablet, desktop) with a defined layout for
  each, and no horizontal scroll at 320px. Fixed component widths are restated as min/max
  constraints; text containers allow +35% string growth (the i18n rule).
- **NAV-4** Every interactive row is ≥ 44×44px, including at the mobile breakpoint. Nothing gets
  smaller as the viewport narrows.
- **NAV-5** The active sidebar label uses `accent.primary-text` (5.87:1 on `surface.accent-subtle`),
  never `accent.primary` (4.02 light / 4.02 cream / 4.34 navy).

---

## 18. Performance

- **PERF-1** No unreferenced asset ships. Total image weight on any single page is stated and
  justified.
- **PERF-2** Images carry dimensions and `loading="lazy"` below the fold; raster art ships in a
  modern format.
- **PERF-3** No animation of layout properties (`width`, `height`, `top`, `left`). Progress bars
  and slider fills animate `transform: scaleX()` with `transform-origin: left`.
- **PERF-4** Docs pages ship no client JavaScript beyond what search and the theme switch need;
  survey script stays scoped to the survey route. (The old build got this right — keep it.)
- **PERF-5** `will-change` appears only on a known-expensive, actually-animating element, never at
  rest.

---

---

## 19. Design-file requirements

The design file is now the only specification. These are the things it does **not** say, graded the
same way: each is pass/fail on the built site. Where the fix belonged in the design file it has
already been made — those are listed at the end so the build inherits them rather than re-deciding.

**DF-1 — Typography metrics are a token, not prose.** All 13 Penpot styles carry
`lineHeight: 1.2` and `letterSpacing: 0`, and `tokens.source.json` has no typography tokens at all,
so a build that reads the design file ships documentation prose at 15px on 18px leading. The
metrics below are authoritative and must be emitted as tokens, not hand-typed per component. They
were **deliberately not written back into the Penpot styles**: every one of the 2,601 text shapes on
a theme page is a hand-positioned single-line box, so raising the line height re-measures 2,601 box
heights and would break the drawn geometry. The file specifies *colour and composition*; leading
lives here.

| style | size | weight | line-height | tracking |
|---|---|---|---|---|
| `display-hero` | 58 | 700 | 1.1 | -0.02em |
| `heading-lg` | 38 | 700 | 1.2 | -0.02em |
| `heading-md` | 26 | 700 | 1.25 | -0.01em |
| `title` | 20 | 600 | 1.3 | 0 |
| `wordmark` | 19 | 300 | 1.2 | 0 |
| `body` | 15 | 400 | **1.65** | 0 |
| `body-sm` | 13 | 400 | 1.6 | 0 |
| `label` | 14 | 600 | 1.2 | 0 |
| `label-sm` | 12 | 700 | 1.2 | 0.04em |
| `caption` | 12 | 400 | 1.4 | 0 |
| `mono` | 13 | 400 | 1.5 | 0 |
| `mono-sm` | 11.5 | 400 | 1.5 | 0 |
| `eyebrow` | 13 | 600 | 1.2 | 0 |

Also required: docs prose measure **65–75ch** (the file draws 74ch and is right), and no new type
size outside these thirteen.

**DF-2 — The eight undrawn control states.** The design file specifies hover / active / focus /
disabled for exactly three primitives (button, link, input). The product has at least eleven
controls: choice card, multi-select box, satisfaction tile, slider, drag-rank row, accordion
header, sidebar item, tab, search-result row, copy button, theme menu. Every one of them ships all
of FLOOR-5's states, and the design file is **not** evidence that they were considered.

**DF-3 — Focus is drawn nowhere.** Zero focus indicators exist on any of the 212 theme-page boards,
though `DESIGN.md` says the ring "must be extended from form fields to every interactive element".
FLOOR-4 already covers the requirement; this notes that there is no reference drawing to copy, so
the build authors it.

**DF-4 — Loading and error states do not exist in the design at all.** Zero loading indicators and
zero validation/error states across 26,025 shapes, for a nine-step form and a live search. Required
on the built site: field-level validation with the message beside the field, a failed-submit path
that preserves the user's answers, a zero-results state for search (distinct from the existing and
good *pre-query* empty state), an empty state for the results dashboard before responses exist, a
saved-confirmation for `Save & exit`, and a copied-confirmation on every Copy button.

**DF-5 — Drag-only ranking is a WCAG 2.2 failure.** `Survey · 5 Pain points` is drag-to-reorder with
no pointer alternative — **SC 2.5.7 Dragging Movements**. The built control must offer a
single-pointer path (up/down buttons or a numbered select per row) and a keyboard path, with the
drag retained as an enhancement.

**DF-6 — Target sizes.** 1,004 of 1,448 interactive targets in the design are below 24×24: every
real button is 44 tall, but 640 sidebar links and 112 TOC links exist only as 16px text with no hit
area, copy buttons are 62×20, toggles 38×22. On the built site every interactive row has a
**minimum 44×44** hit area (SC 2.5.8 asks 24×24; 44 is the house floor), regardless of how tall its
text is drawn.

**DF-7 — One artboard is not a responsive spec.** All 245 boards are 1440 wide. Mobile, tablet and
wide compositions are unspecified and are the build's to author. Two concrete traps measured from
the file: the nav links span 403px and at PRODUCT.md's own **+35% string growth** reach x=964,
overlapping the search field that starts at x=880; and no board contains a **locale switcher or a
version selector**, the two controls Docusaurus was chosen for. The nav bar needs restructuring,
not 86px of slack.

**DF-8 — Directionality.** Crosshair ticks, left accent bars, `▸` carets, `←/→` pagination and the
3px sidebar active bar are all directional and none is mirrored or noted. If an RTL locale ships,
these are the things that break.

**DF-9 — Docs content model.** `Docs · Installation` — the page a blocked operator opens — has no
H2/H3 at all, and its right rail carries six competitor logos and a survey CTA instead of the table
of contents `DESIGN.md` specifies. The built installation page has real heading structure
(Prerequisites / Docker / From source / Verify / Troubleshooting), a working TOC in the right rail,
a tabbed Docker-vs-nix-vs-cabal block, and the ecosystem logos moved to the Ecosystem page.

**DF-10 — Glossary.** Chain vocabulary is deliberately never softened (correct, per PRODUCT.md),
but there is no glossary or inline-definition component for a first-time operator meeting `SMASH`,
`conway`, `tx_out prune/consumed` or `off-chain pool data`. Add one.

**DF-11 — Delete the third token store, or keep exactly one.** Repaired 2026-08-20 (see below), but
the standing rule for the rebuild is: **`tokens.source.json` is the single origin.** `tokens.css`
and the Penpot token library are both generated from it. Three stores is the defect; two that
disagree is the symptom.

#### Already fixed in the design file — inherit, do not redo

- **Non-text contrast is closed.** 283 of 1,038 control boundaries and icons were below 3:1; the
  file now reads **0 of 1,057**. `border/interactive` was corrected to clear 3:1 on both
  `surface.base` and `surface.sunken` in every theme (light `#7e8893`, cream `#857e6a`; dark
  `#6e7781` and navy `#8fa3d9` already passed) and applied to the theme switcher, keycaps,
  checkbox boxes and the informational rules inside search and nav. Status glyphs on light and
  cream moved to their existing deep text-grade values (`#8a6100`, `#0b7c68`). Toggle knobs gained
  a 1px `#06584a` edge. The unthemed `#000000` outline on 84 search-result icons is gone.
- **The Penpot token library is exportable again** — 143 tokens, exactly matching
  `tokens.source.json`, 0 missing, 0 extra, 0 value mismatches, all four themes preserved.
- **Space and radius tokens still ship unitless** (§4.1) — that remains the highest-value one-line
  fix in the generator, and it is unchanged by any of the above.

---

---

## 20. Design-file work this build depends on

**Blocking — no UI work can start without these.** They are design deliverables, not engineering
tasks, and they are on the critical path.

1. **DF-7 — nothing below 1440px exists.** All 245 boards are 1440 wide. NAV-2 and NAV-3 require a
   defined layout at three breakpoints; those layouts have never been drawn. Also undrawn: the
   **locale switcher** and the **version selector** — the two controls Docusaurus was chosen for,
   now needed at five locales and five version lines.
2. **DF-2 — eight of eleven controls have no drawn states.** The file specifies hover / active /
   focus / disabled for button, link and input only. FLOOR-5 requires the full set on choice cards,
   multi-select boxes, satisfaction tiles, sliders, drag-rank rows, accordion headers, sidebar
   items, tabs, search-result rows, copy buttons and the theme menu.
3. **DF-3 — focus is drawn nowhere.** Zero focus indicators across 212 theme-page boards. FLOOR-4
   and A11Y-3 require them globally; there is no reference drawing to work from.

**Non-blocking but required before the surfaces they gate ship:**

4. **TOK-7 / M5** — the search field's placeholder token fails AA on its specified surface.
   Blocks §7.
5. **TOK-3** — **done in the source and generated CSS (2026-08-23)**; only the Penpot `tokensLib`
   remains unitless, which does not affect the build (§4).
6. **SUR-13 / SUR-14** — **closed in the design file at revn 1337 (2026-08-23)**; the built
   survey must still carry the corrected copy (checklist B32).
7. **NUM-2** — replace `PLACEHOLDER` / `STUB` with stated estimates. Blocks §11.
8. **Six missing search states and the environment banner** are authored, not drawn (SRCH-9,
   ENV-4). Neither exists in the Penpot file.
9. **THM-6** — rule `Metric / Dark` and `Avatar / Hexagon`.

---

## 21. Open decisions

**Settled 2026-08-21:** preview (not preprod) · locales EN / JA / ES / DE / VI via Crowdin ·
`dbsync.cardano.intersect.org` for production, `lidonation.github.io/cardano-db-sync` for dev ·
3-of-5 native-script multisig · `end_epoch` 2000 (&asymp; 2045) · `eligible_roles = [Keyholder]`.

**Settled 2026-08-23:** analytics — **none at launch**; revisit with Intersect around production
cutover. Anything adopted later must need no backend, no third party the upstream inherits by
default, and must survive EU privacy expectations (DE is a launch locale).

Remaining:

- **Which five signers** hold the 3-of-5. The script hash *is* the credential (SUR-10), so the set
  cannot change without becoming a different owner — decide before the core survey is signed.
- **The core question set** (SUR-16, SUR-17). Frozen at signing, piloted on preview first. This is
  now the highest-risk remaining item: everything else in this document is recoverable, and it is
  not.
- **SEC-7** — confirm `window.cardano` appears under the production CSP, per wallet. Narrowed to a
  minutes-long per-wallet check; **SEC-8** already bounds the blast radius to the survey route.
- **Locale evidence.** EN / JA / ES / DE / VI is grounded in the ecosystem's sustained translation
  capacity — cardano.org ships JA / DE / VI through Crowdin — not in an operator census; no
  reliable SPO-by-country data was found. Revisit if evidence shows a different distribution among
  db-sync operators, who skew more infrastructure-engineer than cardano.org's audience.

---

## Appendix A — Outstanding audit findings

The **18** findings from the 2026-08 audits that still have work behind them. Each maps to the
criterion that carries it — **act on the criterion, not on the row.** When the last row clears,
this appendix goes with it.

**24 of the original 42 are cleared:**

- **4** graded **(a)** — already fixed in the token layer before the rebuild.
- **9** graded **(b)** — structurally unable to recur in a fresh build; they described defects in
  code being deleted.
- **3** resolved after grading — **B1** (unitless `space`/`radius`, fixed 2026-08-23 in
  `tokens.source.json` and the generated `tokens.css`), **N1** (the generator emits no
  `--dbs-colors-*`), **N2** (usage line matches the working invocation).
- **8** pruned 2026-08-23 once every criterion they cited had closed — **M1**, **M3**, **M5**,
  **M7**, **m3**, **m6**, **m7**, **m8**.

The judgements are kept so a moot finding is not re-raised; the rows themselves are gone.

**All 18 remaining are graded (c), and every one needs the build to exist** — TOK 8, NAV 7,
A11Y 6, CONTENT 3, FLOOR 2. Nothing in this list can be fixed today.

| # | Finding | Verdict | Resolution |
|---|---|---|---|
| S1 | P0 Design tokens not consumed by the code | **c** | The parallel palette dies with `global.css`, but consuming the tokens is a requirement → TOK-1, TOK-2 |
| S3 | P0 Survey is keyboard- and screen-reader-inaccessible | **c** | The single biggest requirement → A11Y-6…A11Y-10 |
| S4 | P0 No visible focus indicator | **c** | Tokens exist (`--dbs-focus-ring`, `--dbs-focus-offset`; 4.53 light / 6.09 dark / 4.37 cream / 5.46 navy) but nothing applies them → A11Y-3 |
| S5 | P1 No reduced-motion path | **c** | → A11Y-4 |
| S6 | P1 Touch targets ~26–28px | **c** | → NAV-4 |
| S7 | P1 One breakpoint (768px), no mobile nav pattern | **c** | → NAV-2, NAV-3 (same root as M8) |
| S8 | P1 Borders below the 3:1 UI threshold | **c** | Partly advanced (`border.interactive` now themed 4×, `border.selected` added) but `border.default` is still 1.28:1 → FLOOR-3, TOK-6 |
| S9 | P1 Server error text returned to the client | **c** | → A11Y-11 (any submit endpoint the new stack grows) |
| S12 | P2 1.88 MB unreferenced `ERD.png` | **c** | Still present, still unreferenced → CONTENT-1 |
| S13 | P2 Emoji as product iconography (💻 ⚙️ 🧭 📊 ⠿) | **c** | → CONTENT-2 |
| S15 | P3 `isActive` is a 20-branch OR chain in `Nav.astro` | **c** | → NAV-1 |
| S16 | P3 No skip link; sidebar `<aside>` unnamed | **c** | → A11Y-5 |
| S17 | P3 No `<h1>`/metadata guarantee per doc page | **c** | → CONTENT-4 |

| # | Finding | Verdict | Resolution |
|---|---|---|---|
| B2 | BLOCKER `badge`/`card-warm` bind text to `brand-intersect-navy` (1.08 dark / 1.26 navy) | **c** | Confirmed by computation; no `text.on-tint` token exists yet → TOK-4, TOK-5 |
| M4 | MAJOR `sidebar-item-active` binds its label to a fill token | **c** | The right token exists and is themed 4× (`accent.primary-text`); the wrong binding must not be re-authored → TOK-5, NAV-5 |
| M8 | MAJOR Nothing below 1440px has been designed | **c** | → NAV-2, NAV-3 |
| m1 | MINOR Vestigial `*-inverse` family that cannot theme-switch | **c** | 7 names still present → TOK-10 |
| m4 | MINOR Control boundaries fail 3:1 in every theme | **c** | Same requirement as S8 → FLOOR-3, TOK-6 |

**Two pairs above are one requirement each, seen from both audits** — S7/M8 (nothing below 1440px
is designed) and S8/m4 (control boundaries fail 3:1). So these 18 rows carry **16 distinct
requirements**. The accounting for the other 24 is at the top of this appendix.

---