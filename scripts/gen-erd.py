#!/usr/bin/env python3
# Emit a graphviz .dot ER diagram from extracted schema data.
# Usage: gen-erd.py COLUMNS_FILE PKS_FILE FK_FILE OUT_DOT
#   COLUMNS_FILE lines: table|ordinal|column|type
#   PKS_FILE     lines: table|column
#   FK_FILE      lines: child|column|parent   (logical FKs, e.g. parsed from migrations)
import sys, html

cols_file, pks_file, fk_file, out_dot = sys.argv[1:5]

cols = {}
for line in open(cols_file):
    line = line.rstrip("\n")
    if not line: continue
    t, _o, c, ty = line.split("|", 3)
    cols.setdefault(t, []).append((c, ty))
tables = set(cols)

pk = {}
for line in open(pks_file):
    line = line.rstrip("\n")
    if not line: continue
    t, c = line.split("|", 1)
    pk.setdefault(t, set()).add(c)

edges, fkcols, seen = [], {}, set()
for line in open(fk_file):
    line = line.rstrip("\n")
    if not line: continue
    child, col, parent = line.split("|", 2)
    if child in tables and parent in tables and any(c == col for c, _ in cols[child]):
        if (child, col, parent) in seen: continue
        seen.add((child, col, parent))
        edges.append((child, col, parent))
        fkcols.setdefault(child, set()).add(col)

def domain(t):
    if t.startswith("off_chain"): return "offchain"
    if t.startswith("ma_") or t == "multi_asset": return "multiasset"
    if t.startswith("pool_") or t in {"delisted_pool", "reserved_pool_ticker"}: return "pool"
    if t.startswith("stake_") or t in {"delegation", "reward", "reward_rest",
        "epoch_stake", "epoch_stake_progress", "treasury", "reserve", "pot_transfer"}: return "stake"
    if t.startswith("drep_") or t.startswith("committee") or t in {"constitution",
        "new_committee", "gov_action_proposal", "voting_procedure", "voting_anchor",
        "treasury_withdrawal", "delegation_vote", "param_proposal", "event_info"}: return "gov"
    if t.startswith("epoch") or t in {"ada_pots", "cost_model"}: return "epoch"
    if t in {"block", "tx", "tx_out", "tx_in", "collateral_tx_in", "collateral_tx_out",
        "reference_tx_in", "tx_metadata", "tx_cbor", "datum", "redeemer", "redeemer_data",
        "script", "extra_key_witness", "withdrawal", "slot_leader", "reverse_index", "meta"}: return "core"
    return "misc"

FILL = {"core": ("#BDD7EE", "#EAF3FB"), "stake": ("#C6E0B4", "#EDF6E7"),
        "pool": ("#FFE699", "#FFF7DF"), "gov": ("#D9C2EC", "#F1E9F8"),
        "offchain": ("#F8CBAD", "#FCE9DE"), "epoch": ("#B4E5E0", "#E3F6F4"),
        "multiasset": ("#FFD9CC", "#FFEDE6"), "misc": ("#D9D9D9", "#F2F2F2")}
ORDER = ["core", "stake", "pool", "gov", "offchain", "epoch", "multiasset", "misc"]
LABELS = {"core": "blocks / tx / outputs", "stake": "stake / rewards", "pool": "pools",
          "gov": "governance", "offchain": "off-chain metadata", "epoch": "epoch / protocol",
          "multiasset": "multi-asset", "misc": "bookkeeping"}

def esc(s): return html.escape(s, quote=True)

def node(t):
    hc, bc = FILL[domain(t)]
    rows = [f'<TR><TD BGCOLOR="{hc}" ALIGN="CENTER"><B>{esc(t)}</B></TD></TR>']
    for c, ty in cols[t]:
        mark = "PK " if c in pk.get(t, set()) else ("FK " if c in fkcols.get(t, set()) else "   ")
        b0, b1 = ("<B>", "</B>") if mark.strip() else ("", "")
        rows.append(f'<TR><TD BGCOLOR="{bc}" ALIGN="LEFT" PORT="{esc(c)}">'
                    f'<FONT POINT-SIZE="9">{mark}{b0}{esc(c)}{b1} : {esc(ty)}</FONT></TD></TR>')
    return (f'  "{t}" [label=<<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" '
            f'CELLPADDING="3">{"".join(rows)}</TABLE>>];')

def legend():
    rows = ['<TR><TD COLSPAN="2" ALIGN="CENTER"><B>Legend</B></TD></TR>']
    for d in ORDER:
        hc, _ = FILL[d]
        rows.append(f'<TR><TD BGCOLOR="{hc}" WIDTH="20"> </TD>'
                    f'<TD ALIGN="LEFT"><FONT POINT-SIZE="10">{LABELS[d]}</FONT></TD></TR>')
    rows.append('<TR><TD COLSPAN="2" ALIGN="LEFT"><FONT POINT-SIZE="9">'
                'PK = primary key, FK = foreign key. Crow foot = many side.<BR ALIGN="LEFT"/>'
                'FK constraints are dropped at run time for insert speed; edges are the logical references.'
                '</FONT></TD></TR>')
    return ('  "legend" [shape=plaintext, label=<<TABLE BORDER="1" CELLBORDER="0" '
            f'CELLSPACING="4" CELLPADDING="4" BGCOLOR="white">{"".join(rows)}</TABLE>>];')

out = ["digraph erd {",
       '  graph [layout=sfdp, overlap=prism, splines=true, K=1.2, repulsiveforce=1.6, '
       'fontname="Helvetica", bgcolor="white", pad=0.4];',
       '  node [shape=plaintext, fontname="Helvetica"];',
       '  edge [color="#8C8C8C", arrowsize=0.8, dir=both, arrowtail=crow, arrowhead=none, penwidth=1.0];']
for t in sorted(tables):
    out.append(node(t))
out.append(legend())
for child, col, parent in edges:
    out.append(f'  "{child}":"{col}" -> "{parent}":"id" ;')
out.append("}")
open(out_dot, "w").write("\n".join(out))
sys.stderr.write(f"tables={len(tables)} edges={len(edges)} -> {out_dot}\n")
