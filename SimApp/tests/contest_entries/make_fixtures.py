# Builds the synthetic DK entries CSVs the contest-entries test reads.
# Layout copied from DraftKings' "Edit Entries" download (Oct 2026).
import csv, itertools
games = [("BUF","MIA","09/28/2026 01:00PM ET"), ("KC","LAC","09/28/2026 04:25PM ET")]
shape = [("QB",2,"QB"),("RB",3,"RB/FLEX"),("WR",4,"WR/FLEX"),("TE",2,"TE/FLEX"),("DST",1,"DST")]
pool, pid = [], 40000000
for a,h,t in games:
    for team in (a,h):
        for pos,n,rp in shape:
            for k in range(n):
                pid += 1
                name = f"{team} {pos}{k+1}" if pos!="DST" else f"{team} Defense"
                sal = 3000 + 500*((pid*7)%10)
                pool.append([pos, f"{name} ({pid})", name, str(pid), rp, str(sal), f"{a}@{h} {t}", team, "10.5"])

def write(path, slots, contests, pool, prefill=None):
    instr = ["1. Locate the player you want to select in the list below ",
             "2. Copy the ID of your player (you can use the Name + ID column or the ID column) ",
             "3. Paste the ID into the roster position desired ",
             "4. You must include an ID for each player; you cannot use just the player's name ",
             "5. You can create up to 500 lineups per file ", ""]
    rows = []
    eid = 4400000000
    for name, cid, fee, n in contests:
        for _ in range(n):
            eid += 1
            rows.append([str(eid), name, cid, fee] + [""]*len(slots))
    prefill = prefill or {}
    for i, cells in prefill.items(): rows[i][4:4+len(slots)] = cells
    width = 4 + len(slots) + 1
    out = [["Entry ID","Contest Name","Contest ID","Entry Fee"] + slots + ["", "Instructions"]]
    tail = instr + [["Position","Name + ID","Name","ID","Roster Position","Salary","Game Info","TeamAbbrev","AvgPointsPerGame"]] + pool
    n = max(len(rows), len(tail))
    for i in range(n):
        left = rows[i] + [""] if i < len(rows) else [""]*width
        right = tail[i] if i < len(tail) else []
        right = [right] if isinstance(right, str) else right
        out.append(left + right)
    with open(path, "w", newline="") as f:
        f.write("﻿"); csv.writer(f, lineterminator="\r\n").writerows(out)

contests = [("NFL $3M Fantasy Football Millionaire [$1M to 1st]","181000001","$20.00",3),
            ("NFL $150K Flea Flicker [150 Entry Max]","181000002","$5.00",10),
            ("NFL $50K Red Zone [20 Entry Max]","181000003","$3.00",5),
            ("NFL $25K Single Entry Special [Single Entry]","181000004","$25.00",1),
            ("NFL $10K Double Up","181000005","$10.00",2)]
by = {p[3]: p for p in pool}
def nid(i): return by[i][1]
ids = [p[3] for p in pool]
# entry 0: already filled (pre-lock); entry 1: early game locked (late swap)
q = lambda t,pos,k: next(p for p in pool if p[7]==t and p[0]==pos and p[2].endswith(str(k)) or (p[7]==t and pos=="DST" and p[0]=="DST"))[1]
filled = [q("BUF","QB",1), q("BUF","RB",1), q("KC","RB",1), q("BUF","WR",1), q("MIA","WR",1), q("KC","WR",1), q("BUF","TE",1), q("LAC","WR",2), q("LAC","DST",1)]
locked = [c + " (LOCKED)" if ("BUF" in c or "MIA" in c) else c for c in filled]
write("DKEntries_nfl_classic.csv", ["QB","RB","RB","WR","WR","WR","TE","FLEX","DST"], contests, pool,
      prefill={0: filled, 1: locked})

# Showdown: KC @ LAC, every player has a CPT id and a FLEX id.
sd_pool, pid = [], 41000000
for p in pool:
    if p[7] not in ("KC","LAC"): continue
    for rp in ("CPT","FLEX"):
        pid += 1
        sal = int(p[5]) * (15 if rp=="CPT" else 10) // 10
        sd_pool.append([p[0], f"{p[2]} ({pid})", p[2], str(pid), rp, str(sal), p[6], p[7], p[8]])
write("DKEntries_nfl_showdown.csv", ["CPT","FLEX","FLEX","FLEX","FLEX","FLEX"],
      [("NFL Showdown $100K [150 Entry Max]","182000001","$4.00",6),
       ("NFL Showdown $5 Double Up","182000002","$5.00",2)], sd_pool)
print("ok", len(pool), len(sd_pool))
