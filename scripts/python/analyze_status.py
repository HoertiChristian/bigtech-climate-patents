"""
Deduplicate patents and show legal status breakdown.

Run from: scripts/python/
Usage:    python analyze_status.py
"""

import json
from pathlib import Path
from collections import Counter, defaultdict

DATA_DIR = Path(__file__).resolve().parent / "data"
COMPANIES = ["alphabet", "amazon", "apple", "meta", "microsoft"]


def main():
    # ── Load patents ──────────────────────────────────────────────────────
    all_patents = []
    for company in COMPANIES:
        path = DATA_DIR / f"{company}_patents.json"
        if path.exists():
            with open(path, encoding="utf-8") as f:
                patents = json.load(f)
            for p in patents:
                p["_company"] = company
            all_patents.extend(patents)
            print(f"  Loaded {company}: {len(patents)} patents")
        else:
            print(f"  {path.name} not found — skipping")

    print(f"\n  Total loaded: {len(all_patents)}")

    # ── Deduplicate by lens_id ────────────────────────────────────────────
    seen = {}
    for p in all_patents:
        lid = p.get("lens_id", "")
        if lid and lid not in seen:
            seen[lid] = p

    deduped = list(seen.values())
    removed = len(all_patents) - len(deduped)
    print(f"  After dedup:  {len(deduped)} ({removed} duplicates removed)\n")

    # ── Legal status breakdown ────────────────────────────────────────────
    status_counts = Counter()
    status_by_company = defaultdict(Counter)

    for p in deduped:
        status = (
            p.get("legal_status", {}).get("patent_status", "NOT_AVAILABLE")
            if isinstance(p.get("legal_status"), dict)
            else "NOT_AVAILABLE"
        )
        company = p.get("_company", "Unknown")
        status_counts[status] += 1
        status_by_company[company][status] += 1

    # Overall
    print("=" * 55)
    print("  Legal Status Breakdown (all companies)")
    print("=" * 55)
    for status, count in status_counts.most_common():
        pct = count / len(deduped) * 100
        bar = "█" * (count // max(1, len(deduped) // 40))
        print(f"    {status:<20} {count:>6}  ({pct:5.1f}%)  {bar}")
    print(f"    {'TOTAL':<20} {len(deduped):>6}")

    # Per company
    all_statuses = sorted(status_counts.keys())
    print(f"\n\n  Breakdown by company:\n")
    header = f"    {'Status':<20}" + "".join(f"{c:<12}" for c in COMPANIES)
    print(header)
    print("    " + "─" * (20 + 12 * len(COMPANIES)))
    for status in all_statuses:
        row = f"    {status:<20}"
        for company in COMPANIES:
            c = status_by_company[company].get(status, 0)
            row += f"{c:<12}"
        print(row)

    # Totals row
    print("    " + "─" * (20 + 12 * len(COMPANIES)))
    row = f"    {'TOTAL':<20}"
    for company in COMPANIES:
        row += f"{sum(status_by_company[company].values()):<12}"
    print(row)

    # ── Publication type breakdown (bonus) ────────────────────────────────
    pub_counts = Counter()
    for p in deduped:
        pub_type = p.get("publication_type", "NOT_AVAILABLE")
        pub_counts[pub_type] += 1

    print(f"\n\n  Publication Type Breakdown:")
    print("  " + "─" * 45)
    for pt, count in pub_counts.most_common():
        pct = count / len(deduped) * 100
        print(f"    {pt:<35} {count:>6}  ({pct:5.1f}%)")


if __name__ == "__main__":
    main()
