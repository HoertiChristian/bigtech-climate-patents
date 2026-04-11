"""
Analyze Y02/Y04S CPC code depth across patent data.

Checks how many patents have full subgroup-level codes (e.g. Y02E10/70)
vs. only shallow codes (e.g. Y02E, Y02E10, Y02E10/00).

Run from: scripts/python/
Usage:    python analyze_cpc_depth.py
"""

import json
import re
from pathlib import Path
from collections import Counter, defaultdict

DATA_DIR = Path(__file__).resolve().parent / "data"

COMPANIES = ["alphabet", "amazon", "apple", "meta", "microsoft"]


def classify_code_depth(code: str) -> str:
    """
    Classify a Y02/Y04S code by its depth in the CPC hierarchy.

    Examples:
        Y02          → section
        Y02E         → subclass
        Y02E10       → main_group  (no slash)
        Y02E10/00    → main_group  (/00 = main group entry)
        Y02E10/70    → subgroup    (specific subgroup)
        Y02E10/763   → subgroup
    """
    code = code.strip()

    if "/" in code:
        after_slash = code.split("/", 1)[1]
        if after_slash == "00":
            return "main_group"       # e.g. Y02E10/00
        else:
            return "subgroup"         # e.g. Y02E10/70, Y02E10/763
    elif re.match(r"^Y\d{2}[A-Z]\d+$", code):
        return "main_group"           # e.g. Y02E10 (no slash)
    elif re.match(r"^Y\d{2}[A-Z]$", code):
        return "subclass"             # e.g. Y02E
    else:
        return "section"              # e.g. Y02


def extract_y_codes(patent: dict) -> list[str]:
    """Extract all Y02/Y04S codes from a patent record."""
    codes = []
    cpc = patent.get("biblio", {}).get("classifications_cpc", {})
    for c in cpc.get("classifications", []):
        symbol = c.get("symbol", "")
        if symbol.startswith("Y02") or symbol.startswith("Y04S"):
            codes.append(symbol)
    return codes


def deepest_level(codes: list[str]) -> str:
    """Return the deepest classification level among a list of codes."""
    hierarchy = ["section", "subclass", "main_group", "subgroup"]
    best = 0
    for code in codes:
        depth = classify_code_depth(code)
        idx = hierarchy.index(depth)
        if idx > best:
            best = idx
    return hierarchy[best]


def main():
    # ── Load patents ──────────────────────────────────────────────────────
    all_patents: dict[str, list[dict]] = {}

    for company in COMPANIES:
        path = DATA_DIR / f"{company}_patents.json"
        if path.exists():
            with open(path, encoding="utf-8") as f:
                all_patents[company] = json.load(f)
            print(f"  Loaded {company}: {len(all_patents[company])} patents")
        else:
            print(f"  {path.name} not found — skipping")

    if not all_patents:
        # Fall back to patents_raw.json
        raw_path = DATA_DIR / "patents_raw.json"
        if raw_path.exists():
            print(f"  Falling back to {raw_path.name}")
            with open(raw_path, encoding="utf-8") as f:
                all_patents = json.load(f)
        else:
            print("No patent data found in", DATA_DIR)
            return

    print()

    # ── Analyze code depth ────────────────────────────────────────────────
    total_patents = 0
    depth_by_company: dict[str, Counter] = defaultdict(Counter)
    code_depth_counts = Counter()        # per individual code
    subclass_distribution = Counter()    # Y02E, Y02D, etc.
    shallow_examples: list[dict] = []    # sample patents with no subgroup

    for company, patents in all_patents.items():
        for patent in patents:
            y_codes = extract_y_codes(patent)
            if not y_codes:
                continue

            total_patents += 1
            deepest = deepest_level(y_codes)
            depth_by_company[company][deepest] += 1

            for code in y_codes:
                depth = classify_code_depth(code)
                code_depth_counts[depth] += 1

                # Extract subclass prefix (e.g. Y02E, Y02D, Y04S)
                m = re.match(r"(Y\d{2}[A-Z]?)", code)
                if m:
                    subclass_distribution[m.group(1)] += 1

            # Collect some examples of patents without subgroup-level codes
            if deepest != "subgroup" and len(shallow_examples) < 10:
                title = "No title"
                titles = patent.get("biblio", {}).get("invention_title", [])
                if titles:
                    title = titles[0].get("text", "No title")
                shallow_examples.append({
                    "company": company,
                    "lens_id": patent.get("lens_id", ""),
                    "title": title[:80],
                    "y_codes": y_codes,
                    "deepest": deepest,
                })

    # ── Print results ─────────────────────────────────────────────────────
    hierarchy = ["section", "subclass", "main_group", "subgroup"]

    print("=" * 65)
    print("  Y02/Y04S Code Depth Analysis")
    print("=" * 65)

    # Per-company breakdown: deepest code level per patent
    print("\n  Patents by deepest Y02/Y04S code level (per patent):\n")
    header = f"  {'Company':<14}" + "".join(f"{lvl:<14}" for lvl in hierarchy) + "Total"
    print(header)
    print("  " + "─" * len(header))

    grand = Counter()
    for company in sorted(depth_by_company):
        counts = depth_by_company[company]
        total = sum(counts.values())
        row = f"  {company:<14}"
        for lvl in hierarchy:
            c = counts.get(lvl, 0)
            pct = f"({c/total*100:.0f}%)" if total else ""
            row += f"{c} {pct:<11}"
            grand[lvl] += c
        row += str(total)
        print(row)

    total_all = sum(grand.values())
    row = f"  {'TOTAL':<14}"
    for lvl in hierarchy:
        c = grand[lvl]
        pct = f"({c/total_all*100:.0f}%)" if total_all else ""
        row += f"{c} {pct:<11}"
    row += str(total_all)
    print("  " + "─" * len(header))
    print(row)

    # Subclass distribution
    print("\n\n  Y02/Y04S subclass distribution (all codes, not unique per patent):\n")
    for subclass, count in subclass_distribution.most_common():
        bar = "█" * (count // max(1, total_all // 40))
        print(f"    {subclass:<6} {count:>6}  {bar}")

    # Shallow examples
    if shallow_examples:
        print(f"\n\n  Sample patents WITHOUT subgroup-level Y02/Y04S codes:\n")
        for ex in shallow_examples:
            print(f"    [{ex['company']}] {ex['lens_id']}")
            print(f"      Title:  {ex['title']}")
            print(f"      Codes:  {', '.join(ex['y_codes'])}")
            print(f"      Depth:  {ex['deepest']}")
            print()

    # Summary
    subgroup_count = grand.get("subgroup", 0)
    non_subgroup = total_all - subgroup_count
    print("─" * 65)
    print(f"  SUMMARY")
    print(f"    Total patents with Y02/Y04S codes:  {total_all}")
    print(f"    With subgroup-level codes:           {subgroup_count} ({subgroup_count/total_all*100:.1f}%)")
    print(f"    Without (shallow only):              {non_subgroup} ({non_subgroup/total_all*100:.1f}%)")
    print("─" * 65)


if __name__ == "__main__":
    main()
