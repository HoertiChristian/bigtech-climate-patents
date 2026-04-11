"""
Analyze impact of simple family deduplication on patent counts.

Groups patents by simple family and shows how many duplicates
would be removed per company.

Run from: scripts/python/
Usage:    python analyze_families.py

Requires 'families' in INCLUDE_FIELDS when fetching data.
"""

import json
from pathlib import Path
from collections import defaultdict

DATA_DIR = Path(__file__).resolve().parent / "data"
COMPANIES = ["alphabet", "amazon", "apple", "meta", "microsoft"]


def get_simple_family_key(patent: dict) -> str | None:
    """
    Build a unique key for the simple family of a patent.
    Uses the sorted set of member lens_ids as a hashable identifier.
    Falls back to the patent's own lens_id if no family data.
    """
    families = patent.get("families", {})
    if not families:
        return patent.get("lens_id")

    simple = families.get("simple_family", {})
    members = simple.get("members", [])

    if not members:
        return patent.get("lens_id")

    member_ids = sorted(m.get("lens_id", "") for m in members if m.get("lens_id"))
    if member_ids:
        return "|".join(member_ids)

    return patent.get("lens_id")


def pick_representative(patents: list[dict]) -> dict:
    """
    From a group of patents in the same family, pick the best representative.
    Priority: granted > application > other, then earliest publication date.
    """
    type_priority = {"GRANTED_PATENT": 0, "PATENT_APPLICATION": 1}

    def sort_key(p):
        pub_type = p.get("publication_type", "UNKNOWN")
        priority = type_priority.get(pub_type, 2)
        date = p.get("date_published", "9999-99-99")
        return (priority, date)

    return sorted(patents, key=sort_key)[0]


def main():
    # ── Load patents ──────────────────────────────────────────────────────
    patents_by_company: dict[str, list[dict]] = {}
    for company in COMPANIES:
        path = DATA_DIR / f"{company}_patents.json"
        if path.exists():
            with open(path, encoding="utf-8") as f:
                patents = json.load(f)
            patents_by_company[company] = patents
            print(f"  Loaded {company}: {len(patents)} patents")
        else:
            print(f"  {path.name} not found — skipping")

    # ── Deduplicate by lens_id first ──────────────────────────────────────
    seen_ids = set()
    all_deduped = []
    company_for_patent = {}

    for company, patents in patents_by_company.items():
        for p in patents:
            lid = p.get("lens_id", "")
            if lid and lid not in seen_ids:
                seen_ids.add(lid)
                all_deduped.append(p)
                company_for_patent[lid] = company

    print(f"\n  After lens_id dedup: {len(all_deduped)} unique documents")

    # ── Check family data availability ────────────────────────────────────
    has_family = sum(1 for p in all_deduped if p.get("families"))
    print(f"  Patents with family data: {has_family}/{len(all_deduped)}")

    if has_family == 0:
        print("\n  ⚠  No family data found. Add 'families' to INCLUDE_FIELDS")
        print("     and re-run the API query first.")
        return

    # ── Group by simple family ────────────────────────────────────────────
    families: dict[str, list[dict]] = defaultdict(list)
    for p in all_deduped:
        key = get_simple_family_key(p)
        families[key].append(p)

    # Stats
    family_sizes = [len(members) for members in families.values()]
    multi_member = [f for f in families.values() if len(f) > 1]

    print(f"\n  Unique simple families:  {len(families)}")
    print(f"  Families with 1 member:  {sum(1 for s in family_sizes if s == 1)}")
    print(f"  Families with 2+ members: {len(multi_member)}")
    if family_sizes:
        print(f"  Largest family:          {max(family_sizes)} documents")

    # ── Per-company impact ────────────────────────────────────────────────
    # Count how many documents each company keeps after family dedup
    before_by_company = defaultdict(int)
    after_by_company = defaultdict(int)

    for p in all_deduped:
        company = company_for_patent[p.get("lens_id", "")]
        before_by_company[company] += 1

    for family_members in families.values():
        rep = pick_representative(family_members)
        company = company_for_patent[rep.get("lens_id", "")]
        after_by_company[company] += 1

    print(f"\n\n  Impact of simple family deduplication:\n")
    print(f"    {'Company':<14} {'Before':>8} {'After':>8} {'Removed':>8} {'% Removed':>10}")
    print(f"    {'─' * 48}")

    total_before = 0
    total_after = 0
    for company in COMPANIES:
        before = before_by_company.get(company, 0)
        after = after_by_company.get(company, 0)
        removed = before - after
        pct = removed / before * 100 if before else 0
        print(f"    {company:<14} {before:>8} {after:>8} {removed:>8} {pct:>9.1f}%")
        total_before += before
        total_after += after

    total_removed = total_before - total_after
    total_pct = total_removed / total_before * 100 if total_before else 0
    print(f"    {'─' * 48}")
    print(f"    {'TOTAL':<14} {total_before:>8} {total_after:>8} {total_removed:>8} {total_pct:>9.1f}%")

    # ── Show some multi-member families as examples ───────────────────────
    print(f"\n\n  Example families with multiple documents:\n")
    examples = sorted(multi_member, key=len, reverse=True)[:5]
    for i, members in enumerate(examples, 1):
        rep = pick_representative(members)
        title = "No title"
        titles = rep.get("biblio", {}).get("invention_title", [])
        if titles:
            title = titles[0].get("text", "No title")

        print(f"    Family {i} ({len(members)} documents): {title[:70]}")
        for m in members:
            lid = m.get("lens_id", "")
            pub_type = m.get("publication_type", "?")
            date = m.get("date_published", "?")
            jurisdiction = m.get("jurisdiction", "?")
            company = company_for_patent.get(lid, "?")
            print(f"      {lid}  {jurisdiction:<4} {pub_type:<22} {date}  [{company}]")
        print()


if __name__ == "__main__":
    main()
