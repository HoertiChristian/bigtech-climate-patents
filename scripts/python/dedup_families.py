"""
Deduplicate Y02 patents by simple patent family.

Keeps one representative per family (prefers granted, then earliest date).
Outputs deduplicated JSON and a summary CSV for downstream analysis.

Run from: scripts/python/
Usage:    python dedup_families.py

Reads:    data/[Company]_patents.json
Writes:   data/patents_deduped.json
          data/patents_deduped_summary.csv
"""

import json
import csv
from pathlib import Path
from collections import defaultdict

DATA_DIR = Path(__file__).resolve().parent / "data"
COMPANIES = ["Alphabet", "Amazon", "Apple", "Meta", "Microsoft"]


def get_simple_family_key(patent: dict) -> str:
    """
    Build a unique key for the simple family.
    Uses sorted member lens_ids. Falls back to the patent's own lens_id.
    """
    families = patent.get("families", {})
    if families:
        simple = families.get("simple_family", {})
        members = simple.get("members", [])
        member_ids = sorted(
            m.get("lens_id", "") for m in members if m.get("lens_id")
        )
        if member_ids:
            return "|".join(member_ids)
    return patent.get("lens_id", "")


def pick_representative(patents: list[dict]) -> dict:
    """
    From a family group, pick the best representative.
    Priority: granted > application > other, then earliest publication date.
    """
    type_priority = {"GRANTED_PATENT": 0, "PATENT_APPLICATION": 1}

    def sort_key(p):
        pub_type = p.get("publication_type", "UNKNOWN")
        return (type_priority.get(pub_type, 2), p.get("date_published", "9999"))

    return sorted(patents, key=sort_key)[0]


def get_title(patent: dict) -> str:
    titles = patent.get("biblio", {}).get("invention_title", [])
    if titles:
        return titles[0].get("text", "No title")
    return "No title"


def get_applicants(patent: dict) -> list[str]:
    applicants = patent.get("biblio", {}).get("parties", {}).get("applicants", [])
    names = []
    for a in applicants:
        for field in ["extracted_name", "applicant_name"]:
            extracted = a.get(field, {})
            if isinstance(extracted, dict):
                name = extracted.get("value", "") or extracted.get("last_name", "")
                if name:
                    names.append(name)
                    break
            elif isinstance(extracted, list):
                for e in extracted:
                    name = e.get("value", "") or e.get("last_name", "")
                    if name:
                        names.append(name)
                break
    return names


def extract_climate_codes(patent: dict) -> list[str]:
    codes = []
    cpc = patent.get("biblio", {}).get("classifications_cpc", {})
    for c in cpc.get("classifications", []):
        symbol = c.get("symbol", "")
        if symbol.startswith("Y02") or symbol.startswith("Y04S"):
            codes.append(symbol)
    return codes


def get_abstract(patent: dict) -> str:
    abstract = patent.get("abstract", "")
    if isinstance(abstract, list) and abstract:
        return abstract[0].get("text", "")
    return abstract if isinstance(abstract, str) else ""


def main():
    # ── Load all patents ──────────────────────────────────────────────────
    all_patents = []
    for company in COMPANIES:
        path = DATA_DIR / f"{company}_patents.json"
        if path.exists():
            with open(path, encoding="utf-8") as f:
                patents = json.load(f)
            for p in patents:
                p["_company"] = company
            all_patents.extend(patents)
            print(f"  Loaded {company}: {len(patents)}")
        else:
            print(f"  {path.name} not found — skipping")

    print(f"\n  Total loaded: {len(all_patents)}")

    # ── Deduplicate by lens_id ────────────────────────────────────────────
    seen = {}
    for p in all_patents:
        lid = p.get("lens_id", "")
        if lid and lid not in seen:
            seen[lid] = p
    patents = list(seen.values())
    lid_dupes = len(all_patents) - len(patents)
    print(f"  After lens_id dedup: {len(patents)} ({lid_dupes} duplicates removed)")

    # ── Check family data ─────────────────────────────────────────────────
    has_family = sum(1 for p in patents if p.get("families"))
    print(f"  Patents with family data: {has_family}/{len(patents)}")
    if has_family == 0:
        print("\n  ⚠  No family data found. Add 'families' to INCLUDE_FIELDS")
        print("     and re-run the API query.")
        return

    # ── Group by simple family ────────────────────────────────────────────
    families: dict[str, list[dict]] = defaultdict(list)
    for p in patents:
        key = get_simple_family_key(p)
        families[key].append(p)

    deduped = [pick_representative(group) for group in families.values()]
    family_dupes = len(patents) - len(deduped)

    print(f"  After family dedup:  {len(deduped)} ({family_dupes} family duplicates removed)")

    # ── Per-company impact ────────────────────────────────────────────────
    before_by_company = defaultdict(int)
    after_by_company = defaultdict(int)

    for p in patents:
        before_by_company[p["_company"]] += 1
    for p in deduped:
        after_by_company[p["_company"]] += 1

    print(f"\n  {'Company':<14} {'Before':>8} {'After':>8} {'Removed':>8} {'% Removed':>10}")
    print(f"  {'─' * 48}")
    for company in COMPANIES:
        b = before_by_company.get(company, 0)
        a = after_by_company.get(company, 0)
        r = b - a
        pct = r / b * 100 if b else 0
        print(f"  {company:<14} {b:>8} {a:>8} {r:>8} {pct:>9.1f}%")

    total_b = sum(before_by_company.values())
    total_a = sum(after_by_company.values())
    total_r = total_b - total_a
    total_pct = total_r / total_b * 100 if total_b else 0
    print(f"  {'─' * 48}")
    print(f"  {'TOTAL':<14} {total_b:>8} {total_a:>8} {total_r:>8} {total_pct:>9.1f}%")

    # ── Save deduplicated JSON ────────────────────────────────────────────
    output = []
    for p in deduped:
        clean = {k: v for k, v in p.items() if not k.startswith("_")}
        clean["_company"] = p["_company"]
        output.append(clean)

    json_path = DATA_DIR / "patents_deduped.json"
    with open(json_path, "w", encoding="utf-8") as f:
        json.dump(output, f, indent=2, ensure_ascii=False)
    print(f"\n  Saved deduplicated JSON → {json_path}")

    # ── Save summary CSV ──────────────────────────────────────────────────
    csv_path = DATA_DIR / "patents_deduped_summary.csv"
    with open(csv_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=[
            "parent_company", "lens_id", "date_published",
            "publication_type", "patent_status",
            "title", "applicants", "climate_codes", "abstract",
        ])
        writer.writeheader()
        for p in deduped:
            status = (p.get("legal_status", {}) or {}).get("patent_status", "")
            writer.writerow({
                "parent_company": p.get("_company", ""),
                "lens_id": p.get("lens_id", ""),
                "date_published": p.get("date_published", ""),
                "publication_type": p.get("publication_type", ""),
                "patent_status": status,
                "title": get_title(p),
                "applicants": "; ".join(get_applicants(p)),
                "climate_codes": "; ".join(extract_climate_codes(p)),
                "abstract": get_abstract(p)[:500],
            })
    print(f"  Saved summary CSV    → {csv_path}")

    # ── Year-by-year counts (for cross-checking with total_patent_counts) ─
    print(f"\n  Deduplicated Y02 patents per company per year:\n")
    year_counts: dict[str, dict[int, int]] = defaultdict(lambda: defaultdict(int))
    for p in deduped:
        date = p.get("date_published", "")
        if date and len(date) >= 4:
            year = int(date[:4])
            if 2010 <= year <= 2024:
                year_counts[p["_company"]][year] += 1

    years = range(2010, 2025)
    header = f"  {'Company':<12}" + "".join(f"{y:>6}" for y in years)
    print(header)
    print(f"  {'─' * (12 + 6 * 15)}")
    for company in COMPANIES:
        row = f"  {company:<12}"
        for y in years:
            row += f"{year_counts[company][y]:>6}"
        print(row)

    print(f"\n  Done.")


if __name__ == "__main__":
    main()
