"""
Lens Patent API — Query Y02/Y04S climate patents for Big Tech firms (2010–2024).

Reads applicant names from ../../subsidiaries.csv (repo root).
Saves results to ./data/ as JSON and a summary CSV.

Usage:
    python lens_patent_query.py YOUR_API_TOKEN

Repo structure:
    BIGTECH-CLIMATE-PATENTS/
    ├── subsidiaries.csv
    ├── scripts/
    │   └── python/
    │       ├── lens_patent_query.py    ← this file
    │       └── data/                   ← output goes here
    └── ...qd

Dependencies (install once):
    pip install requests rapidfuzz

API docs: https://docs.api.lens.org/request-patent.html
Key distinction:
  - SEARCH fields:  class_cpc.symbol, applicant.name, date_published
  - INCLUDE fields: biblio.invention_title, biblio.parties.applicants, biblio.classifications_cpc
  - CPC prefix:     query_string with "class_cpc.symbol:Y02* OR class_cpc.symbol:Y04S*"
"""

import requests
import json
import csv
import sys
import time
from pathlib import Path
from collections import defaultdict

from rapidfuzz import process, fuzz

# ─── Paths ───────────────────────────────────────────────────────────────────

SCRIPT_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parent.parent
CSV_PATH = REPO_ROOT / "subsidiaries.csv"
DATA_DIR = SCRIPT_DIR / "data"

API_URL = "https://api.lens.org/patent/search"

# Fuzzy match threshold (0–100). 70 = permissive — catches more variations
# in entity naming while still filtering out unrelated companies.
FUZZY_THRESHOLD = 70

# Fallback applicant names if CSV is empty or missing
DEFAULT_FIRMS = {
    "Alphabet": ["Google LLC", "Google Inc", "Alphabet Inc"],
    "Amazon": ["Amazon Technologies Inc", "Amazon.com Inc"],
    "Apple": ["Apple Inc"],
    "Meta": ["Meta Platforms Inc", "Facebook Inc"],
    "Microsoft": ["Microsoft Corporation", "Microsoft Technology Licensing"],
}

# Fields to include in the API response (these use the nested biblio structure)
INCLUDE_FIELDS = [
    "lens_id",
    "jurisdiction",
    "date_published",
    "publication_type",
    "legal_status",
    "families",
    "biblio.invention_title",
    "biblio.parties.applicants",
    "biblio.classifications_cpc",
    "biblio.references_cited",
    "abstract",
]


# ─── CSV loading ─────────────────────────────────────────────────────────────

def load_subsidiaries(csv_path: Path) -> dict[str, list[str]]:
    """
    Read subsidiaries.csv → {parent_company: [subsidiary_name, ...]}.
    Expected columns: parent_company, subsidiary_name, source, reason
    Falls back to DEFAULT_FIRMS if missing or empty.
    """
    if not csv_path.exists():
        print(f"  CSV not found at {csv_path}")
        print(f"  → Using default applicant names.\n")
        return DEFAULT_FIRMS

    firms: dict[str, list[str]] = {}
    with open(csv_path, newline="", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f)
        reader.fieldnames = [col.strip().lower().replace(" ", "_") for col in reader.fieldnames]

        for row in reader:
            parent = row.get("parent_company", "").strip()
            subsidiary = row.get("subsidiary_name", "").strip()
            if parent and subsidiary:
                firms.setdefault(parent, []).append(subsidiary)

    if not firms:
        print(f"  CSV at {csv_path} is empty or couldn't be parsed.")
        print(f"  → Using default applicant names.\n")
        return DEFAULT_FIRMS

    return {k: list(dict.fromkeys(v)) for k, v in firms.items()}


# ─── Fuzzy matching ───────────────────────────────────────────────────────────

def build_fuzzy_lookup(firms: dict[str, list[str]]) -> dict[str, str]:
    """
    Build a flat lookup: every known subsidiary/query name → canonical parent.
    Used as the reference list for fuzzy matching of raw API assignee names.

    Returns:
        {query_name_lower: canonical_parent, ...}
    """
    lookup = {}
    for parent, subs in firms.items():
        for s in subs:
            lookup[s.lower()] = parent
    return lookup


def fuzzy_match_assignee(
    raw_name: str,
    firms: dict[str, list[str]],
    threshold: int = FUZZY_THRESHOLD,
) -> str | None:
    """
    Match a raw assignee name returned by the API to a canonical parent company.

    Strategy:
      1. Exact match (case-insensitive) against all known subsidiary names.
      2. If no exact match, use rapidfuzz token_sort_ratio against the same
         list — handles word-order swaps, punctuation, and minor typos.
      3. Return None if the best score is below threshold.

    Args:
        raw_name:  Assignee string from the API (e.g. "Google L.L.C.")
        firms:     {parent: [subsidiary, ...]} from load_subsidiaries()
        threshold: Minimum similarity score (0–100) to accept a match.

    Returns:
        Canonical parent company name, or None if no match found.
    """
    if not raw_name:
        return None

    raw_lower = raw_name.strip().lower()

    # Build flat list of (subsidiary, parent) pairs for matching
    candidates = []  # list of (subsidiary_name, parent_name)
    for parent, subs in firms.items():
        for s in subs:
            candidates.append((s, parent))

    if not candidates:
        return None

    candidate_names = [c[0] for c in candidates]

    # 1. Exact match first (fast path)
    for name, parent in candidates:
        if raw_lower == name.lower():
            return parent

    # 2. Fuzzy match using token_sort_ratio (robust to word-order differences)
    result = process.extractOne(
        raw_name,
        candidate_names,
        scorer=fuzz.token_sort_ratio,
        score_cutoff=threshold,
    )

    if result:
        matched_name, score, idx = result
        return candidates[idx][1]

    return None


def assign_patents_to_parents(
    all_results: dict[str, list[dict]],
    firms: dict[str, list[str]],
) -> tuple[dict[str, set[str]], list[dict]]:
    """
    For every patent in all_results, attempt to confirm/correct its parent
    company assignment using fuzzy matching of raw API assignee names.

    Returns:
        deduplicated:  {canonical_parent: set of unique lens_ids}
        match_log:     list of dicts describing each fuzzy match decision
                       (useful for auditing)
    """
    deduplicated: dict[str, set[str]] = defaultdict(set)
    match_log = []

    for query_parent, patents in all_results.items():
        for patent in patents:
            lens_id = patent.get("lens_id", "")
            raw_assignees = get_applicants(patent)

            matched_parent = None
            matched_assignee = None
            match_score = None

            # Try to fuzzy-match each raw assignee name
            for raw in raw_assignees:
                result = process.extractOne(
                    raw,
                    [s for subs in firms.values() for s in subs],
                    scorer=fuzz.token_sort_ratio,
                    score_cutoff=FUZZY_THRESHOLD,
                )
                if result:
                    matched_name, score, _ = result
                    # Resolve matched subsidiary → parent
                    for parent, subs in firms.items():
                        if matched_name in subs:
                            matched_parent = parent
                            matched_assignee = raw
                            match_score = score
                            break
                if matched_parent:
                    break

            # Fall back to the query-time parent if no fuzzy match
            final_parent = matched_parent if matched_parent else query_parent

            if lens_id:
                deduplicated[final_parent].add(lens_id)

            match_log.append({
                "lens_id": lens_id,
                "query_parent": query_parent,
                "raw_assignees": "; ".join(raw_assignees),
                "fuzzy_matched_assignee": matched_assignee or "",
                "fuzzy_match_score": match_score or "",
                "resolved_parent": final_parent,
                "reassigned": final_parent != query_parent,
            })

    return deduplicated, match_log


# ─── API query builders ──────────────────────────────────────────────────────

def build_query(applicant_names: list[str], size: int = 50,
                cpc_filter: bool = True) -> dict:
    """
    Build a Lens API query for patents by applicant, 2010–2024.

    Args:
        applicant_names: List of subsidiary/entity names to search.
        size:            Number of results per page.
        cpc_filter:      If True, restrict to Y02/Y04S climate patents.
                         If False, match all patents (for total counts).

    Uses the correct SEARCH field names:
        - applicant.name  (not biblio.parties.applicants.extracted_name.value)
        - class_cpc.symbol via query_string wildcard (not match on Y02)
        - date_published range
    """
    must_clauses = [
        # Applicant: match any subsidiary name
        {
            "bool": {
                "should": [
                    {"match_phrase": {"applicant.name": name}}
                    for name in applicant_names
                ]
            }
        },
        # Date range 2010–2024
        {
            "range": {
                "date_published": {
                    "gte": "2010-01-01",
                    "lte": "2025-12-31",
                }
            }
        },
    ]

    # Optionally add CPC classification filter (Y02 + Y04S)
    if cpc_filter:
        must_clauses.append({
            "query_string": {
                "query": "class_cpc.symbol:Y02* OR class_cpc.symbol:Y04S*"
            }
        })

    query = {
        "query": {
            "bool": {
                "must": must_clauses,
            }
        },
        "include": INCLUDE_FIELDS,
        "size": size,
        "sort": [{"date_published": "desc"}],
        "scroll": "1m",
    }

    return query


def build_count_query(applicant_names: list[str], year: int) -> dict:
    """
    Build a lightweight Lens API query to get the total patent count
    for a company in a single year (no CPC filter, no full data).

    Returns size=1 so we only need the 'total' field from the response.
    """
    return {
        "query": {
            "bool": {
                "must": [
                    {
                        "bool": {
                            "should": [
                                {"match_phrase": {"applicant.name": name}}
                                for name in applicant_names
                            ]
                        }
                    },
                    {
                        "range": {
                            "date_published": {
                                "gte": f"{year}-01-01",
                                "lte": f"{year}-12-31",
                            }
                        }
                    },
                ],
            }
        },
        "include": ["lens_id"],
        "size": 1,
    }


def search_patents(token: str, query: dict) -> dict:
    """Execute a POST request against the Lens Patent API."""
    headers = {
        "Authorization": f"Bearer {token}",
        "Content-Type": "application/json",
    }
    response = requests.post(API_URL, headers=headers, json=query)

    if response.status_code == 200:
        return response.json()
    elif response.status_code == 204:
        return {"data": [], "total": 0}
    elif response.status_code == 401:
        print("ERROR 401: Unauthorized — check your API token.")
        sys.exit(1)
    elif response.status_code == 429:
        print("\n  Rate limited — waiting 10s...", flush=True)
        time.sleep(10)
        return search_patents(token, query)
    else:
        print(f"\nERROR {response.status_code}: {response.text[:500]}")
        sys.exit(1)


def fetch_all_patents(token: str, applicant_names: list[str]) -> list[dict]:
    """Fetch all matching patents using scroll-based pagination."""
    all_patents = []

    query = build_query(applicant_names, size=50)
    result = search_patents(token, query)
    total = result.get("total", 0)
    scroll_id = result.get("scroll_id")
    batch = result.get("data", [])
    all_patents.extend(batch)

    print(f"    Total matching: {total} | Fetched: {len(all_patents)}", end="", flush=True)

    while scroll_id and len(all_patents) < total:
        scroll_query = {
            "scroll_id": scroll_id,
            "include": INCLUDE_FIELDS,
        }
        result = search_patents(token, scroll_query)
        batch = result.get("data", [])
        scroll_id = result.get("scroll_id")

        if not batch:
            break

        all_patents.extend(batch)
        print(f"\r    Total matching: {total} | Fetched: {len(all_patents)}", end="", flush=True)

    print()
    return all_patents


def fetch_total_patent_counts(
    token: str, firms: dict[str, list[str]]
) -> list[dict]:
    """
    For each company, query the total number of patents (no CPC filter)
    for each year from 2010 to 2024. Only retrieves the count, not the
    patent data itself.

    Returns:
        List of dicts: [{company, year, total_patents}, ...]
    """
    rows = []
    years = range(2010, 2025)

    for parent, subs in firms.items():
        print(f"  [{parent}] ", end="", flush=True)
        for year in years:
            query = build_count_query(subs, year)
            result = search_patents(token, query)
            total = result.get("total", 0)
            rows.append({
                "company": parent,
                "year": year,
                "total_patents": total,
            })
            print(".", end="", flush=True)
        print(f" done")

    return rows


# ─── Data extraction helpers ─────────────────────────────────────────────────

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
    """Extract Y02 and Y04S CPC codes from a patent."""
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


# ─── Reporting ────────────────────────────────────────────────────────────────

def print_patent_count_table(
    raw_counts: dict[str, int],
    deduplicated: dict[str, set[str]],
) -> None:
    """
    Print a formatted table comparing:
      - Raw patents fetched per API call (one call per canonical parent)
      - Deduplicated patent count per canonical company after fuzzy matching

    Deduplication removes:
      1. Patents with the same lens_id fetched under multiple query names
      2. Patents reassigned to a different parent after fuzzy matching
    """
    col_w = [22, 14, 18, 12]
    header = (
        f"{'Company':<{col_w[0]}}"
        f"{'Raw (API)':<{col_w[1]}}"
        f"{'Dedup (unique)':<{col_w[2]}}"
        f"{'Δ Removed':<{col_w[3]}}"
    )
    divider = "─" * sum(col_w)

    print()
    print("  Patent Count Summary (Y02 + Y04S)")
    print("  " + divider)
    print("  " + header)
    print("  " + divider)

    total_raw = 0
    total_dedup = 0

    all_parents = sorted(set(list(raw_counts.keys()) + list(deduplicated.keys())))
    for parent in all_parents:
        raw = raw_counts.get(parent, 0)
        dedup = len(deduplicated.get(parent, set()))
        removed = raw - dedup
        total_raw += raw
        total_dedup += dedup
        print(
            f"  {parent:<{col_w[0]}}"
            f"{raw:<{col_w[1]}}"
            f"{dedup:<{col_w[2]}}"
            f"{removed:<{col_w[3]}}"
        )

    print("  " + divider)
    total_removed = total_raw - total_dedup
    print(
        f"  {'TOTAL':<{col_w[0]}}"
        f"{total_raw:<{col_w[1]}}"
        f"{total_dedup:<{col_w[2]}}"
        f"{total_removed:<{col_w[3]}}"
    )
    print("  " + divider)
    print()


# ─── Main ────────────────────────────────────────────────────────────────────

def main():
    if len(sys.argv) < 2:
        print("Usage: python lens_patent_query.py YOUR_API_TOKEN")
        sys.exit(1)

    token = sys.argv[1]

    print("=" * 60)
    print("  Big Tech Climate Patents — Lens API Query")
    print("  CPC class Y02* + Y04S* | 2010–2024")
    print("=" * 60)
    print()

    # ── Load subsidiary names ─────────────────────────────────────────────
    print("Loading subsidiaries...")
    firms = load_subsidiaries(CSV_PATH)
    for parent, subs in firms.items():
        print(f"  {parent}: {len(subs)} entities → {subs[:3]}{'...' if len(subs) > 3 else ''}")
    print()

    # Create output directory
    DATA_DIR.mkdir(parents=True, exist_ok=True)

    # ── Query climate patents (Y02 + Y04S) per firm ───────────────────────
    print("Fetching climate patents (Y02 + Y04S)...")
    all_results: dict[str, list[dict]] = {}
    raw_counts: dict[str, int] = {}
    summary_rows = []

    for parent, subs in firms.items():
        print(f"  [{parent}]")
        patents = fetch_all_patents(token, subs)
        all_results[parent] = patents
        raw_counts[parent] = len(patents)

        # Y02/Y04S sub-class distribution
        subclass_counts: dict[str, int] = {}
        for p in patents:
            for code in extract_climate_codes(p):
                prefix = code[:4]
                subclass_counts[prefix] = subclass_counts.get(prefix, 0) + 1

        for p in patents:
            summary_rows.append({
                "parent_company": parent,
                "lens_id": p.get("lens_id", ""),
                "date_published": p.get("date_published", ""),
                "title": get_title(p),
                "applicants": "; ".join(get_applicants(p)),
                "climate_codes": "; ".join(extract_climate_codes(p)),
                "abstract": get_abstract(p)[:500],
            })

        if subclass_counts:
            print(f"    Y02/Y04S breakdown: {dict(sorted(subclass_counts.items()))}")
        print()

    # ── Fuzzy matching & deduplication ────────────────────────────────────
    print("Running fuzzy matching & deduplication...")
    print(f"  Threshold: {FUZZY_THRESHOLD}% (token_sort_ratio)")
    deduplicated, match_log = assign_patents_to_parents(all_results, firms)

    reassigned = [r for r in match_log if r["reassigned"]]
    if reassigned:
        print(f"  Reassigned {len(reassigned)} patent(s) to a different parent after fuzzy matching:")
        for r in reassigned[:10]:   # show up to 10 examples
            print(f"    lens_id={r['lens_id']} | '{r['raw_assignees']}'"
                  f" → {r['query_parent']} → {r['resolved_parent']}"
                  f" (score={r['fuzzy_match_score']})")
        if len(reassigned) > 10:
            print(f"    ... and {len(reassigned) - 10} more (see fuzzy_match_log.csv)")
    else:
        print("  No reassignments — all patents confirmed under their query parent.")
    print()

    # ── Print patent count table ──────────────────────────────────────────
    print_patent_count_table(raw_counts, deduplicated)

    # ── Fetch total patent counts per company per year ────────────────────
    print("Fetching total patent counts per company per year (no CPC filter)...")
    total_counts = fetch_total_patent_counts(token, firms)
    print()

    # ── Save outputs ──────────────────────────────────────────────────────

    # Raw JSON
    json_path = DATA_DIR / "patents_raw.json"
    with open(json_path, "w", encoding="utf-8") as f:
        json.dump(all_results, f, indent=2, ensure_ascii=False)
    print(f"Saved raw JSON         → {json_path}")

    # Summary CSV (one row per patent, pre-dedup)
    csv_path = DATA_DIR / "patents_summary.csv"
    if summary_rows:
        fieldnames = ["parent_company", "lens_id", "date_published", "title",
                      "applicants", "climate_codes", "abstract"]
        with open(csv_path, "w", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            writer.writerows(summary_rows)
    print(f"Saved summary CSV      → {csv_path}")

    # Patent count table CSV (climate patents only)
    count_path = DATA_DIR / "patent_counts.csv"
    all_parents = sorted(set(list(raw_counts.keys()) + list(deduplicated.keys())))
    with open(count_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["company", "raw_api_count",
                                               "deduplicated_count", "removed"])
        writer.writeheader()
        for parent in all_parents:
            raw = raw_counts.get(parent, 0)
            dedup = len(deduplicated.get(parent, set()))
            writer.writerow({
                "company": parent,
                "raw_api_count": raw,
                "deduplicated_count": dedup,
                "removed": raw - dedup,
            })
    print(f"Saved patent counts    → {count_path}")

    # Total patent counts per company per year (no CPC filter)
    total_count_path = DATA_DIR / "total_patent_counts.csv"
    with open(total_count_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["company", "year", "total_patents"])
        writer.writeheader()
        writer.writerows(total_counts)
    print(f"Saved total counts     → {total_count_path}")

    # Fuzzy match audit log
    log_path = DATA_DIR / "fuzzy_match_log.csv"
    if match_log:
        with open(log_path, "w", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=match_log[0].keys())
            writer.writeheader()
            writer.writerows(match_log)
    print(f"Saved fuzzy match log  → {log_path}")

    print()
    print("Done.")


if __name__ == "__main__":
    main()