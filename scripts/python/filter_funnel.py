"""
Three-Step Climate Patent Filtering Funnel
==========================================

Based on methodologies from:
  - Angelucci, Hurtado-Albir & Volpe (2018) — Y02/Y04 scheme design
  - Dechezlepretre et al. (2020) — dual-purpose patents, family-size quality proxy
  - Hötte & Jee (2022) — NPL citation intensity as science-based innovation signal

Steps:
  1. Structural Tag Filter — classify patents as "Pure" vs "Dual-Purpose"
  2. NPL Citation Filter — for non-pure patents, require ≥1 scientific citation
  3. International Family Size Filter — require filings in 2+ jurisdictions

No family deduplication — all documents are kept individually.

Run from: scripts/python/
Usage:    python filter_funnel.py

Reads:    data/[Company]_patents.json
Writes:   data/filtered_patents.json
          data/filter_report.csv
"""

import json
import re
import csv
from pathlib import Path
from collections import Counter

DATA_DIR = Path(__file__).resolve().parent / "data"
COMPANIES = ["alphabet", "amazon", "apple", "meta", "microsoft"]

# ─── CPC tag sets for Step 1 ─────────────────────────────────────────────────

# "Pure" climate Y02 prefixes — kept unconditionally
PURE_PREFIXES = [
    "Y02C",       # Carbon capture & storage
    "Y02E10",     # Renewables (wind, solar, hydro, geothermal, ocean)
    "Y02A",       # Adaptation to climate change
    "Y02W",       # Waste management
]

# "Dual-purpose" Y02 prefixes — high noise, need NPL + family filtering
DUAL_PURPOSE_PREFIXES = [
    "Y02D",       # ICT for reduced energy consumption
    "Y02B",       # Buildings (energy efficiency)
]

# Standard consumer/IT hardware CPC codes that signal dual-purpose intent
HARDWARE_TAGS = [
    "G06F",       # Computing
    "H04L",       # Networking / transmission
    "H04W",       # Wireless communication
    "G06N",       # Computer systems based on specific models
    "G06Q",       # Business methods
    "H05K",       # Printed circuits / casings
    "H01M",       # Batteries (often generic consumer electronics)
]


# ─── Helper functions ─────────────────────────────────────────────────────────

def extract_all_cpc_codes(patent: dict) -> list[str]:
    """Extract all CPC classification codes from a patent."""
    cpc = patent.get("biblio", {}).get("classifications_cpc", {})
    return [c.get("symbol", "") for c in cpc.get("classifications", []) if c.get("symbol")]


def extract_y_codes(patent: dict) -> list[str]:
    """Extract only Y02/Y04S codes."""
    return [c for c in extract_all_cpc_codes(patent)
            if c.startswith("Y02") or c.startswith("Y04S")]


def get_npl_count(patent: dict) -> int:
    """Get the number of non-patent literature citations."""
    refs = patent.get("biblio", {}).get("references_cited", {})
    if not refs:
        return 0
    npl = refs.get("npl_resolved_count") or refs.get("npl_count")
    if npl is not None:
        return int(npl)
    citations = refs.get("citations", [])
    return sum(1 for c in citations if c.get("nplcit"))


def get_family_jurisdictions(patent: dict) -> set[str]:
    """Get unique jurisdictions from simple patent family members."""
    families = patent.get("families", {})
    if not families:
        j = patent.get("jurisdiction", "")
        return {j} if j else set()

    simple = families.get("simple_family", {})
    members = simple.get("members", [])

    jurisdictions = set()
    for m in members:
        doc_id = m.get("document_id", {})
        j = doc_id.get("jurisdiction", "")
        if j:
            jurisdictions.add(j)

    if not jurisdictions:
        j = patent.get("jurisdiction", "")
        if j:
            jurisdictions.add(j)

    return jurisdictions


def get_title(patent: dict) -> str:
    titles = patent.get("biblio", {}).get("invention_title", [])
    if titles:
        return titles[0].get("text", "No title")
    return "No title"


def classify_patent(patent: dict) -> str:
    """
    Step 1: Classify a patent as 'pure', 'dual_purpose', or 'other'.

    Pure:         Has Y02C, Y02E10, Y02A, or Y02W codes
    Dual-purpose: Has Y02D or Y02B AND standard hardware/IT tags
    Other:        Any remaining Y02 patent (Y02E non-10, Y02P, Y02T, Y04S, etc.)
    """
    y_codes = extract_y_codes(patent)
    all_cpc = extract_all_cpc_codes(patent)
    non_y_cpc = [c for c in all_cpc if not c.startswith("Y")]

    for code in y_codes:
        for prefix in PURE_PREFIXES:
            if code.startswith(prefix):
                return "pure"

    has_dual_y = False
    for code in y_codes:
        for prefix in DUAL_PURPOSE_PREFIXES:
            if code.startswith(prefix):
                has_dual_y = True
                break

    if has_dual_y:
        has_hardware = any(
            c.startswith(tag) for c in non_y_cpc for tag in HARDWARE_TAGS
        )
        if has_hardware:
            return "dual_purpose"

    return "other"


# ─── Main pipeline ────────────────────────────────────────────────────────────

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

    # ── Deduplicate by lens_id only ───────────────────────────────────────
    seen = {}
    for p in all_patents:
        lid = p.get("lens_id", "")
        if lid and lid not in seen:
            seen[lid] = p
    patents = list(seen.values())
    print(f"  After lens_id dedup: {len(patents)}")

    # ── Remove discontinued patents ───────────────────────────────────────
    before = len(patents)
    patents = [
        p for p in patents
        if (p.get("legal_status", {}) or {}).get("patent_status", "").upper()
        != "DISCONTINUED"
    ]
    print(f"  After removing discontinued: {len(patents)} (-{before - len(patents)})")

    total_input = len(patents)
    print(f"\n{'=' * 70}")
    print(f"  THREE-STEP FILTERING FUNNEL")
    print(f"  Input: {total_input} unique, non-discontinued patents")
    print(f"{'=' * 70}\n")

    # ══════════════════════════════════════════════════════════════════════
    # STEP 1: Structural Tag Filter
    # ══════════════════════════════════════════════════════════════════════
    classification = {"pure": [], "dual_purpose": [], "other": []}
    for p in patents:
        cat = classify_patent(p)
        p["_filter_class"] = cat
        classification[cat].append(p)

    print(f"  STEP 1 — Structural Tag Classification")
    print(f"  ─────────────────────────────────────────")
    for cat in ["pure", "dual_purpose", "other"]:
        n = len(classification[cat])
        pct = n / total_input * 100 if total_input else 0
        print(f"    {cat:<16} {n:>6}  ({pct:5.1f}%)")

    print(f"\n    By company:")
    print(f"    {'Company':<14} {'Pure':>8} {'Dual-Purp':>10} {'Other':>8} {'Total':>8}")
    print(f"    {'─' * 48}")
    for company in COMPANIES:
        by_cat = Counter(
            p["_filter_class"] for p in patents if p.get("_company") == company
        )
        total = sum(by_cat.values())
        print(f"    {company:<14} {by_cat['pure']:>8} {by_cat['dual_purpose']:>10} "
              f"{by_cat['other']:>8} {total:>8}")

    print(f"\n    Y02 subclass distribution in 'dual_purpose' patents:")
    dp_y_dist = Counter()
    for p in classification["dual_purpose"]:
        for code in extract_y_codes(p):
            m = re.match(r"(Y\d{2}[A-Z])", code)
            if m:
                dp_y_dist[m.group(1)] += 1
    for sub, count in dp_y_dist.most_common():
        print(f"      {sub}: {count}")

    # ══════════════════════════════════════════════════════════════════════
    # STEP 2: NPL Citation Filter (applied to dual_purpose and other)
    # ══════════════════════════════════════════════════════════════════════
    step1_kept = list(classification["pure"])

    step2_candidates = classification["dual_purpose"] + classification["other"]
    step2_passed = []
    step2_failed = []

    npl_distribution = Counter()
    for p in step2_candidates:
        npl = get_npl_count(p)
        p["_npl_count"] = npl
        npl_distribution[min(npl, 10)] += 1

        if npl >= 1:
            step2_passed.append(p)
            p["_filter_npl"] = "pass"
        else:
            step2_failed.append(p)
            p["_filter_npl"] = "fail"

    print(f"\n\n  STEP 2 — NPL Citation Filter (≥1 citation required)")
    print(f"  ─────────────────────────────────────────")
    print(f"    Candidates (dual_purpose + other): {len(step2_candidates)}")
    print(f"    Passed (≥1 NPL):   {len(step2_passed):>6}")
    print(f"    Failed (0 NPL):    {len(step2_failed):>6}")
    print(f"\n    NPL citation distribution:")
    for npl_count in sorted(npl_distribution.keys()):
        label = f"{npl_count}+" if npl_count == 10 else str(npl_count)
        count = npl_distribution[npl_count]
        bar = "█" * (count // max(1, len(step2_candidates) // 30))
        print(f"      NPL={label:<3}  {count:>6}  {bar}")

    # ══════════════════════════════════════════════════════════════════════
    # STEP 3: International Family Size Filter (2+ jurisdictions)
    # ══════════════════════════════════════════════════════════════════════
    step3_candidates = step2_passed
    step3_passed = []
    step3_failed = []

    jurisdiction_distribution = Counter()
    for p in step3_candidates:
        jurisdictions = get_family_jurisdictions(p)
        n_jurisdictions = len(jurisdictions)
        p["_n_jurisdictions"] = n_jurisdictions
        p["_jurisdictions"] = sorted(jurisdictions)
        jurisdiction_distribution[min(n_jurisdictions, 6)] += 1

        if n_jurisdictions >= 2:
            step3_passed.append(p)
            p["_filter_family"] = "pass"
        else:
            step3_failed.append(p)
            p["_filter_family"] = "fail"

    print(f"\n\n  STEP 3 — International Family Size Filter (≥2 jurisdictions)")
    print(f"  ─────────────────────────────────────────")
    print(f"    Candidates (Step 2 survivors): {len(step3_candidates)}")
    print(f"    Passed (2+ jurisdictions): {len(step3_passed):>6}")
    print(f"    Failed (1 jurisdiction):   {len(step3_failed):>6}")
    print(f"\n    Jurisdiction count distribution:")
    for n in sorted(jurisdiction_distribution.keys()):
        label = f"{n}+" if n == 6 else str(n)
        count = jurisdiction_distribution[n]
        bar = "█" * (count // max(1, len(step3_candidates) // 30))
        print(f"      Jurisdictions={label:<3}  {count:>6}  {bar}")

    # ══════════════════════════════════════════════════════════════════════
    # FINAL RESULTS
    # ══════════════════════════════════════════════════════════════════════
    final_patents = step1_kept + step3_passed
    for p in step1_kept:
        p["_filter_path"] = "pure_auto_keep"
    for p in step3_passed:
        p["_filter_path"] = "filtered_keep"

    print(f"\n\n{'=' * 70}")
    print(f"  FUNNEL SUMMARY")
    print(f"{'=' * 70}")
    print(f"    Input (unique patents):             {total_input:>6}")
    print(f"    Step 1 — Pure (auto-keep):           {len(step1_kept):>6}")
    print(f"    Step 1 — Candidates for filtering:   {len(step2_candidates):>6}")
    print(f"    Step 2 — Survived NPL filter:        {len(step2_passed):>6}")
    print(f"    Step 3 — Survived family filter:      {len(step3_passed):>6}")
    print(f"    ─────────────────────────────────────")
    print(f"    FINAL DATASET:                       {len(final_patents):>6}  "
          f"({len(final_patents)/total_input*100:.1f}% of input)")
    print(f"      Pure climate patents:              {len(step1_kept):>6}")
    print(f"      Science-backed, international:     {len(step3_passed):>6}")

    print(f"\n    Final dataset by company:")
    print(f"    {'Company':<14} {'Pure':>8} {'Filtered':>10} {'Total':>8} {'% of Input':>12}")
    print(f"    {'─' * 52}")
    for company in COMPANIES:
        pure_n = sum(1 for p in step1_kept if p.get("_company") == company)
        filt_n = sum(1 for p in step3_passed if p.get("_company") == company)
        total = pure_n + filt_n
        orig = sum(1 for p in patents if p.get("_company") == company)
        pct = total / orig * 100 if orig else 0
        print(f"    {company:<14} {pure_n:>8} {filt_n:>10} {total:>8} {pct:>10.1f}%")

    # ── Save filtered dataset ─────────────────────────────────────────────
    output_patents = []
    for p in final_patents:
        clean = {k: v for k, v in p.items() if not k.startswith("_")}
        clean["filter_metadata"] = {
            "company": p.get("_company", ""),
            "classification": p.get("_filter_class", ""),
            "filter_path": p.get("_filter_path", ""),
            "npl_count": p.get("_npl_count", None),
            "n_jurisdictions": p.get("_n_jurisdictions", None),
            "jurisdictions": p.get("_jurisdictions", None),
        }
        output_patents.append(clean)

    out_path = DATA_DIR / "filtered_patents.json"
    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(output_patents, f, indent=2, ensure_ascii=False)
    print(f"\n  Saved filtered dataset → {out_path}")

    # ── Save filter report CSV ────────────────────────────────────────────
    report_path = DATA_DIR / "filter_report.csv"
    with open(report_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=[
            "lens_id", "company", "title", "date_published",
            "publication_type", "patent_status",
            "classification", "npl_count", "n_jurisdictions",
            "jurisdictions", "filter_path", "kept",
        ])
        writer.writeheader()

        for p in patents:
            kept = p.get("_filter_path") in ("pure_auto_keep", "filtered_keep")
            status = (p.get("legal_status", {}) or {}).get("patent_status", "")
            writer.writerow({
                "lens_id": p.get("lens_id", ""),
                "company": p.get("_company", ""),
                "title": get_title(p)[:120],
                "date_published": p.get("date_published", ""),
                "publication_type": p.get("publication_type", ""),
                "patent_status": status,
                "classification": p.get("_filter_class", ""),
                "npl_count": p.get("_npl_count", ""),
                "n_jurisdictions": p.get("_n_jurisdictions", ""),
                "jurisdictions": ", ".join(p.get("_jurisdictions", [])),
                "filter_path": p.get("_filter_path", "removed"),
                "kept": kept,
            })

    print(f"  Saved filter report  → {report_path}")

    # ── Sample removed patents (spot-check) ───────────────────────────────
    print(f"\n\n  Sample REMOVED patents (for spot-checking):\n")
    removed = [p for p in patents if p.get("_filter_path") not in ("pure_auto_keep", "filtered_keep")]
    for p in removed[:8]:
        title = get_title(p)[:70]
        y_codes = ", ".join(extract_y_codes(p)[:3])
        npl = p.get("_npl_count", "?")
        nj = p.get("_n_jurisdictions", "?")
        cls = p.get("_filter_class", "?")
        print(f"    [{p.get('_company', '?')}] {title}")
        print(f"      Y-codes: {y_codes}  |  Class: {cls}  |  NPL: {npl}  |  Jurisdictions: {nj}")
        print()


if __name__ == "__main__":
    main()