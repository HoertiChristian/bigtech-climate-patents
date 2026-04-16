"""
Quick test: compare total patent counts with/without group_by SIMPLE_FAMILY.
Only queries year 2020 for each company.

Usage: python test_family_grouping.py YOUR_API_TOKEN
"""

import requests
import csv
import sys
import time
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parent.parent
CSV_PATH = REPO_ROOT / "subsidiaries.csv"
API_URL = "https://api.lens.org/patent/search"

DEFAULT_FIRMS = {
    "Alphabet": ["Google LLC", "Google Inc", "Alphabet Inc"],
    "Amazon": ["Amazon Technologies Inc", "Amazon.com Inc"],
    "Apple": ["Apple Inc"],
    "Meta": ["Meta Platforms Inc", "Facebook Inc"],
    "Microsoft": ["Microsoft Corporation", "Microsoft Technology Licensing"],
}


def load_subsidiaries(csv_path):
    if not csv_path.exists():
        return DEFAULT_FIRMS
    firms = {}
    with open(csv_path, newline="", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f)
        reader.fieldnames = [col.strip().lower().replace(" ", "_") for col in reader.fieldnames]
        for row in reader:
            parent = row.get("parent_company", "").strip()
            sub = row.get("subsidiary_name", "").strip()
            if parent and sub:
                firms.setdefault(parent, []).append(sub)
    return firms if firms else DEFAULT_FIRMS


def query_count(token, applicant_names, year, group_by_family=False):
    query = {
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
        "size": 100,
    }
    if group_by_family:
        query["group_by"] = "SIMPLE_FAMILY"

    headers = {
        "Authorization": f"Bearer {token}",
        "Content-Type": "application/json",
    }
    response = requests.post(API_URL, headers=headers, json=query)

    remaining = response.headers.get("x-rate-limit-remaining-request-per-minute", "?")
    print(f"      [rate limit: {remaining} requests/min remaining]")

    data = response.json()
    print(f"total={data.get('total')}, actual_data_len={len(data.get('data', []))}")

    if response.status_code == 200:
        return response.json().get("total", 0)
    elif response.status_code == 429:
        print("      Rate limited — waiting 15s...")
        time.sleep(15)
        return query_count(token, applicant_names, year, group_by_family)
    else:
        print(f"      ERROR {response.status_code}: {response.text[:200]}")
        return -1


def main():
    if len(sys.argv) < 2:
        print("Usage: python test_family_grouping.py YOUR_API_TOKEN")
        sys.exit(1)

    token = sys.argv[1]
    firms = load_subsidiaries(CSV_PATH)
    year = 2020

    print(f"  Comparing patent counts for {year}: raw documents vs. simple family\n")
    print(f"  {'Company':<14} {'Raw docs':>10} {'Family':>10} {'Diff':>8} {'% Removed':>10}")
    print(f"  {'─' * 52}")

    for parent, subs in firms.items():
        print(f"  Querying {parent}...")
        raw = query_count(token, subs, year, group_by_family=False)
        time.sleep(2)  # be gentle with rate limit
        family = query_count(token, subs, year, group_by_family=True)
        time.sleep(2)

        diff = raw - family
        pct = diff / raw * 100 if raw > 0 else 0
        print(f"  {parent:<14} {raw:>10} {family:>10} {diff:>8} {pct:>9.1f}%\n")


if __name__ == "__main__":
    main()
