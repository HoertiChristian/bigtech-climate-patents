"""
SEC Exhibit 21.1 Subsidiary Scraper
====================================
Extracts subsidiary names from all 10-K Exhibit 21.1 filings
(2010–present) for Alphabet, Amazon, Apple, Meta, and Microsoft
using the free SEC EDGAR API (no API key required).

Deduplicates across filing years and strips commas from all fields
so the CSV output contains no quoting.

Outputs: subsidiaries_sec.csv, subsidiaries_sec_compat.csv
Columns: parent_company, subsidiary_name, jurisdiction, source

Usage:
    python sec_exhibit21_scraper.py

Requirements:
    pip install requests beautifulsoup4 lxml pandas
"""

import requests
import time
import re
import json
import csv
import logging
from pathlib import Path
from bs4 import BeautifulSoup, NavigableString
import pandas as pd

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

# SEC EDGAR requires a descriptive User-Agent header.
# Replace with your own name and email before running.
HEADERS = {
    "User-Agent": "YourName YourEmail@example.com",
    "Accept-Encoding": "gzip, deflate",
}

# Rate-limit: SEC asks for ≤10 requests/second; we stay well below.
REQUEST_DELAY = 0.2  # seconds between requests

# Companies to scrape  (CIK must be zero-padded to 10 digits)
COMPANIES = {
    "Alphabet": {"cik": "0001652044", "ticker": "GOOGL"},
    "Amazon":   {"cik": "0001018724", "ticker": "AMZN"},
    "Apple":    {"cik": "0000320193", "ticker": "AAPL"},
    "Meta":     {"cik": "0001326801", "ticker": "META"},
    "Microsoft":{"cik": "0000789019", "ticker": "MSFT"},
}

OUTPUT_FILE = "subsidiaries_sec.csv"

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s  %(levelname)-8s  %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Helper: throttled GET
# ---------------------------------------------------------------------------

def get(url: str) -> requests.Response:
    """GET with rate-limiting and error handling."""
    time.sleep(REQUEST_DELAY)
    resp = requests.get(url, headers=HEADERS, timeout=30)
    resp.raise_for_status()
    return resp

# Year cutoff — collect all 10-K filings from this year onward
START_YEAR = 2010

# ---------------------------------------------------------------------------
# Step 1: Find all 10-K filings for a company back to START_YEAR
# ---------------------------------------------------------------------------

def _extract_10ks(forms, accessions, dates, primary_docs) -> list[dict]:
    """Pick 10-K / 10-K/A entries filed on or after START_YEAR."""
    results = []
    for i, form in enumerate(forms):
        if form not in ("10-K", "10-K/A"):
            continue
        filing_date = dates[i]
        if int(filing_date[:4]) < START_YEAR:
            continue
        results.append({
            "form": form,
            "accessionNumber": accessions[i],
            "filingDate": filing_date,
            "primaryDocument": primary_docs[i],
        })
    return results


def get_all_10ks(cik: str) -> list[dict]:
    """
    Query the EDGAR submissions API and return metadata for every
    10-K filing from START_YEAR onward.
    """
    url = f"https://data.sec.gov/submissions/CIK{cik}.json"
    log.info(f"  Fetching submissions: {url}")
    data = get(url).json()

    recent = data.get("filings", {}).get("recent", {})
    filings = _extract_10ks(
        recent.get("form", []),
        recent.get("accessionNumber", []),
        recent.get("filingDate", []),
        recent.get("primaryDocument", []),
    )

    # Check older filing pages for filings before the recent window
    older_files = data.get("filings", {}).get("files", [])
    for file_ref in older_files:
        older_url = f"https://data.sec.gov/submissions/{file_ref['name']}"
        older_data = get(older_url).json()
        filings.extend(_extract_10ks(
            older_data.get("form", []),
            older_data.get("accessionNumber", []),
            older_data.get("filingDate", []),
            older_data.get("primaryDocument", []),
        ))

    # Sort newest first
    filings.sort(key=lambda f: f["filingDate"], reverse=True)
    return filings

# ---------------------------------------------------------------------------
# Step 2: Find the Exhibit 21 document URL within a 10-K filing
# ---------------------------------------------------------------------------

def find_exhibit21_url(cik: str, accession: str) -> str | None:
    """
    Find the Exhibit 21 document URL within a 10-K filing.

    Uses two strategies:
      1. Parse the filing index HTML page (most reliable)
      2. Scan the filing directory listing for filenames matching EX-21
    """
    cik_int = str(int(cik))  # strip leading zeros for URL paths
    acc_no_dashes = accession.replace("-", "")
    base_url = f"https://www.sec.gov/Archives/edgar/data/{cik_int}/{acc_no_dashes}/"

    # --- Strategy 1: Filing index HTML page ---
    index_html_url = f"{base_url}{accession}-index.htm"
    log.info(f"  Fetching filing index: {index_html_url}")
    try:
        resp = get(index_html_url)
        soup = BeautifulSoup(resp.text, "lxml")

        for row in soup.find_all("tr"):
            cells = row.find_all("td")
            if len(cells) < 2:
                continue

            # The SEC index table columns are typically:
            #   Seq | Description | Document | Type | Size
            # but column order can vary; search all cells for EX-21 type
            row_text = " ".join(c.get_text(strip=True) for c in cells).upper()

            is_ex21 = bool(re.search(r"EX-?21", row_text))
            if not is_ex21:
                continue

            # Find the document link in this row
            for cell in cells:
                link = cell.find("a")
                if link and link.get("href"):
                    href = link["href"]
                    # Skip iXBRL viewer links and JSON files
                    if "ix?doc=" in href or href.endswith(".json"):
                        continue
                    # Build absolute URL
                    if href.startswith("/"):
                        return f"https://www.sec.gov{href}"
                    if href.startswith("http"):
                        return href
                    return base_url + href

    except requests.HTTPError as e:
        log.warning(f"  Index HTML not found ({e}), trying directory listing")

    # --- Strategy 2: Directory listing ---
    log.info(f"  Trying directory listing: {base_url}")
    try:
        resp = get(base_url)
        soup = BeautifulSoup(resp.text, "lxml")
        for link in soup.find_all("a", href=True):
            name = link["href"].lower()
            if re.search(r"(ex-?21|dex-?21)", name) and name.endswith((".htm", ".html", ".txt")):
                return base_url + link["href"]
    except Exception as e:
        log.warning(f"  Directory listing failed: {e}")

    return None

# ---------------------------------------------------------------------------
# Step 3: Parse the Exhibit 21 HTML to extract subsidiaries
# ---------------------------------------------------------------------------

def parse_exhibit21(html: str, parent_company: str) -> list[dict]:
    """
    Parse an Exhibit 21 HTML page and extract subsidiary names
    and jurisdictions.  Handles both table-based and text-based
    formats commonly found in SEC filings.
    """
    soup = BeautifulSoup(html, "lxml")
    subsidiaries = []
    seen = set()  # deduplicate

    def clean(text: str) -> str:
        """Normalise whitespace and strip."""
        return re.sub(r"\s+", " ", text).strip()

    def add(name: str, jurisdiction: str = ""):
        name = clean(name)
        jurisdiction = clean(jurisdiction)
        # Skip empty, header-like, or parent-company rows
        if not name or len(name) < 3:
            return
        skip_patterns = [
            r"^name\b", r"^subsidiary", r"^entity", r"^exhibit",
            r"^list of", r"^significant", r"^jurisdiction",
            r"^state", r"^country", r"^place of", r"^page\b",
            r"^\d+$", r"^-+$", r"^\*+",
        ]
        for pat in skip_patterns:
            if re.search(pat, name, re.IGNORECASE):
                return
        key = name.upper()
        if key not in seen:
            seen.add(key)
            subsidiaries.append({
                "parent_company": parent_company,
                "subsidiary_name": name,
                "jurisdiction": jurisdiction,
                "source": "SEC Exhibit 21.1",
            })

    # ---- Strategy 1: HTML tables ----
    tables = soup.find_all("table")
    for table in tables:
        rows = table.find_all("tr")
        for row in rows:
            cells = row.find_all(["td", "th"])
            texts = [clean(c.get_text()) for c in cells]
            # Filter out empty cells
            texts = [t for t in texts if t]
            if len(texts) >= 2:
                add(texts[0], texts[1])
            elif len(texts) == 1:
                # Some tables use a single-column format
                # Try to split on common delimiters
                parts = re.split(r"\s{2,}|\t|—|–|\|", texts[0])
                parts = [p.strip() for p in parts if p.strip()]
                if len(parts) >= 2:
                    add(parts[0], parts[-1])
                else:
                    add(texts[0])

    # ---- Strategy 2: Text/paragraph-based (if tables yielded nothing) ----
    if not subsidiaries:
        log.info("  No table found — trying text-based parsing")
        text = soup.get_text("\n")
        lines = [clean(l) for l in text.split("\n") if clean(l)]

        # Look for the start of the subsidiary list
        start_idx = 0
        for i, line in enumerate(lines):
            if re.search(r"(subsidiaries|list of|exhibit\s*21)", line, re.IGNORECASE):
                start_idx = i + 1
                break

        for line in lines[start_idx:]:
            # Skip headers/footers
            if re.match(r"^(name|subsidiary|entity|exhibit|page|\d+$)", line, re.IGNORECASE):
                continue
            # Try to split name from jurisdiction
            parts = re.split(r"\s{2,}|\t", line)
            parts = [p.strip() for p in parts if p.strip()]
            if len(parts) >= 2:
                add(parts[0], parts[-1])
            elif len(parts) == 1 and len(parts[0]) > 3:
                add(parts[0])

    return subsidiaries

# ---------------------------------------------------------------------------
# Step 4: Main pipeline
# ---------------------------------------------------------------------------

def scrape_all() -> pd.DataFrame:
    """Run the full pipeline for all companies, all 10-Ks since START_YEAR."""
    all_subs = []

    for company_name, info in COMPANIES.items():
        cik = info["cik"]
        log.info(f"\n{'='*60}")
        log.info(f"Processing: {company_name} (CIK {cik})")
        log.info(f"{'='*60}")

        # 1. Find all 10-K filings since START_YEAR
        filings = get_all_10ks(cik)
        if not filings:
            log.warning(f"  No 10-K filings found for {company_name}")
            continue
        log.info(f"  Found {len(filings)} 10-K filings ({filings[-1]['filingDate']} → {filings[0]['filingDate']})")

        for filing in filings:
            log.info(
                f"\n  --- {filing['form']} filed {filing['filingDate']} "
                f"(accession: {filing['accessionNumber']}) ---"
            )

            # 2. Find Exhibit 21 URL
            ex21_url = find_exhibit21_url(cik, filing["accessionNumber"])
            if not ex21_url:
                log.warning(f"  No Exhibit 21 found in this filing")
                continue
            log.info(f"  Exhibit 21 URL: {ex21_url}")

            # 3. Download and parse
            try:
                resp = get(ex21_url)
                subs = parse_exhibit21(resp.text, company_name)
                log.info(f"  Extracted {len(subs)} subsidiaries")
                all_subs.extend(subs)
            except Exception as e:
                log.warning(f"  Failed to parse Exhibit 21: {e}")

    df = pd.DataFrame(all_subs)
    return df


def main():
    log.info("SEC Exhibit 21.1 Subsidiary Scraper")
    log.info("=" * 60)

    df = scrape_all()

    if df.empty:
        log.warning("No subsidiaries extracted. Check logs for errors.")
        return

    # Strip commas from all string columns so CSV needs no quoting
    for col in df.select_dtypes(include="object").columns:
        df[col] = df[col].str.replace(",", "", regex=False)

    # Deduplicate by (parent_company, subsidiary_name) — keep first occurrence
    before = len(df)
    df = df.drop_duplicates(subset=["parent_company", "subsidiary_name"], keep="first")
    log.info(f"\nDeduplicated: {before} → {len(df)} unique subsidiaries")

    # Save full output (no quotes)
    df.to_csv(OUTPUT_FILE, index=False, quoting=csv.QUOTE_NONE, escapechar="\\")
    log.info(f"Saved {len(df)} subsidiaries to {OUTPUT_FILE}")

    # Print summary
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    summary = df.groupby("parent_company").size().reset_index(name="count")
    print(summary.to_string(index=False))
    print(f"\nTotal: {len(df)} subsidiaries across {df['parent_company'].nunique()} firms")

    # Also save a version compatible with your existing subsidiaries.csv format
    compat_df = df.rename(columns={"jurisdiction": "reason"})
    compat_df["reason"] = "SEC Exhibit 21.1 subsidiary — " + compat_df["reason"]
    compat_df = compat_df[["parent_company", "subsidiary_name", "source", "reason"]]
    compat_file = "subsidiaries_sec_compat.csv"
    compat_df.to_csv(compat_file, index=False, quoting=csv.QUOTE_NONE, escapechar="\\")
    log.info(f"Saved pipeline-compatible version to {compat_file}")


if __name__ == "__main__":
    main()