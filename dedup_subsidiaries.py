"""Remove duplicate rows from subsidiaries.csv based on subsidiary_name."""

import pandas as pd

path = "subsidiaries.csv"

# Some rows have 3 cols (no reason), some have 4 — read with flexible column count
df = pd.read_csv(
    path,
    names=["parent_company", "subsidiary_name", "source", "reason"],
    header=0,
    skipinitialspace=True,
)

# Fill missing reason
df["reason"] = df["reason"].fillna("")

# Strip anything in parentheses from subsidiary_name, e.g. "Facebook Holdings LLC (Delaware)" -> "Facebook Holdings LLC"
df["subsidiary_name"] = df["subsidiary_name"].str.replace(r"\s*\(.*?\)", "", regex=True).str.strip()

before = len(df)
df = df.drop_duplicates(subset=["subsidiary_name"], keep="first")

# Sort alphabetically by parent_company, then subsidiary_name
df = df.sort_values(["parent_company", "subsidiary_name"], ignore_index=True)

# Strip commas from all fields to keep CSV clean
for col in df.select_dtypes(include="object").columns:
    df[col] = df[col].str.replace(",", "", regex=False)

df.to_csv(path, index=False)
print(f"Deduplicated: {before} -> {len(df)} rows ({before - len(df)} removed)")