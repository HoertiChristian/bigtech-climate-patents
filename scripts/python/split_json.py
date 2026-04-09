import json
from pathlib import Path

data_dir = Path("scripts/python/data")

with open(data_dir / "patents_raw.json") as f:
    data = json.load(f)

for company, records in data.items():
    filename = company.lower() + "_patents.json"
    with open(data_dir / filename, "w") as f:
        json.dump(records, f, indent=2)
    print(f"{filename}: {len(records)} patents")