# Big Tech Climate Patents — Patent Landscape Analysis

Mapping climate and energy R&D priorities at Alphabet, Amazon, Apple, Meta, and Microsoft using patent data from The Lens.

## Research Questions

1. **Climate innovation intensity**: How has each firm's share of energy/climate patents evolved (2010–2024), and do portfolios cluster or diverge?
2. **Topic discovery**: What are the dominant technological themes within Big Tech climate patents, and have these shifted around major policy events?
3. **Portfolio convergence** (advanced): Are Big Tech climate patent portfolios becoming more similar over time, especially in AI-adjacent energy domains?

## Project Structure

```
bigtech-climate-patents/
├── data/
│   ├── raw/              # Raw API responses, subsidiary lists
│   └── processed/        # Cleaned datasets, topic assignments
├── scripts/
│   ├── python/           # Lens API queries, BERTopic pipeline
│   └── r/                # Intensity index, visualizations, convergence
├── outputs/
│   ├── figures/           # Charts and plots
│   └── tables/            # Summary tables
└── docs/                  # Notes, references, methodology decisions
```

## Tech Stack

- **Python**: Lens API querying, SBERT embeddings, BERTopic
- **R**: Climate innovation intensity index, trend analysis, visualizations (ggplot2)

## Data Source

- [The Lens](https://www.lens.org/) — patent records via API
- CPC Y02 classification codes for climate/energy patent filtering

## Setup

### Python (using uv)
```bash
# Install uv (once)
curl -LsSf https://astral.sh/uv/install.sh | sh

# From project root — creates venv and installs everything
uv sync

# Run a script
uv run python scripts/python/01_collect_patents.py

# Add a new dependency
uv add some-package
```

### R
```r
install.packages(c("tidyverse", "ggplot2", "readr", "janitor", "scales"))
```
