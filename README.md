# Retora Message Evaluation Report

This repository contains the data, scripts, and report used to compare human survey ratings
with Retora model ratings for a set of political message drafts. The main output is a Quarto
HTML report that summarizes mean scores, rankings, agreement metrics, and stability across
demographic groups and repeated model runs.

## What is in this repo
- `main.qmd`: Primary Quarto report (HTML output `main.html`).
- `cleaning_qualtrics.R`: Data prep and analysis script used as the basis for the report.
- `testing_retora.ipynb`: Notebook for calling the Retora API and writing model results.
- `custom_feedback_runs_by_group.csv`: Retora outputs by message, run, age, and gender.
- `messages_surveyjs_id_map.csv`: Mapping between message IDs and full text.
- `messages_to_test.csv`: Source message set for model runs.
- `retora_February 2, 2026_15.54.csv`: Qualtrics export used for the human ratings.
- `main.html` and `main_files/`: Rendered report output and assets.

## Data inputs and outputs
Inputs:
- Qualtrics export CSV (default: `retora_February 2, 2026_15.54.csv`)
- Retora outputs (default: `custom_feedback_runs_by_group.csv`)
- Message ID map (default: `messages_surveyjs_id_map.csv`)

Outputs:
- `main.html`: Rendered report
- `main_files/`: HTML assets

## Requirements
- R (recommended: 4.2+)
- Quarto
- R packages (installed via `pacman::p_load` in `main.qmd`):
  - tidyverse, psych, irr, here, stringdist, stringr, purrr, kableExtra

## Render the report
From the repo root:
```bash
quarto render main.qmd
```

The report reads data paths from Quarto parameters defined at the top of `main.qmd`:
```yaml
params:
  qualtrics_csv: "retora_February 2, 2026_15.54.csv"
  retora_csv: "custom_feedback_runs_by_group.csv"
  map_csv: "messages_surveyjs_id_map.csv"
```
Update these if you want to use a different export or Retora run file.

## Updating Retora runs
The notebook `testing_retora.ipynb` contains the API workflow used to generate Retora scores
and append them to `custom_feedback_runs_by_group.csv`. It:
- Authenticates with the Retora API.
- Sends each message to the `custom-feedback` endpoint.
- Writes per-run, per-message scores including all dimensions.

When you update the Retora runs, re-render `main.qmd` to refresh the report.

## Report structure (main.qmd)
Key sections include:
- Human data preparation and labeling (age/gender factors).
- Retora data prep and message ID mapping.
- Message-level comparisons (overall, by age, by gender).
- Dimension-level comparisons.
- Agreement metrics (ranks and mean scores with Spearman/Pearson/R2).
- Retora rank stability across runs and demographic groups.

## Notes and conventions
- Scores are on a 0-10 scale.
- Message IDs use the `msg_XX` format from `messages_surveyjs_id_map.csv`.
- Ranking uses dense ranks with higher mean score = better rank.
- Stability uses run-to-mean rank correlations within each age x gender group.

## Troubleshooting
- If the report shows unmatched messages, verify `messages_surveyjs_id_map.csv` matches
  the exact message text in `custom_feedback_runs_by_group.csv`.
- If a render fails due to missing packages, install them in R:
  `install.packages(c("tidyverse","psych","irr","here","stringdist","stringr","purrr","kableExtra"))`

## Contact
This repo is a working analysis workspace. If you need to change the report layout or add
new plots/tables, update `main.qmd` and re-render.
