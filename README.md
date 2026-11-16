# Manufacturing Process Anomaly Detection (PCA, R)

A small R project that detects abnormal behaviour in a chemical manufacturing process
using **Principal Component Analysis (PCA)**. A PCA model is trained on normal operating
data only, and new samples are flagged as anomalies when their **Hotelling's T²** or
**SPE (Q statistic)** exceeds a control limit. Results are shown in an **R Shiny dashboard**.

## Dataset

| | |
|---|---|
| **Name** | Additional Tennessee Eastman Process Simulation Data for Anomaly Detection Evaluation |
| **Authors** | Rieth, C. A.; Amsel, B. D.; Tran, R.; Cook, M. B. (2017) |
| **Website** | Harvard Dataverse |
| **Dataset page** | https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/6C3JR1 |
| **DOI** | [10.7910/DVN/6C3JR1](https://doi.org/10.7910/DVN/6C3JR1) |

Files used (save them in `data/raw/`):

| File | Size | Direct download |
|---|---|---|
| `TEP_FaultFree_Training.RData` | 24.7 MB | https://dataverse.harvard.edu/api/access/datafile/3031241 |
| `TEP_Faulty_Training.RData` | 494 MB | https://dataverse.harvard.edu/api/access/datafile/3031242 |

**Why this dataset?** The Tennessee Eastman Process is the standard benchmark for
multivariate process monitoring. It has 52 correlated process variables (41 measurements
`xmeas_*` + 11 manipulated variables `xmv_*`) sampled every 3 minutes, with clearly
labelled normal runs and 20 known fault types. Strongly correlated sensors are exactly
the situation where PCA works well.

**Download.** `main.R` tries to download missing files automatically. If that fails,
open the links above in a browser (no login needed) and save the files into `data/raw/`.

## How the data is used

To keep things fast, `preprocessing.R` builds a small subset and saves it to `data/processed/`:

* **Training set (normal only):** fault-free runs 1–20 → 10,000 samples
* **Test set:** fault-free runs 21–25 (2,500 normal samples) + run 1 of each fault 1–20
  (10,000 samples). In faulty runs the fault starts after sample 20 (1 hour), so samples
  1–20 are labelled normal and 21–500 anomaly.
* Missing values are counted and replaced by the training median; constant sensors are removed.

## Project structure

```
Manufacturing-Anomaly-Detection/
├── data/raw/              # original .RData files from Harvard Dataverse
├── data/processed/        # train_normal.csv, test_mixed.csv
├── R/
│   ├── data_loading.R     # download + load .RData
│   ├── preprocessing.R    # subsets, missing values, labels
│   ├── eda.R              # summary stats, sensor plots, correlation heatmap
│   ├── pca_model.R        # PCA fit, T²/SPE, control limits
│   ├── anomaly_detection.R# flag anomalies + plots
│   └── evaluation.R       # accuracy, precision, recall, F1
├── app/app.R              # Shiny dashboard
├── results/plots/         # saved figures
├── results/metrics/       # saved metrics (CSV)
├── main.R                 # runs everything
└── requirements.R         # installs packages
```

## Requirements

R ≥ 4.1 and the packages `ggplot2`, `corrplot`, `shiny`, `shinydashboard`, `DT`.

```r
# from the project folder
source("requirements.R")
```

## How to run

```bash
cd Manufacturing-Anomaly-Detection
Rscript main.R
```

(or in RStudio: set the working directory to the project folder and `source("main.R")`).

The first run reads the large `.RData` files (needs ~3 GB of free RAM, about 1 minute)
and writes small CSVs to `data/processed/`. Later runs reuse the CSVs and take a few seconds.
`main.R` prints the EDA summary, PCA settings, evaluation metrics, and saves plots/metrics to `results/`.

## Launch the dashboard

After `main.R` has run once:

```r
shiny::runApp("app")
```

The dashboard has two pages:

* **Live Monitoring** – pick a run (normal or one of 20 faults) and move/play the time slider.
  Shows process status (Normal / Anomaly Detected), current T² and SPE vs. limits, an
  anomaly timeline, sensor trend plots with normal ranges, and a table of all sensor readings.
* **PCA & Model Performance** – accuracy, precision, recall, F1, PCA score plot and the
  detection rate for each fault type.

## How to check that the PCA detection works

1. **Normal data gives few alarms.** `main.R` prints the alarm rate on the normal
   training data; it should be about 1–2 % (the limits are 99th percentiles).
   The test-set *false alarm rate* (normal samples flagged) should also be low.
2. **Faults give alarms after they start.** In the dashboard choose *Fault 1*, *Fault 4* or
   *Fault 6* and press play: T²/SPE stay under the red line for the first 20 samples and
   cross it soon after the orange fault-start line; the status box turns red.
   Choose a *Normal operation* run: the status stays green almost all the time.
3. **Look at the metrics.** `results/metrics/overall_metrics.csv` and
   `per_fault_detection.csv`. Most step faults are detected close to 100 %.
   Faults **3, 9 and 15** are known in the literature to be almost undetectable by PCA
   (and most other methods) because they barely change the measured variables, so their
   detection rate is close to the false-alarm rate. This lowers the overall recall and is expected.

## Results (from an actual run of `main.R`)

* PCA kept **31 of 52 components** (90.5 % of variance).
* Control limits (99th percentile of normal data): **T² = 51.96**, **SPE = 11.32**.
* Alarm rate on normal training data: 1.99 %.

Test set: 12,500 samples (2,900 normal, 9,600 anomaly)

| Metric | Value |
|---|---|
| Accuracy | 0.728 |
| Precision | 0.993 |
| Recall | 0.651 |
| F1-score | 0.786 |
| False alarm rate (normal samples) | 0.016 |

Confusion matrix: TP = 6249, FP = 46, TN = 2854, FN = 3351.

Detection rate per fault (%):

| Fault | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Rate | 99 | 98 | 1 | 100 | 41 | 100 | 100 | 93 | 1 | 43 | 76 | 96 | 94 | 100 | 1 | 22 | 88 | 81 | 13 | 54 |

Precision is very high (few false alarms). Recall is lowered mainly by faults 3, 9, 15
(not detectable with PCA, as is well known for TEP) and the subtle faults 5, 10, 16, 19, 20.
