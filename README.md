# Negative Control Falsification Tests for Instrumental Variable Designs

Replication package for the article **“Negative Control Falsification Tests for Instrumental Variable Designs.”**
The archive reproduces every figure, table and numerical result reported in the paper,
spanning both Monte‑Carlo simulations and four empirical applications.

---

## 1  Overview

Running the master script `main.R` (after restoring the R environment—see Section 5) executes **all** elements of the workflow in the proper order:

| Stage                                | Approx. runtime | Output location         |
| ------------------------------------ | --------------- | ----------------------- |
|  Simulations (Sections 3–4 of paper) |  ≈ 24 h         | `Simulations/`          |
|  Applications (Sections 5–6)         |  ≈ 1 h          | `Data_analysis/`        |

The simulations evaluate negative‑control falsification tests under a variety of data‑generating processes,
while the data-applications replicate and extend the following published studies:

1. Autor, Dorn & Hanson (2013) — “The China Syndrome: Local Labor Market Effects of Import Competition in the United States.”
2. Deming (2014) — “Using School Choice Lotteries to Test Measures of School Effectiveness.”.
3. Ashraf & Galor (2013) — “The ‘Out of Africa’ Hypothesis, Human Genetic Diversity, and Comparative Economic Development.”.
4. Nunn & Qian (2014) — “US Food Aid and Civil Conflict.”

---

## 2  Data Availability and Provenance

### 2.1  Statement about Rights

\* All data used were obtained from open‑access replication archives at the AEA Data & Code Repository (openICPSR). No proprietary or confidential data are redistributed here.

### 2.2  License for Data

The *code* in this repository is released under the Creative Commons Attribution 4.0 International Public License.
The raw data remain under the licenses imposed by their original depositors; users must consult the linked repositories for terms of use.

### 2.3  Summary of Availability

\* Raw data: **publicly available**. See *openICPSR* links in Section 7 of this file.

### 2.4  Details on Each Data Source

| Data source                                                                              | Files in original archive | Provided here? | Persistent ID                                                                                           |
| ---------------------------------------------------------------------------------------- | ------------------------- | -------------- | ------------------------------------------------------------------------------------------------------- |
| Autor, Dorn & Hanson (2013) — *The China Syndrome*                                       | ICPSR bundle              | Yes             | 10.3886/E112670V1 ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112670/version/V1/view)) |
| Deming (2014) — *Using School Choice Lotteries to Test Measures of School Effectiveness* | ICPSR bundle              | Yes             | 10.3886/E112805V1 ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112805/version/V1/view)) |
| Ashraf & Galor (2013) — *The “Out of Africa” Hypothesis …*                               | ICPSR bundle              | Yes             | 10.3886/E112588V1 ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112588/version/V1/view)) |
| Nunn & Qian (2014) — *US Food Aid and Civil Conflict*                                    | ICPSR bundle              | Yes             | 10.3886/E112825V1 ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112825/version/V1/view)) |

---

## 3  Included Files and Directory Structure

```
├── Aux_functions/            # Helper functions called by multiple scripts
├── Data_analysis/
│   ├── ADH/                  # China‑Shock application
│   ├── Deming/               # School‑lottery application
│   ├── AshrafGalor/          # Out‑of‑Africa application
│   └── NunnQian/             # Food‑aid application
├── Simulations/              # Monte‑Carlo study
├── results/                  # Tables & figures created by code
├── renv.lock                 # R package manifest (for renv)
├── main.R                    # Master driver script
└── README.md                 # This document
```

---

## 4  Computational Requirements

- **Software**
  - **R 4.2.2** (x86\_64‑w64‑mingw32) with packages listed below (a complete list is encoded in `renv.lock`): `data.table`, `sandwich`, `ivreg`, `mvtnorm`, `foreach`, `parallel`, `dplyr`, `lfe`, `mgcv`, `caret`, `systemfit`, `aod`, `tidyverse`, `ggpubr`, `latex2exp`, `gridExtra`, `haven`, `fixest`, `mltools`, `stringr`, `kableExtra`, `lmtest`, `MASS`, `stargazer`, `ggplot2`, `readr` ([github.com](https://github.com/barwein/NC_for_IV))
- **Hardware**  ≥ 4 GB RAM, tested on 8‑core CPU (parallel backend optional).
- **Runtime**  ≈ 1 h for applications; ≈ 24 h for full simulation suite.
- **Randomness**  All simulation scripts call `set.seed(...)` for reproducibility.

To recreate the exact R environment, run:

```bash
Rscript -e "install.packages('renv'); renv::restore()"
```

---

## 5  How to Run the Replication

1. **Clone** this repository (or download the ZIP) to a local folder with write permission.
2. **Restore R environment** using `renv` (see command above).
3. **Execute** the master script:
   ```bash
   Rscript main.R
   ```
4. **Inspect outputs** under `results/`—sub‑directories mirror the paper structure. Tables are saved as CSV/LaTeX, figures as PDF/PNG.

Scripts are fully autonomous; no manual editing of paths is required.

---

## 6  Program–Output Cross‑Walk

| Paper result              | Script(s)                                                       | 
| ------------------------- | --------------------------------------------------------------- | 
| Simulation (run)          | `Simulations/NCO_simulations.R`, `Simulations/RC_simulations.R` | 
| Simulations (plots)       | `Simulations/simulations_plots.R`                               | 
| Table 3 (ADH)             | `Data_analysis/ADH/ADH_Run.R`                                   | 
| Table 3 (Deming)          | `Data_analysis/Deming/Deming_Run.R`                             | 
| Table 4 (Ashraf & Galor)  | `Data_analysis/AshrafGalor/AshrafGalor.R`                       | 
| Table 4 (Nunn & Qian)     | `Data_analysis/NunnQian/NunnQian.R`                             | 

---

## 7  References

- Autor, D., Dorn, D., & Hanson, G. (2013). “The China Syndrome: Local Labor Market Effects of Import Competition in the United States.” *American Economic Review*. DOI: 10.3886/E112670V1. ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112670/version/V1/view))
- Deming, D. (2014). “Using School Choice Lotteries to Test Measures of School Effectiveness.” *American Economic Journal: Applied Economics*. DOI: 10.3886/E112805V1. ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112805/version/V1/view))
- Ashraf, Q., & Galor, O. (2013). “The ‘Out of Africa’ Hypothesis, Human Genetic Diversity, and Comparative Economic Development.” *American Economic Review*. DOI: 10.3886/E112588V1. ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112588/version/V1/view))
- Nunn, N., & Qian, N. (2014). “US Food Aid and Civil Conflict.” *American Economic Review*. DOI: 10.3886/E112825V1. ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112825/version/V1/view))
- Danieli, O., Nevo, D., Walk, I., Weinstein, B., & Zeltzer, D. (2025). “Negative Control Falsification Tests for Instrumental Variable Designs.” *Working paper*, arXiv:2312.15624. ([arxiv.org](https://arxiv.org/abs/2312.15624))

---

### Contact

For questions about this replication archive, please open an issue on the GitHub repository or email the corresponding author (see paper for contact details).

