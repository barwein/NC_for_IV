# Negative Control Falsification Tests for Instrumental Variable Designs

Replication package for the article **“Negative Control Falsification Tests for Instrumental Variable Designs.”**
The archive reproduces every figure, table and numerical result reported in the paper,
including analyses of four empirical applications and a summary of the literature review of current use of falsification tests for IV in Economics.

---

## 1  Overview

Running the master script `main.R` (after restoring the R environment—see Section 5) executes all code required to reproduce the paper’s exhibits.
The analysis uses replication data and code from the following published studies:

1. Autor, Dorn & Hanson (2013) — “The China Syndrome: Local Labor Market Effects of Import Competition in the United States.”
2. Deming (2014) — “Using School Choice Lotteries to Test Measures of School Effectiveness.”.
3. Ashraf & Galor (2013) — “The ‘Out of Africa’ Hypothesis, Human Genetic Diversity, and Comparative Economic Development.”.
4. Nunn & Qian (2014) — “US Food Aid and Civil Conflict.”

---

## 2  Data Availability and Provenance

### 2.1  Statement about Rights

All data used were obtained from open‑access replication archives at the AEA Data & Code Repository (openICPSR). No proprietary or confidential data are redistributed here.

### 2.2  License for Data

The *code* in this repository is released under the Creative Commons Attribution 4.0 International Public License.
The raw data remain under the licenses imposed by their original depositors; users must consult the linked repositories for terms of use.

### 2.3  Summary of Availability

Raw data: **publicly available**. See *openICPSR* links in Section 7 of this file.

### 2.4  Details on Each Data Source

| Data source                                                                              | Files in original archive | Provided here? | Persistent ID                                                                                           |
| ---------------------------------------------------------------------------------------- | ------------------------- | -------------- | ------------------------------------------------------------------------------------------------------- |
| Autor, Dorn & Hanson (2013) — *The China Syndrome*                                       | ICPSR bundle              | Yes             | 10.3886/E112670V1 ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112670/version/V1/view)) |
| Deming (2014) — *Using School Choice Lotteries to Test Measures of School Effectiveness* | ICPSR bundle              | Yes             | 10.3886/E112805V1 ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112805/version/V1/view)) |
| Ashraf & Galor (2013) — *The “Out of Africa” Hypothesis …*                               | ICPSR bundle              | Yes             | 10.3886/E112588V1 ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112588/version/V1/view)) |
| Nunn & Qian (2014) — *US Food Aid and Civil Conflict*                                    | ICPSR bundle              | Yes             | 10.3886/E112825V1 ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112825/version/V1/view)) |


The literature survey underlying Table 2 is available in `Literature_survey/NC_literature_survey.csv`.


---

## 3  Included Files and Directory Structure

```
├── Aux_functions/            # Helper functions called by multiple scripts
├── Literature_survey/        # Survey of current literature conducted by the authors
├── Data_analysis/
│   ├── ADH/                  # Autor, Dorn, Hanson
│   ├── Deming/               # Deming
│   ├── AshrafGalor/          # Ashraf and Galor
│   └── NunnQian/             # Nunn and Qian
├── requirements.txt          # list of all required R packages (with version)
├── setup_env.R               # Install all required packages
├── main.R                    # Master driver script
└── README.md                 # This document
```

---

## 4  Computational Requirements

- **Software**
  - **R version**: tested on R 4.4.3 (R ≥ 4.4.0 recommended)
  - **R packages**: pinned in [`requirements.txt`] with `pkg==x.y.z` lines for reproducibility
  - **Operating System**: tested on Windows 11 and MacOS 15.7.1
  - **Hardware**  ≥ 4 GB RAM, tested on 8‑core CPU (parallel backend optional).
- **Runtime**  ≈ 1 h 

To recreate the exact R package environment, run:

1. Open the project in **RStudio**.
2. Open `main.R`
3. Run the lines `source("setup_env.R")` to install all required packages.
4. When prompted, choose:
   - **Strict** – installs the **exact pinned versions** from `requirements.txt`.
   - **Relaxed** – installs the **latest CRAN versions**.

---

## 5  How to Run the Replication

1. **Clone** this repository (or download the ZIP) to a local folder with write permission.
2. **Restore R environment** using `renv` (see command above).
3. **Execute** the master script:
   ```bash
   Rscript main.R
   ```
4. **Inspect outputs** under relevant the folders. Tables are saved as CSV/LaTeX, figures as PDF/PNG.

Paths are relative so no manual editing of paths is required.

---

## 6  Program–Output Cross‑Walk

| Paper exhibit             | Script(s)                                                       | 
| ------------------------- | --------------------------------------------------------------- | 
| Table 2                   | `Literature_survey/NC_literature_survey_summary.R`              | 
| Table 4 (ADH)             | `Data_analysis/ADH/ADH_Run.R`                                   | 
| Table 4 (Deming)          | `Data_analysis/Deming/Deming_Run.R`                             | 
| Table 5 (Ashraf & Galor)  | `Data_analysis/AshrafGalor/AshrafGalor.R`                       | 
| Table 5 (Nunn & Qian)     | `Data_analysis/NunnQian/NunnQian.R`                             | 
| Figure 3 (Deming)         | `Data_analysis/Deming/Deming_Run.R`                             | 

---

## 7  References

- Autor, D., Dorn, D., & Hanson, G. (2013). “The China Syndrome: Local Labor Market Effects of Import Competition in the United States.” *American Economic Review*. DOI: 10.3886/E112670V1. ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112670/version/V1/view))
- Deming, D. (2014). “Using School Choice Lotteries to Test Measures of School Effectiveness.” *American Economic Journal: Applied Economics*. DOI: 10.3886/E112805V1. ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112805/version/V1/view))
- Ashraf, Q., & Galor, O. (2013). “The ‘Out of Africa’ Hypothesis, Human Genetic Diversity, and Comparative Economic Development.” *American Economic Review*. DOI: 10.3886/E112588V1. ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112588/version/V1/view))
- Nunn, N., & Qian, N. (2014). “US Food Aid and Civil Conflict.” *American Economic Review*. DOI: 10.3886/E112825V1. ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112825/version/V1/view))
- Danieli, O., Nevo, D., Walk, I., Weinstein, B., & Zeltzer, D. (2025). “Negative Control Falsification Tests for Instrumental Variable Designs.” *Working paper*, arXiv:2312.15624. ([arxiv.org](https://arxiv.org/abs/2312.15624))

---

### Contact

For questions about this replication archive, please email Oren Danieli, the corresponding author at orendanieli@tauex.tau.ac.il.
