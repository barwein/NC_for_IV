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
├── verify_reproducibility.R  # Reproduce via docker container
├── replication_output.log    # Full replication log created from "verify_reproducibility.R" run
├── main.R                    # Master driver script
└── README.md                 # This document
```

---

## 4  Computational Requirements and Version Control

- **Software**
  - **R version**: tested on R 4.5.1 (R ≥ 4.5.0 recommended)
  - **R packages**: pinned in [`requirements.txt`] with `pkg==x.y.z` lines for reproducibility
  - **Container**: A `Dockerfile` is provided to replicate the exact Linux-based computational environment (Recommended).
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

You may choose between two methods. **Method A** runs on your local R installation. **Method B** uses Docker to guarantee an exact environment match (fixing OS-specific package version issues).

### Method A: Local Execution (Standard)

1. Open the project in **RStudio**.
2. Open `main.R`.
3. Run the line `source("setup_env.R")` to install packages.
   * **Interactive Mode:** You will be prompted to choose **Strict** (exact versions) or **Relaxed** (latest CRAN versions).
   * **Non-Interactive (Script) Mode:** The system defaults to **Strict** automatically.
4. Execute `source("main.R")`.
5. Outputs (Tables/Figures) are saved in their respective `Data_analysis` subfolders.

### Method B: Docker Execution (Guaranteed Reproducibility)

Use this method if you encounter "version not available" errors or OS-specific compilation issues. This runs the analysis in a Linux container with R 4.4.3 and exact package versions.

**Prerequisite:** Ensure [Docker Desktop](https://www.docker.com/products/docker-desktop/) is installed and running.

#### Option 1: Automated R Script (Recommended)
We provide a helper script to manage the Docker process entirely from RStudio:
1. Open `verify_reproducibility.R`.
2. Click **Source** (or run `source("verify_reproducibility.R")`).
3. This script will:
   * Build the Docker image.
   * Run the analysis.
   * Save a log file (`replication_output.log`) proving success.
   * Save all generated Tables/Figures to your local folder.

#### Option 2: Command Line
If you prefer the terminal:

1. **Build the image:**
   ```bash
   docker build -t replication-package .
   ```
  
2. **Run the container:**

  ```bash
  docker run --rm -v "$(pwd):/project" replication-package
  ```

---

## 6  Program–Output Cross‑Walk

| Paper exhibit             | Script(s)                                                       | 
| ------------------------- | --------------------------------------------------------------- | 
| Table 2                   | `Literature_survey/NC_literature_survey_summary.R`              | 
| Table 4 (ADH)             | `Data_analysis/ADH/ADH_Run.R` (lines 163-167, 181-183)          | 
| Table 4 (Deming)          | `Data_analysis/Deming/Deming_Run.R` (lines 49-57, 75-77)        | 
| Table 5 (Ashraf & Galor)  | `Data_analysis/AshrafGalor/AshrafGalor.R` (lines 232-239, 271)  | 
| Table 5 (Nunn & Qian)     | `Data_analysis/NunnQian/NunnQian.R` (lines 123-124, 141-142,188)| 
| Figure 3 (Deming)         | `Data_analysis/Deming/Deming_Run.R` (lines 219-223)             | 

---


## 7 Random-number generators referenced

The following R scripts use random-number generators via `set.seed()` to ensure reproducibility:

- `Data_analysis/Deming/Deming_Run.R` (line 32)
- `Data_analysis/ADH/ADH_Run.R` (line 52)

## 8  References

- Autor, D., Dorn, D., & Hanson, G. (2013). “The China Syndrome: Local Labor Market Effects of Import Competition in the United States.” *American Economic Review*. DOI: 10.3886/E112670V1. ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112670/version/V1/view))
- Deming, D. (2014). “Using School Choice Lotteries to Test Measures of School Effectiveness.” *American Economic Journal: Applied Economics*. DOI: 10.3886/E112805V1. ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112805/version/V1/view))
- Ashraf, Q., & Galor, O. (2013). “The ‘Out of Africa’ Hypothesis, Human Genetic Diversity, and Comparative Economic Development.” *American Economic Review*. DOI: 10.3886/E112588V1. ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112588/version/V1/view))
- Nunn, N., & Qian, N. (2014). “US Food Aid and Civil Conflict.” *American Economic Review*. DOI: 10.3886/E112825V1. ([openicpsr.org](https://www.openicpsr.org/openicpsr/project/112825/version/V1/view))
- Danieli, O., Nevo, D., Walk, I., Weinstein, B., & Zeltzer, D. (2025). “Negative Control Falsification Tests for Instrumental Variable Designs.” *Working paper*, arXiv:2312.15624. ([arxiv.org](https://arxiv.org/abs/2312.15624))

---

### Contact

For questions about this replication archive, please email Oren Danieli, the corresponding author at orendanieli@tauex.tau.ac.il.
