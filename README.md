# UKB_CHD_EWAS

This repository contains code and scripts used for the analysis in the study:  
**"Modifiable influencing factors and their joint effects on early- and late-onset coronary heart disease"**.

---

## 📌 Overview

We conducted an exposure-wide association study (EWAS) to evaluate the relationships between 213 modifiable factors and the risk of coronary heart disease (CHD) using data from the UK Biobank. The analytical pipeline includes:

- Cox regression-based EWAS
- Two-sample Mendelian Randomization (MR)
- Construction of weighted standardized scores by domain
- Gene–environment interaction analysis using polygenic risk scores (PRS)
- Estimation of Population Attributable Fractions (PAFs)

---

## 📂 Repository Structure

| File / Folder         | Description |
|-----------------------|-------------|
| `1. EWAS.R`           | Performs Cox regression for 213 baseline variables to identify associations with CHD. Adjusts for multiple covariates and checks proportional hazards assumptions. |
| `2. TwoSampleMR.R`    | Conducts two-sample MR using GWAS data from FinnGen and IEU OpenGWAS to infer causal relationships. Includes IVW, MR Egger, and sensitivity analyses. |
| `3. Joint effect.R`   | Constructs weighted standardized scores for each domain and evaluates their joint effect on CHD risk. |
| `4. Interaction.R`    | Examines additive interactions between domain scores and genetic risk (PRS) on CHD outcomes. |
| `5. PAF.R`            | Estimates domain-specific PAFs using the `graphPAF` package, with PCA-derived weighting and sensitivity analyses. |

---

## ⚙️ Software and Dependencies

The analyses were conducted in **R (version ≥ 4.2.0)**. Major packages used include:

```r
library(survival)
library(randomForestSRC)
library(TwoSampleMR)
library(MRPRESSO)
library(graphPAF)
library(psych)        # for PCA
library(dplyr)
library(tidyr)
library(data.table)
