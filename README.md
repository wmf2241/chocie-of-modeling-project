# Discrete Choice Experiment: Organic & Certified Tomato Valuation
### A Cross-Country Comparison of Chinese and Japanese Consumer Preferences

**Course:** Choice Modeling in R | University of Warsaw, WNE | June 2026  
**Author:** Meifang Wu  
**Instructor:** Wiktor Budziński

---

## Research Question

Do Chinese and Japanese consumers value organic and specially certified tomatoes
differently, and does external information about certification affect their
willingness to pay?

---

## Data Source

The dataset originates from:

> Yang, R. (2025). The role of brand commitment and external information in
> urban consumers' organic produce choices: Evidence from Japan and China.
> *PLOS ONE*. https://doi.org/10.1371/journal.pone.0337225

> Yang, R. (2025). *Brand_CE* [Dataset]. Mendeley Data, Version 4.
> https://doi.org/10.17632/9jv5bdvbpj.4

The Mendeley deposit includes choice-experiment data for three products
(tomato, cabbage, carrot) across China and Japan. This project uses the
**tomato** data only. The original author's replication code is preserved
in `YANG_PLOSONE/` for reference.

---

## Repository Structure

```
Brand_CE/
│
├── project_codes.R                        # ← Main script (run this)
│
├── choice_of_model_final_report.pdf       # Final report (PDF)
│
├── project_summary.txt                    # Project analysis summary
│
├── C_TOMATO.txt                           # China tomato data (422 respondents)
├── group Total.Tomato.txt                 # Japan tomato data (412 respondents)
├── C_CABBAGE.txt                          # China cabbage data (not used)
├── C_CARROT.txt                           # China carrot data (not used)
├── group Total.Cabbage.txt                # Japan cabbage data (not used)
├── group Total.Carrot.txt                 # Japan carrot data (not used)
│
├── Raw_data_China.xlsx                    # Raw survey data — China
├── Raw_data_Japan.xlsx                    # Raw survey data — Japan
│
├── datasets_organic product/              # Duplicate of all data files above
│
├── YANG_PLOSONE/                          # Original author's replication materials
│   ├── BC_EI_CHINAJAPAN.R                 # Yang (2025) original R code
│   └── *.txt                              # Original data files
│
├── output/                                # All model estimation outputs
│   ├── tomato_apollo_wide.csv             # Wide-format estimation database
│   ├── tomato_choice_shares.csv           # Choice shares by country & product
│   ├── tomato_segment_distribution.csv    # Segment distribution by country
│   ├── tomato_model_comparison.csv        # LL/AIC/BIC for M1–M5 + M4b
│   ├── tomato_class_exploration.csv       # 1/2/3-class LCM comparison
│   ├── tomato_wtp_M2.csv                  # Country-specific WTP (M2)
│   ├── tomato_wtp_M3.csv                  # Info-adjusted WTP (M3)
│   ├── tomato_wtp_M4.csv                  # Class-specific WTP (M4)
│   ├── tomato_M4_class_probs.csv          # M4 posterior class probabilities
│   ├── tomato_M4b_class_probs.csv         # M4b (3-class) posterior probs
│   ├── tomato_M5_attendance_by_info.csv   # M5 ANA attendance by info group
│   ├── M1_baseline_MNL_*                  # Apollo outputs: M1
│   ├── M2_country_MNL_*                   # Apollo outputs: M2
│   ├── M3_info_interactions_MNL_*         # Apollo outputs: M3
│   ├── M4_2class_LCM_*                    # Apollo outputs: M4
│   ├── M4b_3class_LCM_*                   # Apollo outputs: M4b (exploration)
│   ├── M5_ANA_2class_*                    # Apollo outputs: M5
│   └── *_OLD*_*                           # Intermediate versions (can be deleted)
│
├── project of choice of model.R           # Early exploratory script (archived)
├── tomato_apollo_wide.csv                 # Duplicate at root (can be deleted)
├── tomato_choice_shares.csv               # Duplicate at root (can be deleted)
├── tomato_wtp_M2.csv                      # Duplicate at root (can be deleted)
└── .Rhistory
```

---

## Models Estimated

| Model | Description | k | LL | AIC | BIC |
|---|---|---|---|---|---|
| **M1** | Baseline pooled MNL | 4 | −5,833.83 | 11,675.65 | 11,702.88 |
| **M2** | Country-specific MNL + SEG effects | 12 | −5,023.04 | 10,070.08 | 10,151.74 |
| **M3** | Info-treatment interaction MNL | 14 | −5,010.41 | 10,048.81 | 10,144.09 |
| **M4** | Two-class LCM ✦ *preferred* | 16 | −4,370.19 | 8,772.38 | 8,881.27 |
| **M4b** | Three-class LCM *(specification exploration)* | 27 | −3,729.63 | 7,513.25 | 7,697.01 |
| **M5** | ANA two-class *(behavioural extension)* | 10 | −4,609.82 | 9,239.63 | 9,307.69 |

**Why M4 (2-class) over M4b (3-class)?**  
The three-class model has lower AIC/BIC but produces an implausible positive
China price coefficient in Class 1 (+0.145, robust t = +3.66) and a
non-significant Japan price coefficient in Class 3 (t = −0.64), making WTP
interpretation unreliable. The two-class model is retained on grounds of
behavioural plausibility and parameter interpretability.

---

## Key Findings

**1. Both countries strongly prefer certified tomatoes**  
Special certification achieves the highest choice share in China (41.7%) and
Japan (42.1%). No-buy rates are low (~10%).

**2. Significant country differences in WTP**  
- China: +8.34 RMB for organic, +6.51 RMB for special certification  
- Japan: +40.19 JPY for organic, +37.64 JPY for special certification  
- Japan's no-buy ASC (−6.69) is far more negative than China's (−2.38)

**3. Preference heterogeneity dominates (M4)**  
- Class 1 — label-sensitive (74.8%): WTP China +11.5 RMB / Japan +53.8 JPY for organic  
- Class 2 — label-indifferent (25.2%): label coefficients not significant  
- Chinese respondents significantly more likely to be in Class 2 (γ = 0.676, t = 3.12)

**4. Information effects are limited (M3 + M5)**  
- Only organic-only information (EIO) significantly raises organic WTP  
- No information treatment shifts the probability of label attendance (M5 ANA)

---

## How to Reproduce

**Requirements:**
- R 4.5.0+
- Packages: `tidyverse`, `janitor`, `apollo` (v0.3.7)

**Steps:**

```r
# 1. Clone or download this repository
# 2. Open project_codes.R in RStudio
# 3. Update the working directory path at the top of the script:
setwd("C:/your/path/to/Brand_CE")

# 4. Run the full script — all outputs saved to ./output/
source("project_codes.R")
```



---

*Estimated using Apollo 0.3.7 on R 4.5.0 for Windows.*
