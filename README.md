# Discrete Choice Experiment: Organic & Certified Tomato Valuation
### A Cross-Country Comparison of Chinese and Japanese Consumer Preferences

**Course:** Choice Modeling in R | University of Warsaw, WNE | June 2026  
**Author:** Meifang Wu  
**Data source:** Yang, R. (2025). *Brand_CE* [Dataset]. Mendeley Data, Version 4. https://doi.org/10.17632/9jv5bdvbpj.4

---

## Research Question

Do Chinese and Japanese consumers value organic and specially certified tomatoes differently, and does external information about certification affect their willingness to pay?

---

## Project Structure

```
tomato-dce-project/
├── choice_of_model_final_with_M4b.R   # Main estimation script (Apollo, R 4.5.0)
├── tomato_DCE_report_final.pdf        # Final report (PDF)
├── tomato_DCE_report_final.tex        # LaTeX source
├── data/
│   ├── C_TOMATO.txt                   # China choice experiment data (422 respondents)
│   └── group Total.Tomato.txt         # Japan choice experiment data (412 respondents)
└── output/
    ├── tomato_apollo_wide.csv          # Wide-format estimation database
    ├── tomato_choice_shares.csv        # Choice shares by country & product type
    ├── tomato_segment_distribution.csv # Consumer segment sizes by country
    ├── tomato_model_comparison.csv     # LL, k, AIC, BIC for all models (M1–M5 + M4b)
    ├── tomato_class_exploration.csv    # 1/2/3-class LCM comparison table
    ├── tomato_wtp_M2.csv              # Country-specific WTP (M2)
    ├── tomato_wtp_M3.csv              # Information-adjusted WTP (M3)
    ├── tomato_wtp_M4.csv              # Class-specific WTP (M4)
    ├── tomato_M4_class_probs.csv      # M4 posterior class probabilities (individual level)
    ├── tomato_M4b_class_probs.csv     # M4b (3-class) posterior probabilities
    └── tomato_M5_attendance_by_info.csv # M5 ANA attendance probability by info group
```

---

## Data

The dataset comes from a Discrete Choice Experiment (DCE) conducted in China and Japan, originally published in:

> Yang, R. (2025). The role of brand commitment and external information in urban consumers' organic produce choices: Evidence from Japan and China. *PLOS ONE*. https://doi.org/10.1371/journal.pone.0337225

| | China | Japan | Total |
|---|---|---|---|
| Respondents | 422 | 412 | 834 |
| Choice tasks per respondent | 8 | 8 | 8 |
| Alternatives per task | 3 (A, B, No-buy) | 3 | 3 |
| Total observations | 3,376 | 3,296 | 6,672 |

**Attributes:**
- **Price:** RMB/500g (China, range ~3–19 RMB) / JPY/500g (Japan, range ~170–330 JPY)
- **Label:** Ordinary (baseline) / Organic / Special certification

**Information treatments (between-subjects):**
- EIC: Control (no information)
- EIO: Organic certification information only
- EIS: Special certification information only
- EIOS: Both types of information

---

## Models Estimated

| Model | Description | k | LL | AIC | BIC |
|---|---|---|---|---|---|
| **M1** | Baseline pooled MNL | 4 | −5,833.83 | 11,675.65 | 11,702.88 |
| **M2** | Country-specific MNL + SEG effects | 12 | −5,023.04 | 10,070.08 | 10,151.74 |
| **M3** | Info-treatment interaction MNL | 14 | −5,010.41 | 10,048.81 | 10,144.09 |
| **M4** | Two-class LCM *(preferred)* | 16 | −4,370.19 | 8,772.38 | 8,881.27 |
| **M4b** | Three-class LCM *(exploration only)* | 27 | −3,729.63 | 7,513.25 | 7,697.01 |
| **M5** | ANA two-class model *(behavioural extension)* | 10 | −4,609.82 | 9,239.63 | 9,307.69 |

**Why M4 (2-class) over M4b (3-class)?**  
The three-class model has lower AIC/BIC but produces a positive China price coefficient in Class 1 (+0.145, t = +3.66) — an implausible wrong-sign result — and a non-significant Japan price coefficient in Class 3 (t = −0.64). The two-class model is retained as the preferred specification on grounds of behavioural interpretability and parameter plausibility.

---

## Key Findings

**1. Both countries strongly prefer certified tomatoes**  
Special certification achieves the highest choice share in both China (41.7%) and Japan (42.1%), with organic second. No-buy rates are low (~10%).

**2. Significant country differences**  
- Chinese consumers: WTP +8.34 RMB for organic, +6.51 RMB for special certification
- Japanese consumers: WTP +40.19 JPY for organic, +37.64 JPY for special certification
- Japan's no-buy ASC (−6.69) is far more negative than China's (−2.38), indicating a much lower baseline propensity to opt out

**3. Preference heterogeneity dominates (M4)**  
- Class 1 (label-sensitive, **74.8%**): strong WTP for both labels (CN: +11.5 RMB organic / JP: +53.8 JPY organic)
- Class 2 (label-indifferent, **25.2%**): label coefficients not significant; WTP near zero
- Chinese respondents are significantly more likely to be label-indifferent (γ_china = 0.676, robust t = 3.12)

**4. Information effects are limited**  
- Only organic-only information (EIO) significantly raises WTP for organic labels (from 6.57 → 9.71 RMB in China)
- No information treatment shifts the probability of label attendance (M5 ANA): all γ coefficients have |t| < 1.7

---

## How to Reproduce

**Requirements:**
- R 4.5.0+
- Packages: `tidyverse`, `janitor`, `apollo` (v0.3.7)

**Steps:**
```r
# 1. Set your working directory to the project root
setwd("path/to/tomato-dce-project")

# 2. Place data files in the project root:
#    C_TOMATO.txt
#    group Total.Tomato.txt

# 3. Create output directory
dir.create("output")

# 4. Run the main script
source("choice_of_model_final_with_M4b.R")
```

All model outputs (`.rds`, `.csv`, `.txt`) will be saved to `./output/`. The script runs M1 → M2 → M3 → M4 → M4b → M5 sequentially and produces the model comparison table and all WTP/class probability CSV files.

---

*Estimated using Apollo 0.3.7 on R 4.5.0 for Windows. BGW algorithm (Bunch et al., 1993).*
