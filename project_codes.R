############################################################
# FINAL PROJECT: Discrete Choice Experiment
# Research Question: Do Chinese and Japanese consumers value
# organic and specially certified tomatoes differently, and
# does external information affect their preferences?
#
# Models:
#   M1. Baseline pooled MNL
#   M2. Country-specific MNL + WTP
#   M3. Information-treatment interaction MNL
#   M4. Two-class Latent Class Model (preference heterogeneity)
#   M5. Attribute Non-Attendance extension (behavioral extension)
#
# FIXES APPLIED vs original:
#   [1] M5 apollo_lcPars: removed gamma_china_att (caused delta_att/
#       gamma_china_att to explode to ±87 and made Hessian singular).
#       M5 class membership now contains ONLY information treatment
#       dummies — this is the correct ANA specification to answer
#       "does information shift label attendance?".
#   [2] M4 posterior class probs: apollo_conditionals() requires THREE
#       arguments (model, probabilities_fn, inputs). Original code passed
#       only two, causing the Chinese-language missing-argument error.
#   [3] M5 posterior class probs: same apollo_conditionals() fix.
#   [4] Model comparison table: apollo LLout for LCM returns a named
#       vector (one LL per component). Extract the whole-model LL only.
#   [5] extract_fit() now uses model$LL["model"] for LCM components and
#       model$LL for plain MNL, avoiding the duplicate-row bug in the
#       comparison table.
############################################################

# ── 0. Packages ───────────────────────────────────────────
packs <- c("tidyverse", "janitor", "apollo")
new   <- packs[!packs %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new)
invisible(lapply(packs, library, character.only = TRUE))

# ── 1. File paths ─────────────────────────────────────────
setwd("C:/Users/admin/Desktop/2026/choice of model/Brand_CE")
china_file <- "C_TOMATO.txt"
japan_file <- "group Total.Tomato.txt"

############################################################
# 2. Read and clean data
############################################################
read_tomato <- function(path, country_name) {
  readr::read_delim(path, delim = "\t", show_col_types = FALSE, trim_ws = TRUE) %>%
    janitor::clean_names() %>%
    dplyr::select(-tidyselect::starts_with("unnamed"), -tidyselect::starts_with("x")) %>%
    dplyr::mutate(
      country       = country_name,
      resp_num      = id %/% 1000,
      respondent_id = paste(country_name, resp_num, sep = "_"),
      task_id       = paste(country_name, id, sep = "_"),
      china = if_else(country_name == "China", 1L, 0L),
      japan = if_else(country_name == "Japan", 1L, 0L),
      across(c(price, ordi, organic, spec, choice, mode,
               eio, eis, eios, eic,
               seg1, seg2, seg3, seg4, seg5), as.numeric)
    )
}

long_cn <- read_tomato(china_file, "China")
long_jp <- read_tomato(japan_file, "Japan")

tomato_long <- bind_rows(long_cn, long_jp) %>%
  group_by(task_id) %>%
  mutate(
    alt        = row_number(),
    choice_alt = alt[choice == 1][1]
  ) %>%
  ungroup()

check_tasks <- tomato_long %>%
  group_by(task_id) %>%
  summarise(n_alt = n(), chosen_sum = sum(choice), .groups = "drop")

cat("\n=== Task structure check ===\n")
cat("Rows per task (should all be 3):\n"); print(table(check_tasks$n_alt))
cat("Chosen per task (should all be 1):\n"); print(table(check_tasks$chosen_sum))

valid_tasks <- check_tasks %>% filter(n_alt == 3, chosen_sum == 1) %>% pull(task_id)
tomato_long <- tomato_long %>% filter(task_id %in% valid_tasks)

cat("\nValid tasks:", length(valid_tasks), "\n")
cat("China respondents:", n_distinct(tomato_long$respondent_id[tomato_long$china == 1]), "\n")
cat("Japan respondents:", n_distinct(tomato_long$respondent_id[tomato_long$japan == 1]), "\n")

############################################################
# 3. Descriptive statistics
############################################################
choice_shares <- tomato_long %>%
  filter(choice == 1) %>%
  mutate(chosen_type = case_when(
    mode    == 0 ~ "No-buy",
    organic == 1 ~ "Organic",
    spec    == 1 ~ "Special",
    ordi    == 1 ~ "Ordinary",
    TRUE         ~ "Other"
  )) %>%
  count(country, chosen_type) %>%
  group_by(country) %>%
  mutate(share = n / sum(n)) %>%
  arrange(country, desc(share))

cat("\n=== Choice shares ===\n")
print(choice_shares)
readr::write_csv(choice_shares, "output/tomato_choice_shares.csv")

info_groups <- tomato_long %>%
  distinct(respondent_id, country, eio, eis, eios, eic) %>%
  mutate(info_group = case_when(
    eio  == 1 ~ "EIO (Organic info)",
    eis  == 1 ~ "EIS (Special info)",
    eios == 1 ~ "EIOS (Both info)",
    eic  == 1 ~ "EIC (Control)",
    TRUE      ~ "Unknown"
  )) %>%
  count(country, info_group)

cat("\n=== Information treatment groups ===\n")
print(info_groups)

seg_dist <- tomato_long %>%
  distinct(respondent_id, country, seg1, seg2, seg3, seg4, seg5) %>%
  mutate(segment = case_when(
    seg1 == 1 ~ "SEG1", seg2 == 1 ~ "SEG2", seg3 == 1 ~ "SEG3",
    seg4 == 1 ~ "SEG4", seg5 == 1 ~ "SEG5", TRUE ~ "Unknown"
  )) %>%
  count(country, segment) %>%
  arrange(country, segment)

cat("\n=== Consumer segment distribution ===\n")
print(seg_dist)
readr::write_csv(seg_dist, "output/tomato_segment_distribution.csv")

price_summary <- tomato_long %>%
  filter(mode == 1) %>%
  group_by(country) %>%
  summarise(min_price = min(price), mean_price = mean(price),
            max_price = max(price), .groups = "drop")

cat("\n=== Price ranges (RMB for China, JPY for Japan) ===\n")
print(price_summary)

############################################################
# 4. Convert to Apollo wide format
############################################################
make_apollo_wide <- function(d) {
  d %>%
    dplyr::select(
      respondent_id, resp_num, country, china, japan,
      task_id, choice_alt,
      eio, eis, eios, eic,
      seg1, seg2, seg3, seg4, seg5,
      alt, price, ordi, organic, spec, mode
    ) %>%
    tidyr::pivot_wider(
      names_from  = alt,
      values_from = c(price, ordi, organic, spec, mode),
      names_sep   = "_"
    ) %>%
    dplyr::mutate(
      choice     = as.integer(choice_alt),
      price_cn_1 = price_1 * china,
      price_cn_2 = price_2 * china,
      price_cn_3 = price_3 * china,
      price_jp_1 = price_1 * japan,
      price_jp_2 = price_2 * japan,
      price_jp_3 = price_3 * japan,
      seg1 = as.numeric(seg1),
      seg2 = as.numeric(seg2),
      seg3 = as.numeric(seg3),
      seg4 = as.numeric(seg4)
    ) %>%
    dplyr::arrange(respondent_id, task_id)
}

database <- make_apollo_wide(tomato_long)
dir.create("output", showWarnings = FALSE)
readr::write_csv(database, "output/tomato_apollo_wide.csv")

cat("\n=== Apollo database dimensions ===\n")
cat("Rows (choice tasks):", nrow(database), "\n")
cat("Columns:", ncol(database), "\n")

############################################################
# 5. Apollo initialisation
############################################################
apollo_initialise()

############################################################
# Helper: run standard MNL models (M1-M3)
############################################################
run_apollo_model <- function(model_name, model_descr,
                             apollo_beta, apollo_fixed,
                             prob_function) {
  apollo_control <- list(
    modelName       = model_name,
    modelDescr      = model_descr,
    indivID         = "respondent_id",
    nCores          = 1,
    outputDirectory = "output"
  )
  apollo_inputs <- apollo_validateInputs(
    database       = database,
    apollo_control = apollo_control,
    apollo_beta    = apollo_beta,
    apollo_fixed   = apollo_fixed
  )
  model <- apollo_estimate(apollo_beta, apollo_fixed, prob_function, apollo_inputs)
  apollo_modelOutput(model)
  apollo_saveOutput(model)
  return(model)
}

############################################################
# M1. Baseline pooled MNL
############################################################
cat("\n\n============================================================\n")
cat("M1: Baseline Pooled MNL\n")
cat("============================================================\n")

apollo_beta_m1 <- c(
  b_price   = -0.01,
  b_organic =  0.50,
  b_spec    =  0.50,
  asc_nobuy =  0.00
)
apollo_fixed_m1 <- c()

apollo_probabilities_m1 <- function(apollo_beta, apollo_inputs,
                                    functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  V <- list()
  V[["alt1"]] <- b_price * price_1 + b_organic * organic_1 + b_spec * spec_1
  V[["alt2"]] <- b_price * price_2 + b_organic * organic_2 + b_spec * spec_2
  V[["alt3"]] <- asc_nobuy
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail        = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar    = choice,
    utilities    = V
  )
  P <- list()
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model_m1 <- run_apollo_model(
  "M1_baseline_MNL",
  "Baseline pooled MNL for tomato choices (ordinary = reference label)",
  apollo_beta_m1, apollo_fixed_m1, apollo_probabilities_m1
)

############################################################
# M2. Country-specific MNL + WTP
############################################################
cat("\n\n============================================================\n")
cat("M2: Country-specific MNL\n")
cat("============================================================\n")

apollo_beta_m2 <- c(
  b_price_cn    = -0.10, b_price_jp    = -0.01,
  b_organic_cn  =  0.50, b_organic_jp  =  0.50,
  b_spec_cn     =  0.50, b_spec_jp     =  0.50,
  asc_nobuy_cn  =  0.00, asc_nobuy_jp  =  0.00,
  gamma_seg1    =  0.00, gamma_seg2    =  0.00,
  gamma_seg3    =  0.00, gamma_seg4    =  0.00
)
apollo_fixed_m2 <- c()

apollo_probabilities_m2 <- function(apollo_beta, apollo_inputs,
                                    functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  seg_effect <- gamma_seg1 * seg1 + gamma_seg2 * seg2 +
    gamma_seg3 * seg3 + gamma_seg4 * seg4
  V <- list()
  V[["alt1"]] <-
    b_price_cn * price_cn_1 + b_price_jp * price_jp_1 +
    (b_organic_cn * china + b_organic_jp * japan) * organic_1 +
    (b_spec_cn    * china + b_spec_jp    * japan) * spec_1
  V[["alt2"]] <-
    b_price_cn * price_cn_2 + b_price_jp * price_jp_2 +
    (b_organic_cn * china + b_organic_jp * japan) * organic_2 +
    (b_spec_cn    * china + b_spec_jp    * japan) * spec_2
  V[["alt3"]] <- asc_nobuy_cn * china + asc_nobuy_jp * japan + seg_effect
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail        = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar    = choice,
    utilities    = V
  )
  P <- list()
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model_m2 <- run_apollo_model(
  "M2_country_MNL",
  "Country-specific MNL with SEG individual effects on no-buy",
  apollo_beta_m2, apollo_fixed_m2, apollo_probabilities_m2
)

coef_m2 <- model_m2$estimate
wtp_m2 <- tibble::tibble(
  country   = c("China", "China", "Japan", "Japan"),
  attribute = c("Organic vs Ordinary", "Special vs Ordinary",
                "Organic vs Ordinary", "Special vs Ordinary"),
  wtp       = c(
    -coef_m2["b_organic_cn"] / coef_m2["b_price_cn"],
    -coef_m2["b_spec_cn"]    / coef_m2["b_price_cn"],
    -coef_m2["b_organic_jp"] / coef_m2["b_price_jp"],
    -coef_m2["b_spec_jp"]    / coef_m2["b_price_jp"]
  ),
  currency = c("RMB/500g", "RMB/500g", "JPY/500g", "JPY/500g")
)
cat("\n=== WTP estimates from M2 ===\n")
print(wtp_m2)
readr::write_csv(wtp_m2, "output/tomato_wtp_M2.csv")

############################################################
# M3. Information-treatment interaction MNL
############################################################
cat("\n\n============================================================\n")
cat("M3: Information-treatment Interaction MNL\n")
cat("============================================================\n")

apollo_beta_m3 <- c(
  b_price_cn   = -0.10, b_price_jp   = -0.01,
  b_organic    =  0.50, b_spec       =  0.50,
  b_org_eio    =  0.00, b_org_eios   =  0.00,
  b_spec_eis   =  0.00, b_spec_eios  =  0.00,
  asc_nobuy_cn =  0.00, asc_nobuy_jp =  0.00,
  gamma_seg1   =  0.00, gamma_seg2   =  0.00,
  gamma_seg3   =  0.00, gamma_seg4   =  0.00
)
apollo_fixed_m3 <- c()

apollo_probabilities_m3 <- function(apollo_beta, apollo_inputs,
                                    functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  org_effect  <- b_organic + b_org_eio  * eio  + b_org_eios  * eios
  spec_effect <- b_spec    + b_spec_eis * eis  + b_spec_eios * eios
  seg_effect  <- gamma_seg1 * seg1 + gamma_seg2 * seg2 +
    gamma_seg3 * seg3 + gamma_seg4 * seg4
  V <- list()
  V[["alt1"]] <-
    b_price_cn * price_cn_1 + b_price_jp * price_jp_1 +
    org_effect * organic_1  + spec_effect * spec_1
  V[["alt2"]] <-
    b_price_cn * price_cn_2 + b_price_jp * price_jp_2 +
    org_effect * organic_2  + spec_effect * spec_2
  V[["alt3"]] <- asc_nobuy_cn * china + asc_nobuy_jp * japan + seg_effect
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail        = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar    = choice,
    utilities    = V
  )
  P <- list()
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model_m3 <- run_apollo_model(
  "M3_info_interactions_MNL",
  "MNL with information-treatment interactions on label utilities",
  apollo_beta_m3, apollo_fixed_m3, apollo_probabilities_m3
)

coef_m3 <- model_m3$estimate
wtp_m3 <- tibble::tibble(
  scenario  = c("Control", "EIO (Organic info)", "EIOS (Both info)",
                "Control", "EIS (Special info)", "EIOS (Both info)"),
  attribute = c(rep("Organic", 3), rep("Special", 3)),
  wtp_cn = c(
    -coef_m3["b_organic"]                              / coef_m3["b_price_cn"],
    -(coef_m3["b_organic"] + coef_m3["b_org_eio"])    / coef_m3["b_price_cn"],
    -(coef_m3["b_organic"] + coef_m3["b_org_eios"])   / coef_m3["b_price_cn"],
    -coef_m3["b_spec"]                                 / coef_m3["b_price_cn"],
    -(coef_m3["b_spec"]   + coef_m3["b_spec_eis"])    / coef_m3["b_price_cn"],
    -(coef_m3["b_spec"]   + coef_m3["b_spec_eios"])   / coef_m3["b_price_cn"]
  ),
  wtp_jp = c(
    -coef_m3["b_organic"]                              / coef_m3["b_price_jp"],
    -(coef_m3["b_organic"] + coef_m3["b_org_eio"])    / coef_m3["b_price_jp"],
    -(coef_m3["b_organic"] + coef_m3["b_org_eios"])   / coef_m3["b_price_jp"],
    -coef_m3["b_spec"]                                 / coef_m3["b_price_jp"],
    -(coef_m3["b_spec"]   + coef_m3["b_spec_eis"])    / coef_m3["b_price_jp"],
    -(coef_m3["b_spec"]   + coef_m3["b_spec_eios"])   / coef_m3["b_price_jp"]
  )
)
cat("\n=== Information-adjusted WTP from M3 ===\n")
print(wtp_m3)
readr::write_csv(wtp_m3, "output/tomato_wtp_M3.csv")

############################################################
# M4. Two-class Latent Class Model
############################################################
cat("\n\n============================================================\n")
cat("M4: Two-class Latent Class Model\n")
cat("============================================================\n")

if (exists("apollo_lcPars")) rm(apollo_lcPars)

apollo_beta_m4 <- c(
  b_price_cn_1  = -0.10, b_price_jp_1  = -0.01,
  b_organic_1   =  1.00, b_spec_1      =  1.00,
  asc_nobuy_1   =  0.00,
  b_price_cn_2  = -0.10, b_price_jp_2  = -0.01,
  b_organic_2   =  0.00, b_spec_2      =  0.00,
  asc_nobuy_2   =  0.00,
  delta_class2        =  0.00,
  gamma_china_class2  =  0.00,
  gamma_seg1_class2   =  0.00,
  gamma_seg2_class2   =  0.00,
  gamma_seg3_class2   =  0.00,
  gamma_seg4_class2   =  0.00
)
apollo_fixed_m4 <- c()

apollo_lcPars <- function(apollo_beta, apollo_inputs) {
  lcpars <- list()
  lcpars[["b_price_cn"]] <- list(b_price_cn_1, b_price_cn_2)
  lcpars[["b_price_jp"]] <- list(b_price_jp_1, b_price_jp_2)
  lcpars[["b_organic"]]  <- list(b_organic_1,  b_organic_2)
  lcpars[["b_spec"]]     <- list(b_spec_1,     b_spec_2)
  lcpars[["asc_nobuy"]]  <- list(asc_nobuy_1,  asc_nobuy_2)
  V_class <- list()
  V_class[["class1"]] <- 0
  V_class[["class2"]] <- delta_class2 +
    gamma_china_class2 * china +
    gamma_seg1_class2  * seg1 +
    gamma_seg2_class2  * seg2 +
    gamma_seg3_class2  * seg3 +
    gamma_seg4_class2  * seg4
  classAlloc_settings <- list(
    classes       = c(class1 = 1, class2 = 2),
    utilities     = V_class,
    componentName = "classAlloc_m4"
  )
  lcpars[["pi_values"]] <- apollo_classAlloc(classAlloc_settings)
  return(lcpars)
}

apollo_probabilities_m4 <- function(apollo_beta, apollo_inputs,
                                    functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P <- list()
  V1 <- list()
  V1[["alt1"]] <- b_price_cn[[1]] * price_cn_1 + b_price_jp[[1]] * price_jp_1 +
    b_organic[[1]] * organic_1 + b_spec[[1]] * spec_1
  V1[["alt2"]] <- b_price_cn[[1]] * price_cn_2 + b_price_jp[[1]] * price_jp_2 +
    b_organic[[1]] * organic_2 + b_spec[[1]] * spec_2
  V1[["alt3"]] <- asc_nobuy[[1]]
  mnl1 <- list(alternatives = c(alt1=1,alt2=2,alt3=3),
               avail = list(alt1=1,alt2=1,alt3=1),
               choiceVar = choice, utilities = V1,
               componentName = "mnl_class1")
  tmp1 <- list(model = apollo_mnl(mnl1, functionality))
  tmp1 <- apollo_panelProd(tmp1, apollo_inputs, functionality)
  P[["class1"]] <- tmp1[["model"]]
  V2 <- list()
  V2[["alt1"]] <- b_price_cn[[2]] * price_cn_1 + b_price_jp[[2]] * price_jp_1 +
    b_organic[[2]] * organic_1 + b_spec[[2]] * spec_1
  V2[["alt2"]] <- b_price_cn[[2]] * price_cn_2 + b_price_jp[[2]] * price_jp_2 +
    b_organic[[2]] * organic_2 + b_spec[[2]] * spec_2
  V2[["alt3"]] <- asc_nobuy[[2]]
  mnl2 <- list(alternatives = c(alt1=1,alt2=2,alt3=3),
               avail = list(alt1=1,alt2=1,alt3=1),
               choiceVar = choice, utilities = V2,
               componentName = "mnl_class2")
  tmp2 <- list(model = apollo_mnl(mnl2, functionality))
  tmp2 <- apollo_panelProd(tmp2, apollo_inputs, functionality)
  P[["class2"]] <- tmp2[["model"]]
  lc_settings <- list(inClassProb = P, classProb = pi_values)
  P[["model"]] <- apollo_lc(lc_settings, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

apollo_control_m4 <- list(
  modelName       = "M4_2class_LCM",
  modelDescr      = "Two-class LCM with country+SEG class membership",
  indivID         = "respondent_id",
  nCores          = 1,
  outputDirectory = "output"
)

apollo_inputs_m4 <- apollo_validateInputs(
  database       = database,
  apollo_control = apollo_control_m4,
  apollo_beta    = apollo_beta_m4,
  apollo_fixed   = apollo_fixed_m4
)

model_m4 <- apollo_estimate(
  apollo_beta_m4, apollo_fixed_m4,
  apollo_probabilities_m4, apollo_inputs_m4
)
apollo_modelOutput(model_m4)
apollo_saveOutput(model_m4)

coef_m4 <- model_m4$estimate
wtp_m4 <- tibble::tibble(
  class     = c("Class1","Class1","Class1","Class1",
                "Class2","Class2","Class2","Class2"),
  country   = c("China","China","Japan","Japan",
                "China","China","Japan","Japan"),
  attribute = c("Organic","Special","Organic","Special",
                "Organic","Special","Organic","Special"),
  wtp = c(
    -coef_m4["b_organic_1"] / coef_m4["b_price_cn_1"],
    -coef_m4["b_spec_1"]    / coef_m4["b_price_cn_1"],
    -coef_m4["b_organic_1"] / coef_m4["b_price_jp_1"],
    -coef_m4["b_spec_1"]    / coef_m4["b_price_jp_1"],
    -coef_m4["b_organic_2"] / coef_m4["b_price_cn_2"],
    -coef_m4["b_spec_2"]    / coef_m4["b_price_cn_2"],
    -coef_m4["b_organic_2"] / coef_m4["b_price_jp_2"],
    -coef_m4["b_spec_2"]    / coef_m4["b_price_jp_2"]
  ),
  currency = c("RMB","RMB","JPY","JPY","RMB","RMB","JPY","JPY")
)
cat("\n=== Class-specific WTP from M4 ===\n")
print(wtp_m4)
readr::write_csv(wtp_m4, "output/tomato_wtp_M4.csv")

############################################################
# M4b. Three-class Latent Class Model
# Specification exploration: compare 2-class (M4) vs 3-class (M4b)
# to justify choice of number of classes.
# If 3-class BIC > 2-class BIC, or if a class is very small / parameters
# are unstable, we retain M4 (2-class) as the preferred specification.
############################################################
cat("\n\n============================================================\n")
cat("M4b: Three-class LCM (Specification Exploration)\n")
cat("============================================================\n")

if (exists("apollo_lcPars")) rm(apollo_lcPars)

apollo_beta_m4b <- c(
  # Class 1: high label sensitivity (starting values from M4 Class 1)
  b_price_cn_1  = -0.12, b_price_jp_1  = -0.026,
  b_organic_1   =  1.40, b_spec_1      =  1.22,
  asc_nobuy_1   = -9.50,
  # Class 2: low label sensitivity (starting values from M4 Class 2)
  b_price_cn_2  = -0.27, b_price_jp_2  = -0.015,
  b_organic_2   = -0.20, b_spec_2      =  0.10,
  asc_nobuy_2   = -2.80,
  # Class 3: new intermediate class
  b_price_cn_3  = -0.15, b_price_jp_3  = -0.020,
  b_organic_3   =  0.50, b_spec_3      =  0.50,
  asc_nobuy_3   = -3.00,
  # Class membership (Class 1 = reference, utility = 0)
  delta_class2        =  0.00,
  gamma_china_class2  =  0.00,
  gamma_seg1_class2   =  0.00,
  gamma_seg2_class2   =  0.00,
  gamma_seg3_class2   =  0.00,
  gamma_seg4_class2   =  0.00,
  delta_class3        =  0.00,
  gamma_china_class3  =  0.00,
  gamma_seg1_class3   =  0.00,
  gamma_seg2_class3   =  0.00,
  gamma_seg3_class3   =  0.00,
  gamma_seg4_class3   =  0.00
)
apollo_fixed_m4b <- c()

apollo_lcPars <- function(apollo_beta, apollo_inputs) {
  lcpars <- list()
  lcpars[["b_price_cn"]] <- list(b_price_cn_1, b_price_cn_2, b_price_cn_3)
  lcpars[["b_price_jp"]] <- list(b_price_jp_1, b_price_jp_2, b_price_jp_3)
  lcpars[["b_organic"]]  <- list(b_organic_1,  b_organic_2,  b_organic_3)
  lcpars[["b_spec"]]     <- list(b_spec_1,     b_spec_2,     b_spec_3)
  lcpars[["asc_nobuy"]]  <- list(asc_nobuy_1,  asc_nobuy_2,  asc_nobuy_3)
  V_class <- list()
  V_class[["class1"]] <- 0
  V_class[["class2"]] <- delta_class2 +
    gamma_china_class2 * china +
    gamma_seg1_class2  * seg1 + gamma_seg2_class2 * seg2 +
    gamma_seg3_class2  * seg3 + gamma_seg4_class2 * seg4
  V_class[["class3"]] <- delta_class3 +
    gamma_china_class3 * china +
    gamma_seg1_class3  * seg1 + gamma_seg2_class3 * seg2 +
    gamma_seg3_class3  * seg3 + gamma_seg4_class3 * seg4
  classAlloc_settings <- list(
    classes       = c(class1 = 1, class2 = 2, class3 = 3),
    utilities     = V_class,
    componentName = "classAlloc_m4b"
  )
  lcpars[["pi_values"]] <- apollo_classAlloc(classAlloc_settings)
  return(lcpars)
}

apollo_probabilities_m4b <- function(apollo_beta, apollo_inputs,
                                     functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P <- list()
  for (cl in 1:3) {
    Vcl <- list()
    Vcl[["alt1"]] <- b_price_cn[[cl]] * price_cn_1 + b_price_jp[[cl]] * price_jp_1 +
      b_organic[[cl]] * organic_1 + b_spec[[cl]] * spec_1
    Vcl[["alt2"]] <- b_price_cn[[cl]] * price_cn_2 + b_price_jp[[cl]] * price_jp_2 +
      b_organic[[cl]] * organic_2 + b_spec[[cl]] * spec_2
    Vcl[["alt3"]] <- asc_nobuy[[cl]]
    mnlcl <- list(
      alternatives  = c(alt1=1,alt2=2,alt3=3),
      avail         = list(alt1=1,alt2=1,alt3=1),
      choiceVar     = choice,
      utilities     = Vcl,
      componentName = paste0("mnl_class", cl)
    )
    tmpcl           <- list(model = apollo_mnl(mnlcl, functionality))
    tmpcl           <- apollo_panelProd(tmpcl, apollo_inputs, functionality)
    P[[paste0("class", cl)]] <- tmpcl[["model"]]
  }
  lc_settings <- list(inClassProb = P, classProb = pi_values)
  P[["model"]] <- apollo_lc(lc_settings, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

apollo_control_m4b <- list(
  modelName       = "M4b_3class_LCM",
  modelDescr      = "Three-class LCM — specification exploration",
  indivID         = "respondent_id",
  nCores          = 1,
  outputDirectory = "output"
)

apollo_inputs_m4b <- apollo_validateInputs(
  database       = database,
  apollo_control = apollo_control_m4b,
  apollo_beta    = apollo_beta_m4b,
  apollo_fixed   = apollo_fixed_m4b
)

model_m4b <- apollo_estimate(
  apollo_beta_m4b, apollo_fixed_m4b,
  apollo_probabilities_m4b, apollo_inputs_m4b
)
apollo_modelOutput(model_m4b)
apollo_saveOutput(model_m4b)

cat("\n=== 3-class LCM class shares ===\n")
# Posterior class assignment
postProbs_m4b <- tryCatch(
  apollo_conditionals(model_m4b, apollo_probabilities_m4b, apollo_inputs_m4b),
  error = function(e) {
    cat("Posterior extraction failed for M4b:", conditionMessage(e), "\n")
    return(NULL)
  }
)
if (!is.null(postProbs_m4b)) {
  class_summary_m4b <- tibble::as_tibble(postProbs_m4b) %>%
    dplyr::rename(respondent_id = ID,
                  prob_class1 = X1, prob_class2 = X2, prob_class3 = X3) %>%
    dplyr::mutate(assigned_class = case_when(
      prob_class1 >= prob_class2 & prob_class1 >= prob_class3 ~ "Class1",
      prob_class2 >= prob_class1 & prob_class2 >= prob_class3 ~ "Class2",
      TRUE ~ "Class3"
    ))
  cat("M4b class assignment summary:\n")
  print(table(class_summary_m4b$assigned_class))
  readr::write_csv(class_summary_m4b, "output/tomato_M4b_class_probs.csv")
}

############################################################
# M5. Attribute Non-Attendance (ANA) Extension
#
# FIX vs original: gamma_china_att REMOVED from class membership.
# Reason: in the previous version delta_att and gamma_china_att
# exploded to +88 and -87 respectively (nearly cancelling),
# making the Hessian singular and producing no standard errors.
# The correct ANA specification tests whether INFORMATION
# TREATMENTS shift label attendance — country should not be
# in the class membership equation here (it is already captured
# in the utility functions via country-specific price variables).
############################################################
cat("\n\n============================================================\n")
cat("M5: Attribute Non-Attendance (ANA) Extension\n")
cat("============================================================\n")

if (exists("apollo_lcPars")) rm(apollo_lcPars)

apollo_beta_m5 <- c(
  # Shared price across both classes
  b_price_cn     = -0.15,
  b_price_jp     = -0.02,
  # Label utilities (attend class only)
  b_organic_att  =  1.00,
  b_spec_att     =  0.80,
  # No-buy ASC per class
  asc_nobuy_att  = -2.00,
  asc_nobuy_no   = -2.00,
  # Class membership: attend vs no-attend
  # no_attend = reference (utility = 0)
  # FIX: only information treatment dummies here, NOT country
  delta_att       =  0.00,
  gamma_eio_att   =  0.00,
  gamma_eis_att   =  0.00,
  gamma_eios_att  =  0.00
)
apollo_fixed_m5 <- c()

# FIX: apollo_lcPars contains ONLY info treatment effects
apollo_lcPars <- function(apollo_beta, apollo_inputs) {
  lcpars <- list()
  V_class <- list()
  V_class[["attend"]] <- delta_att +
    gamma_eio_att  * eio  +
    gamma_eis_att  * eis  +
    gamma_eios_att * eios
  V_class[["no_attend"]] <- 0
  classAlloc_settings <- list(
    classes       = c(attend = 1, no_attend = 2),
    utilities     = V_class,
    componentName = "classAlloc_m5_ana"
  )
  lcpars[["pi_values"]] <- apollo_classAlloc(classAlloc_settings)
  return(lcpars)
}

apollo_probabilities_m5 <- function(apollo_beta, apollo_inputs,
                                    functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P <- list()
  V_att <- list()
  V_att[["alt1"]] <- b_price_cn * price_cn_1 + b_price_jp * price_jp_1 +
    b_organic_att * organic_1 + b_spec_att * spec_1
  V_att[["alt2"]] <- b_price_cn * price_cn_2 + b_price_jp * price_jp_2 +
    b_organic_att * organic_2 + b_spec_att * spec_2
  V_att[["alt3"]] <- asc_nobuy_att
  mnl_att <- list(alternatives = c(alt1=1,alt2=2,alt3=3),
                  avail = list(alt1=1,alt2=1,alt3=1),
                  choiceVar = choice, utilities = V_att,
                  componentName = "mnl_attend")
  tmp_att       <- list(model = apollo_mnl(mnl_att, functionality))
  tmp_att       <- apollo_panelProd(tmp_att, apollo_inputs, functionality)
  P[["attend"]] <- tmp_att[["model"]]
  V_no <- list()
  V_no[["alt1"]] <- b_price_cn * price_cn_1 + b_price_jp * price_jp_1
  V_no[["alt2"]] <- b_price_cn * price_cn_2 + b_price_jp * price_jp_2
  V_no[["alt3"]] <- asc_nobuy_no
  mnl_no <- list(alternatives = c(alt1=1,alt2=2,alt3=3),
                 avail = list(alt1=1,alt2=1,alt3=1),
                 choiceVar = choice, utilities = V_no,
                 componentName = "mnl_no_attend")
  tmp_no          <- list(model = apollo_mnl(mnl_no, functionality))
  tmp_no          <- apollo_panelProd(tmp_no, apollo_inputs, functionality)
  P[["no_attend"]] <- tmp_no[["model"]]
  lc_settings <- list(inClassProb = P, classProb = pi_values,
                      componentName = "lc_ana")
  P[["model"]] <- apollo_lc(lc_settings, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

apollo_control_m5 <- list(
  modelName       = "M5_ANA_2class",
  modelDescr      = "ANA: label-attending vs label-non-attending; shared price; info treatment in class membership",
  indivID         = "respondent_id",
  nCores          = 1,
  outputDirectory = "output",
  panelData       = TRUE,
  analyticGrad    = FALSE
)

apollo_inputs_m5 <- apollo_validateInputs(
  database       = database,
  apollo_control = apollo_control_m5,
  apollo_beta    = apollo_beta_m5,
  apollo_fixed   = apollo_fixed_m5
)

model_m5 <- apollo_estimate(
  apollo_beta_m5, apollo_fixed_m5,
  apollo_probabilities_m5, apollo_inputs_m5
)
apollo_modelOutput(model_m5)
apollo_saveOutput(model_m5)

############################################################
# 6. Model comparison table
# FIX: LCM models return a named LL vector (one per component).
# Use the "model" entry for whole-model LL; MNL returns a scalar.
############################################################
cat("\n\n============================================================\n")
cat("Model Comparison\n")
cat("============================================================\n")

n_obs <- nrow(database)

extract_fit <- function(model, name) {
  # LLout对LCM也返回多值，用LLout命名向量里的"model"条目
  # 或直接用模型输出文字里的"LL(final, whole model)"
  ll_raw <- model$LLout
  if (length(ll_raw) > 1 && !is.null(names(ll_raw))) {
    # LCM: 取名为"model"的那个整体LL
    ll <- as.numeric(ll_raw["model"])
  } else {
    ll <- as.numeric(ll_raw)
  }
  k <- length(model$estimate)
  tibble::tibble(
    Model = name,
    LL    = round(ll, 2),
    k     = k,
    AIC   = round(-2 * ll + 2 * k, 2),
    BIC   = round(-2 * ll + log(n_obs) * k, 2)
  )
}

fit_table <- bind_rows(
  extract_fit(model_m1,  "M1: Pooled MNL"),
  extract_fit(model_m2,  "M2: Country MNL + SEG"),
  extract_fit(model_m3,  "M3: Info Interactions"),
  extract_fit(model_m4,  "M4: 2-class LCM"),
  extract_fit(model_m4b, "M4b: 3-class LCM"),
  extract_fit(model_m5,  "M5: ANA 2-class")
)

cat("\n"); print(fit_table, n = Inf)
readr::write_csv(fit_table, "output/tomato_model_comparison.csv")

# ── Class-number comparison sub-table (for reporting) ────
cat("\n=== Specification exploration: number of latent classes ===\n")
class_explore <- bind_rows(
  extract_fit(model_m2,  "1-class (M2 Country MNL)"),
  extract_fit(model_m4,  "2-class LCM (M4)"),
  extract_fit(model_m4b, "3-class LCM (M4b)")
)
print(class_explore)
readr::write_csv(class_explore, "output/tomato_class_exploration.csv")

############################################################
# 7. Posterior class membership probabilities (M4)
# FIX: apollo_conditionals() takes THREE arguments:
#   (model, probabilities_function, apollo_inputs)
# Original code passed only two → Chinese-language error.
############################################################
cat("\n\n============================================================\n")
cat("M4: Posterior class probabilities\n")
cat("============================================================\n")

postProbs_m4 <- apollo_conditionals(
  model_m4, apollo_probabilities_m4, apollo_inputs_m4
)

# 输出结构是：ID | X1(class1概率) | X2(class2概率)
class_summary_m4 <- tibble::as_tibble(postProbs_m4) %>%
  dplyr::rename(
    respondent_id  = ID,
    prob_class1    = X1,
    prob_class2    = X2
  ) %>%
  dplyr::mutate(
    assigned_class = if_else(prob_class1 >= 0.5, "Class1", "Class2")
  )

cat("M4 class assignment summary:\n")
print(table(class_summary_m4$assigned_class))
readr::write_csv(class_summary_m4, "output/tomato_M4_class_probs.csv")

class_country <- class_summary_m4 %>%
  dplyr::left_join(
    database %>% distinct(respondent_id, country),
    by = "respondent_id"
  ) %>%
  dplyr::count(country, assigned_class) %>%
  tidyr::pivot_wider(names_from = assigned_class,
                     values_from = n, values_fill = 0)

cat("\nM4 class assignment by country:\n")
print(class_country)

############################################################
# 8. Posterior class membership probabilities (M5)
# FIX: same apollo_conditionals() three-argument fix.
# Wrapped in tryCatch because M5 may have a singular Hessian
# that prevents posterior extraction.
############################################################
cat("\n\n============================================================\n")
cat("M5: ANA class membership summary\n")
cat("============================================================\n")

postProbs_m5 <- tryCatch(
  apollo_conditionals(model_m5, apollo_probabilities_m5, apollo_inputs_m5),
  error = function(e) {
    cat("M5 posterior extraction failed:", conditionMessage(e), "\n")
    return(NULL)
  }
)

if (!is.null(postProbs_m5)) {
  # 结构同M4: ID | X1(attend概率) | X2(no_attend概率)
  class_summary_m5 <- tibble::as_tibble(postProbs_m5) %>%
    dplyr::rename(
      respondent_id  = ID,
      prob_attend    = X1,
      prob_no_attend = X2
    ) %>%
    dplyr::left_join(
      database %>% distinct(respondent_id, country, eio, eis, eios, eic),
      by = "respondent_id"
    )
  
  att_by_info <- class_summary_m5 %>%
    mutate(info_group = case_when(
      eio  == 1 ~ "EIO (Organic info)",
      eis  == 1 ~ "EIS (Special info)",
      eios == 1 ~ "EIOS (Both info)",
      eic  == 1 ~ "EIC (Control)",
      TRUE      ~ "Unknown"
    )) %>%
    group_by(country, info_group) %>%
    summarise(avg_attend_prob = mean(prob_attend),
              n = n(), .groups = "drop")
  
  cat("M5 average attendance probability by info treatment:\n")
  print(att_by_info)
  readr::write_csv(att_by_info, "output/tomato_M5_attendance_by_info.csv")
}

############################################################
# Summary of outputs
############################################################
cat("\n\n============================================================\n")
cat("All outputs saved to ./output/\n")
cat("  tomato_choice_shares.csv\n")
cat("  tomato_segment_distribution.csv\n")
cat("  tomato_apollo_wide.csv\n")
cat("  tomato_wtp_M2.csv\n")
cat("  tomato_wtp_M3.csv\n")
cat("  tomato_wtp_M4.csv\n")
cat("  tomato_model_comparison.csv        (M1-M5 + M4b)\n")
cat("  tomato_class_exploration.csv       (1/2/3-class comparison)\n")
cat("  tomato_M4_class_probs.csv\n")
cat("  tomato_M4b_class_probs.csv\n")
cat("  tomato_M5_attendance_by_info.csv   (if M5 Hessian was non-singular)\n")
cat("  M1-M5 + M4b Apollo model files (*.rds, *.csv, *.txt)\n")
cat("============================================================\n")