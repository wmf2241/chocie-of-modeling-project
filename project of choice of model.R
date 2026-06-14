# project of choice of model
# Do Chinese and Japanese consumers value organic and specially certified tomatoes differently, 
#  and does external information affect their preferences?

# Main models:
#   M1. Baseline MNL
#   M2. Country-specific MNL
#   M3. Information-treatment interaction model
#   M4. 2-class Latent Class Model
#   M5. Attribute non-attendance extension

# package
packs <- c("tidyverse", "janitor", "apollo")
new <- packs[!packs %in% installed.packages()[, "Package"]]
if(length(new)) install.packages(new)
invisible(lapply(packs, library, character.only = TRUE))

# load the files
setwd("C:/Users/admin/Desktop/2026/choice of model/Brand_CE")

china_file <- "C_TOMATO.txt"
japan_file <- "group Total.Tomato.txt"

#############################
# 1. Read and clean tomato data
#############################
read_tomato <- function(path, country_name){
  readr::read_delim(path, delim = "\t", show_col_types = FALSE, trim_ws = TRUE) %>%
    janitor::clean_names() %>%
    dplyr::select(-tidyselect::starts_with("x")) %>%  # remove blank columns from txt export, if any
    dplyr::mutate(
      country = country_name,
      task_id = paste(country_name, id, sep = "_"),
      # ID is coded like 1001, 1002, ..., 2001, 2002, ...
      # floor(ID/1000) gives respondent number; each respondent has 8 tomato tasks.
      respondent_num = id %/% 1000,
      respondent_id = paste(country_name, respondent_num, sep = "_"),
      china = if_else(country_name == "China", 1, 0),
      japan = if_else(country_name == "Japan", 1, 0),
      across(c(price, ordi, organic, spec, choice, mode, eio, eis, eios, eic), as.numeric)
    )
}

long_cn <- read_tomato(china_file, "China")
long_jp <- read_tomato(japan_file, "Japan")

tomato_long <- bind_rows(long_cn, long_jp) %>%
  group_by(task_id) %>%
  mutate(
    alt = row_number(),                 # 1 = product A, 2 = product B, 3 = no-buy/opt-out
    choice_alt = alt[choice == 1][1]
  ) %>%
  ungroup()

# Basic checks: each choice task should have 3 rows and exactly one chosen alternative
check_tasks <- tomato_long %>%
  group_by(task_id) %>%
  summarise(n_alt = n(), chosen_sum = sum(choice), .groups = "drop")

print(table(check_tasks$n_alt))
print(table(check_tasks$chosen_sum))

# Keep only valid choice tasks
valid_tasks <- check_tasks %>% filter(n_alt == 3, chosen_sum == 1) %>% pull(task_id)
tomato_long <- tomato_long %>% filter(task_id %in% valid_tasks)

#############################
# 2. Convert long format to Apollo wide format
#############################
make_apollo_wide <- function(d){
  d %>%
    select(
      respondent_id, respondent_num, country, china, japan,
      task_id, choice_alt,
      eio, eis, eios, eic,
      alt, price, ordi, organic, spec, mode
    ) %>%
    pivot_wider(
      names_from = alt,
      values_from = c(price, ordi, organic, spec, mode),
      names_sep = "_"
    ) %>%
    mutate(
      choice = as.integer(choice_alt),
      # Country-specific price variables avoid mixing RMB and JPY in one price coefficient.
      price_cn_1 = price_1 * china,
      price_cn_2 = price_2 * china,
      price_cn_3 = price_3 * china,
      price_jp_1 = price_1 * japan,
      price_jp_2 = price_2 * japan,
      price_jp_3 = price_3 * japan
    ) %>%
    arrange(respondent_id, task_id)
}

database <- make_apollo_wide(tomato_long)

# Save cleaned data for your report appendix or debugging
readr::write_csv(database, "tomato_apollo_wide.csv")

# Descriptive choice shares: useful for the data section of your report
choice_shares <- tomato_long %>%
  filter(choice == 1) %>%
  mutate(chosen_type = case_when(
    organic == 1 ~ "Organic",
    spec == 1 ~ "Special",
    ordi == 1 ~ "Ordinary",
    mode == 0 ~ "No-buy",
    TRUE ~ "Other"
  )) %>%
  count(country, chosen_type) %>%
  group_by(country) %>%
  mutate(share = n / sum(n)) %>%
  arrange(country, desc(share))

print(choice_shares)
readr::write_csv(choice_shares, "tomato_choice_shares.csv")

############################################################
# Apollo setup helper
############################################################
apollo_initialise()

run_apollo_model <- function(model_name, model_descr, apollo_beta, apollo_fixed, prob_function){
  apollo_control <- list(
    modelName  = model_name,
    modelDescr = model_descr,
    indivID    = "respondent_id",
    nCores     = 1,
    outputDirectory = "output"
  )
  apollo_inputs <- apollo_validateInputs(
    database = database,
    apollo_control = apollo_control,
    apollo_beta = apollo_beta,
    apollo_fixed = apollo_fixed
  )
  model <- apollo_estimate(apollo_beta, apollo_fixed, prob_function, apollo_inputs)
  apollo_modelOutput(model)
  apollo_saveOutput(model)
  return(model)
}

############################################################
# M1. Baseline pooled MNL
# Ordinary tomato is the omitted label baseline.
# organic and spec are interpreted relative to ordinary, conditional on price.
############################################################
apollo_beta_m1 <- c(
  b_price = -0.01,
  b_organic = 0,
  b_spec = 0,
  asc_nobuy = 0
)
apollo_fixed_m1 <- c()

apollo_probabilities_m1 <- function(apollo_beta, apollo_inputs, functionality = "estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
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
  
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model_m1 <- run_apollo_model(
  "M1_baseline_MNL",
  "Baseline pooled MNL for tomato choices",
  apollo_beta_m1,
  apollo_fixed_m1,
  apollo_probabilities_m1
)

############################################################
# M2. Country-specific MNL
# This avoids mixing Chinese and Japanese price units.
############################################################
apollo_beta_m2 <- c(
  b_price_cn = -0.10,
  b_price_jp = -0.01,
  b_organic_cn = 0,
  b_organic_jp = 0,
  b_spec_cn = 0,
  b_spec_jp = 0,
  asc_nobuy_cn = 0,
  asc_nobuy_jp = 0
)
apollo_fixed_m2 <- c()

apollo_probabilities_m2 <- function(apollo_beta, apollo_inputs, functionality = "estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  V <- list()
  
  V[["alt1"]] <-
    b_price_cn * price_cn_1 + b_price_jp * price_jp_1 +
    (b_organic_cn * china + b_organic_jp * japan) * organic_1 +
    (b_spec_cn    * china + b_spec_jp    * japan) * spec_1
  
  V[["alt2"]] <-
    b_price_cn * price_cn_2 + b_price_jp * price_jp_2 +
    (b_organic_cn * china + b_organic_jp * japan) * organic_2 +
    (b_spec_cn    * china + b_spec_jp    * japan) * spec_2
  
  V[["alt3"]] <- asc_nobuy_cn * china + asc_nobuy_jp * japan
  
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail        = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar    = choice,
    utilities    = V
  )
  
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model_m2 <- run_apollo_model(
  "M2_country_MNL",
  "Country-specific MNL for tomato choices",
  apollo_beta_m2,
  apollo_fixed_m2,
  apollo_probabilities_m2
)

# WTP in local price units, based on country-specific coefficients.
# Organic and Special WTP are relative to ordinary tomatoes.
coef_m2 <- model_m2$estimate
wtp_m2 <- tibble(
  country = c("China", "China", "Japan", "Japan"),
  attribute = c("Organic", "Special", "Organic", "Special"),
  wtp = c(
    -coef_m2["b_organic_cn"] / coef_m2["b_price_cn"],
    -coef_m2["b_spec_cn"]    / coef_m2["b_price_cn"],
    -coef_m2["b_organic_jp"] / coef_m2["b_price_jp"],
    -coef_m2["b_spec_jp"]    / coef_m2["b_price_jp"]
  )
)
print(wtp_m2)
readr::write_csv(wtp_m2, "tomato_wtp_M2.csv")

############################################################
# M3. Information-treatment interaction model
# This is the simple empirical version of the teacher's suggestion:
# check whether information treatments increase response to label attributes.
#
# Interpretation:
#   b_org_eio  > 0: organic information increases attention/preference for organic label.
#   b_spec_eis > 0: special/safety information increases attention/preference for special label.
#   b_org_eios and b_spec_eios show combined information effects.
############################################################
apollo_beta_m3 <- c(
  b_price_cn = -0.10,
  b_price_jp = -0.01,
  b_organic = 0,
  b_spec = 0,
  b_org_eio = 0,
  b_org_eios = 0,
  b_spec_eis = 0,
  b_spec_eios = 0,
  asc_nobuy = 0
)
apollo_fixed_m3 <- c()

apollo_probabilities_m3 <- function(apollo_beta, apollo_inputs, functionality = "estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  V <- list()
  
  org_effect  <- b_organic + b_org_eio * eio + b_org_eios * eios
  spec_effect <- b_spec    + b_spec_eis * eis + b_spec_eios * eios
  
  V[["alt1"]] <- b_price_cn * price_cn_1 + b_price_jp * price_jp_1 +
    org_effect * organic_1 + spec_effect * spec_1
  
  V[["alt2"]] <- b_price_cn * price_cn_2 + b_price_jp * price_jp_2 +
    org_effect * organic_2 + spec_effect * spec_2
  
  V[["alt3"]] <- asc_nobuy
  
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail        = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar    = choice,
    utilities    = V
  )
  
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model_m3 <- run_apollo_model(
  "M3_info_interactions_MNL",
  "MNL with information-treatment interactions",
  apollo_beta_m3,
  apollo_fixed_m3,
  apollo_probabilities_m3
)

############################################################
# M4. Two-class Latent Class Model
# This satisfies the preference heterogeneity requirement.
# Class membership is allowed to differ by country.
############################################################
apollo_beta_lc <- c(
  # Class 1
  b_price_cn_1 = -0.10,
  b_price_jp_1 = -0.01,
  b_organic_1 = 0,
  b_spec_1 = 0,
  asc_nobuy_1 = 0,
  
  # Class 2
  b_price_cn_2 = -0.10,
  b_price_jp_2 = -0.01,
  b_organic_2 = 0,
  b_spec_2 = 0,
  asc_nobuy_2 = 0,
  
  # Class allocation: class 1 is the reference class, class 2 utility is below
  delta_class2 = 0,
  gamma_china_class2 = 0
)
apollo_fixed_lc <- c()

apollo_control_lc <- list(
  modelName  = "M4_2class_LCM",
  modelDescr = "Two-class latent class model for tomato choices",
  indivID    = "respondent_id",
  nCores     = 1,
  outputDirectory = "output"
)

apollo_lcPars <- function(apollo_beta, apollo_inputs){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  lcpars <- list()
  lcpars[["b_price_cn"]] <- list(b_price_cn_1, b_price_cn_2)
  lcpars[["b_price_jp"]] <- list(b_price_jp_1, b_price_jp_2)
  lcpars[["b_organic"]]  <- list(b_organic_1,  b_organic_2)
  lcpars[["b_spec"]]     <- list(b_spec_1,     b_spec_2)
  lcpars[["asc_nobuy"]]  <- list(asc_nobuy_1,  asc_nobuy_2)
  
  V <- list()
  V[["class1"]] <- 0
  V[["class2"]] <- delta_class2 + gamma_china_class2 * china
  
  classAlloc_settings <- list(
    classes   = c(class1 = 1, class2 = 2),
    utilities = V
  )
  
  lcpars[["pi_values"]] <- apollo_classAlloc(classAlloc_settings)
  return(lcpars)
}

apollo_probabilities_lc <- function(apollo_beta, apollo_inputs, functionality = "estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  
  for(s in 1:2){
    V <- list()
    
    V[["alt1"]] <-
      b_price_cn[[s]] * price_cn_1 + b_price_jp[[s]] * price_jp_1 +
      b_organic[[s]] * organic_1 + b_spec[[s]] * spec_1
    
    V[["alt2"]] <-
      b_price_cn[[s]] * price_cn_2 + b_price_jp[[s]] * price_jp_2 +
      b_organic[[s]] * organic_2 + b_spec[[s]] * spec_2
    
    V[["alt3"]] <- asc_nobuy[[s]]
    
    mnl_settings <- list(
      alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
      avail        = list(alt1 = 1, alt2 = 1, alt3 = 1),
      choiceVar    = choice,
      utilities    = V
    )
    
    P[[paste0("class", s)]] <- apollo_mnl(mnl_settings, functionality)
    P[[paste0("class", s)]] <- apollo_panelProd(P[[paste0("class", s)]], apollo_inputs, functionality)
  }
  
  lc_settings <- list(
    inClassProb = P,
    classProb   = pi_values
  )
  
  P[["model"]] <- apollo_lc(lc_settings, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

apollo_inputs_lc <- apollo_validateInputs(
  database = database,
  apollo_control = apollo_control_lc,
  apollo_beta = apollo_beta_lc,
  apollo_fixed = apollo_fixed_lc
)

model_lc <- apollo_estimate(apollo_beta_lc, apollo_fixed_lc, apollo_probabilities_lc, apollo_inputs_lc)
apollo_modelOutput(model_lc)
apollo_saveOutput(model_lc)

############################################################
# M5. Simplified 2-class Attribute Non-Attendance Model
# Class 1: attends to labels: PRICE + ORGANIC + SPEC
# Class 2: does not attend to labels: PRICE only
############################################################

############################################################
# M5 simple ANA: 2-class label-attendance model
# Class 1 = label-attending consumers: PRICE + ORGANIC + SPEC
# Class 2 = label-non-attending consumers: PRICE only
############################################################

# Important: clear old LC functions
if (exists("apollo_lcPars")) rm(apollo_lcPars)
if (exists("apollo_probabilities_ana")) rm(apollo_probabilities_ana)

# Sometimes helps with Apollo symbolic checks
options(expressions = 500000)

apollo_beta_ana <- c(
  # Class 1: label attending
  b_price_cn_att = -0.15,
  b_price_jp_att = -0.02,
  b_organic_att  =  1.00,
  b_spec_att     =  0.80,
  asc_nobuy_att  = -3.00,
  
  # Class 2: label non-attending / price-focused
  b_price_cn_no  = -0.15,
  b_price_jp_no  = -0.02,
  asc_nobuy_no   = -3.00,
  
  # Class allocation
  # Class 2 is reference; delta_att controls size of class 1
  delta_att = 0
)

apollo_fixed_ana <- c()

apollo_control_ana <- list(
  modelName       = "M5_simple_ANA_2class",
  modelDescr      = "Simple 2-class ANA model: label-attending vs label-non-attending",
  indivID         = "respondent_id",
  nCores          = 1,
  outputDirectory = "output",
  panelData       = TRUE,
  analyticGrad    = FALSE
)

############################################################
# Latent class parameters
############################################################

if (exists("apollo_lcPars")) rm(apollo_lcPars)

apollo_lcPars <- function(apollo_beta, apollo_inputs){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  lcpars <- list()
  
  V <- list()
  V[["attend"]]    <- delta_att
  V[["no_attend"]] <- 0
  
  classAlloc_settings <- list(
    classes       = c(attend = 1, no_attend = 2),
    utilities     = V,
    componentName = "classAlloc_ana"
  )
  
  lcpars[["pi_values"]] <- apollo_classAlloc(classAlloc_settings)
  
  return(lcpars)
}

############################################################
# Probability function for simple 2-class ANA model
############################################################

if (exists("apollo_probabilities_ana")) rm(apollo_probabilities_ana)

apollo_probabilities_ana <- function(apollo_beta, apollo_inputs, functionality = "estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  
  ##########################################################
  # Class 1: label-attending
  ##########################################################
  
  V_att <- list()
  
  V_att[["alt1"]] <-
    b_price_cn_att * price_cn_1 +
    b_price_jp_att * price_jp_1 +
    b_organic_att  * organic_1 +
    b_spec_att     * spec_1
  
  V_att[["alt2"]] <-
    b_price_cn_att * price_cn_2 +
    b_price_jp_att * price_jp_2 +
    b_organic_att  * organic_2 +
    b_spec_att     * spec_2
  
  V_att[["alt3"]] <- asc_nobuy_att
  
  mnl_settings_att <- list(
    alternatives  = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail         = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar     = choice,
    utilities     = V_att,
    componentName = "mnl_attend"
  )
  
  P_att <- list()
  P_att[["model"]] <- apollo_mnl(mnl_settings_att, functionality)
  P_att <- apollo_panelProd(P_att, apollo_inputs, functionality)
  
  P[["attend"]] <- P_att[["model"]]
  
  
  ##########################################################
  # Class 2: label-non-attending
  ##########################################################
  
  V_no <- list()
  
  V_no[["alt1"]] <-
    b_price_cn_no * price_cn_1 +
    b_price_jp_no * price_jp_1
  
  V_no[["alt2"]] <-
    b_price_cn_no * price_cn_2 +
    b_price_jp_no * price_jp_2
  
  V_no[["alt3"]] <- asc_nobuy_no
  
  mnl_settings_no <- list(
    alternatives  = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail         = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar     = choice,
    utilities     = V_no,
    componentName = "mnl_no_attend"
  )
  
  P_no <- list()
  P_no[["model"]] <- apollo_mnl(mnl_settings_no, functionality)
  P_no <- apollo_panelProd(P_no, apollo_inputs, functionality)
  
  P[["no_attend"]] <- P_no[["model"]]
  
  
  ##########################################################
  # Combine latent classes
  ##########################################################
  
  lc_settings <- list(
    inClassProb   = P,
    classProb     = pi_values,
    componentName = "lc_ana"
  )
  
  P[["model"]] <- apollo_lc(lc_settings, apollo_inputs, functionality)
  
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  
  return(P)
}
############################################################
# Validate and estimate
############################################################
apollo_inputs_ana <- apollo_validateInputs(
  database       = database,
  apollo_control = apollo_control_ana,
  apollo_beta    = apollo_beta_ana,
  apollo_fixed   = apollo_fixed_ana
)

model_ana <- apollo_estimate(
  apollo_beta_ana,
  apollo_fixed_ana,
  apollo_probabilities_ana,
  apollo_inputs_ana
)

apollo_modelOutput(model_ana)
apollo_saveOutput(model_ana)
# Uncomment when M1-M4 have run successfully.
# model_ana <- apollo_estimate(apollo_beta_ana, apollo_fixed_ana, apollo_probabilities_ana, apollo_inputs_ana)
# apollo_modelOutput(model_ana)
# apollo_saveOutput(model_ana)

############################################################
# Suggested reporting order:
# 1. Descriptive statistics and choice shares
# 2. M1 baseline MNL
# 3. M2 country-specific MNL + WTP
# 4. M3 information-treatment interactions
# 5. M4 2-class LCM for preference heterogeneity
# 6. M5 ANA extension as behavioral extension, if stable
############################################################

