## ============ 0) Packages ============
packs <- c("tidyverse","janitor","survival","car","tibble","tidyr","readr")
new <- packs[!packs %in% installed.packages()[,1]]
if(length(new)) install.packages(new)
invisible(lapply(packs, library, character.only = TRUE))

## ============ 1) Paths (edit if needed) ============
# China
path_china_cab <- "C:/data/C_CABBAGE.txt"
path_china_tom <- "C:/data/C_TOMATO.txt"
path_china_car <- "C:/data/C_CARROT.txt"
# Japan
path_japan_cab <- "C:/data/group Total.Cabbage.txt"
path_japan_tom <- "C:/data/group Total.Tomato.txt"
path_japan_car <- "C:/data/group Total.Carrot.txt"

stopifnot(file.exists(path_china_cab), file.exists(path_japan_cab))
stopifnot(file.exists(path_china_tom), file.exists(path_japan_tom))
stopifnot(file.exists(path_china_car), file.exists(path_japan_car))

## ============ 2) Helpers: read & standardize ============
read_txt <- function(path){
  readr::read_delim(path, delim = "\t", show_col_types = FALSE) |>
    janitor::clean_names() |>
    rename_with(toupper)
}

## ===== 2) 标准化列名：去掉“...17”之类空列，补EI别名、生成SEG =====
std_cols <- function(d){
  d <- d %>% dplyr::select(-tidyselect::starts_with("..."))  # 去掉无名列
  
  pick <- function(nms){ nm <- intersect(nms, names(d)); if(length(nm)) nm[1] else NA }
  map <- list(EIO  = pick(c("EIO","B","EI_O")),
              EIS  = pick(c("EIS","C","EI_S")),
              EIOS = pick(c("EIOS","D","EI_OS")),
              EIC  = pick(c("EIC","ECONTROL","E_CONTROL","E","CONTROL")))
  for (k in names(map)) d[[k]] <- if(!is.na(map[[k]])) d[[ map[[k]] ]] else 0
  
  need <- c("ID","PRICE","ORDI","ORGANIC","SPEC","CHOICE","MODE")
  for (v in need) if(!v %in% names(d)) d[[v]] <- NA
  
  seg_names <- intersect(paste0("SEG",1:5), names(d))
  if(length(seg_names)){
    d$SEG <- apply(d[,seg_names,drop=FALSE], 1, function(z) if(all(is.na(z))) NA else which.max(z))
  } else if(!"SEG" %in% names(d)) {
    d$SEG <- NA_integer_
  }
  d
}

## ===== 3) 主函数：合并中日 -> CLOGIT -> 跨国Wald -> WTP（带干净列名） =====
run_cc_ce <- function(path_china, path_japan){
  cn <- read_txt(path_china) |> dplyr::mutate(COUNTRY="China") |> std_cols()
  jp <- read_txt(path_japan) |> dplyr::mutate(COUNTRY="Japan") |> std_cols()
  
  dat <- bind_rows(cn, jp) |>
    mutate(
      SET_ID    = interaction(ID, MODE, drop = TRUE),
      COUNTRY_F = factor(COUNTRY, levels = c("Japan","China")), # Japan 基线
      CHINA     = if_else(COUNTRY_F=="China", 1, 0),
      across(c(CHOICE,EIO,EIS,EIOS,EIC,ORGANIC,SPEC,ORDI), as.numeric)
    )
  
  ## 可选：只保留“每个选择集恰好选1项”的记录
  valid_sets <- dat %>% group_by(SET_ID) %>% summarise(ch = sum(CHOICE, na.rm=TRUE), .groups="drop") %>%
    filter(ch == 1) %>% pull(SET_ID)
  dat <- dat %>% filter(SET_ID %in% valid_sets)
  
  ## 条件logit + 中日交互
  m <- survival::clogit(
    CHOICE ~ PRICE + ORGANIC + SPEC +
      CHINA:PRICE + CHINA:ORGANIC + CHINA:SPEC +
      strata(SET_ID) + cluster(ID),
    data = dat, method = "efron"
  )
  
  ## 跨国联合Wald
  wald_cc <- car::linearHypothesis(
    m,
    c("PRICE:CHINA = 0","ORGANIC:CHINA = 0","SPEC:CHINA = 0"),
    vcov. = vcov(m)
  )
  
  ## WTP（Delta method）
  b  <- coef(m); vc <- vcov(m)
  coef_names <- names(b)
  int_name <- function(x, y, pool_names = coef_names){
    cand <- c(paste0(x,":",y), paste0(y,":",x))
    hit  <- intersect(cand, pool_names)
    if (length(hit)==0) NA_character_ else hit[1]
  }
  nm_PCH <- int_name("PRICE","CHINA")
  nm_OCH <- int_name("ORGANIC","CHINA")
  nm_SCH <- int_name("SPEC","CHINA")
  
  wtp_delta <- function(num_names, den_names, b, vc){
    N <- sum(b[num_names], na.rm=TRUE)
    D <- sum(b[den_names], na.rm=TRUE)
    if (is.na(D) || abs(D) < .Machine$double.eps^0.5)
      return(c(est=NA_real_, se=NA_real_, lwr=NA_real_, upr=NA_real_))
    g <- rep(0, length(b)); names(g) <- names(b)
    g[num_names] <- D; g[den_names] <- -N; g <- g/(D^2)
    se  <- sqrt(as.numeric(t(g) %*% vc %*% g))
    est <- -N/D
    c(est=est, se=se, lwr=est-1.96*se, upr=est+1.96*se)
  }
  
  ## 直接 rbind 成矩阵，然后 bind_cols（不再用 unnest_wider）
  vals <- rbind(
    wtp_delta(c("ORGANIC"),          c("PRICE"),         b, vc),   # JP ORG
    wtp_delta(c("SPEC"),             c("PRICE"),         b, vc),   # JP SEMI
    wtp_delta(c("ORGANIC", nm_OCH),  c("PRICE", nm_PCH), b, vc),   # CN ORG
    wtp_delta(c("SPEC",    nm_SCH),  c("PRICE", nm_PCH), b, vc)    # CN SEMI
  )
  
  res_wtp <- tibble::tibble(
    country   = c("Japan","Japan","China","China"),
    attribute = c("ORGANIC","SEMI","ORGANIC","SEMI")
  ) |>
    dplyr::bind_cols(as.data.frame(vals)) |>
    dplyr::mutate(dplyr::across(c(est,se,lwr,upr), ~ round(., 3)))
  
  list(model = m,
       wald_cc = wald_cc,
       wtp = res_wtp)
}

## ===== 4) EI 检验：组内 + 跨国（鲁棒匹配交互项名，避免 bad coefficient 报错）=====
run_ei_tests <- function(path_china, path_japan){
  cn <- read_txt(path_china) |> dplyr::mutate(COUNTRY="China") |> std_cols()
  jp <- read_txt(path_japan) |> dplyr::mutate(COUNTRY="Japan") |> std_cols()
  
  dat <- bind_rows(cn, jp) |>
    dplyr::mutate(
      SET_ID    = interaction(ID, MODE, drop = TRUE),
      COUNTRY_F = factor(COUNTRY, levels = c("Japan","China")),
      CHINA     = if_else(COUNTRY_F=="China", 1, 0),
      dplyr::across(c(CHOICE,EIO,EIS,EIOS,EIC,ORGANIC,SPEC,ORDI), as.numeric)
    )
  
  # 仅保留“每个选择集恰好选择1项”
  valid_sets <- dat %>%
    dplyr::group_by(SET_ID) %>%
    dplyr::summarise(ch = sum(CHOICE, na.rm=TRUE), .groups="drop") %>%
    dplyr::filter(ch == 1) %>% dplyr::pull(SET_ID)
  dat <- dat %>% dplyr::filter(SET_ID %in% valid_sets)
  
  # EI × 属性 + 中日 × 属性 + 三重交互
  m <- survival::clogit(
    CHOICE ~ PRICE + ORGANIC + SPEC +
      (ORGANIC + SPEC):(EIO + EIS + EIOS) +
      CHINA:PRICE + CHINA:ORGANIC + CHINA:SPEC +
      CHINA:(ORGANIC + SPEC):(EIO + EIS + EIOS) +
      strata(SET_ID) + cluster(ID),
    data = dat, method = "efron"
  )
  
  cnames <- names(coef(m))
  
  # 找二维交互：a:b 或 b:a
  int2 <- function(a,b){
    cand <- c(paste0(a,":",b), paste0(b,":",a))
    hit  <- intersect(cand, cnames)
    if(length(hit)) hit[1] else NA_character_
  }
  
  # 找三维交互：任意排列
  int3 <- function(a,b,c){
    cand <- c(paste(a,b,c,sep=":"), paste(a,c,b,sep=":"),
              paste(b,a,c,sep=":"), paste(b,c,a,sep=":"),
              paste(c,a,b,sep=":"), paste(c,b,a,sep=":"))
    hit <- intersect(cand, cnames)
    if(length(hit)) hit[1] else NA_character_
  }
  
  # 安全 Wald：只检验实际存在的系数；若都不存在，返回 NA 行
  wald_try <- function(terms){
    keep <- terms[!is.na(terms)]
    if(!length(keep)){
      return(tibble::tibble(test = NA_character_, Chisq = NA_real_, Df = NA_real_, p = NA_real_))
    }
    lh <- car::linearHypothesis(m, paste0(keep," = 0"), vcov.=vcov(m))
    tibble::tibble(
      test  = paste(paste0(keep," = 0"), collapse = ", "),
      Chisq = as.numeric(lh$Chisq[2]),
      Df    = as.numeric(lh$Df[2]),
      p     = as.numeric(lh$`Pr(>Chisq)`[2])
    )
  }
  
  ## —— 组内：日本（基线）——
  jp_EIO  <- wald_try(c(int2("ORGANIC","EIO"),  int2("SPEC","EIO")))  |> dplyr::mutate(side="JP", info="EIO")
  jp_EIS  <- wald_try(c(int2("ORGANIC","EIS"),  int2("SPEC","EIS")))  |> dplyr::mutate(side="JP", info="EIS")
  jp_EIOS <- wald_try(c(int2("ORGANIC","EIOS"), int2("SPEC","EIOS"))) |> dplyr::mutate(side="JP", info="EIOS")
  
  ## —— 组内：中国（= 日本基线 + 三重交互差异）——
  cn_EIO  <- wald_try(c(int2("ORGANIC","EIO"),  int2("SPEC","EIO"),
                        int3("CHINA","ORGANIC","EIO"), int3("CHINA","SPEC","EIO")))  |> dplyr::mutate(side="CN", info="EIO")
  cn_EIS  <- wald_try(c(int2("ORGANIC","EIS"),  int2("SPEC","EIS"),
                        int3("CHINA","ORGANIC","EIS"), int3("CHINA","SPEC","EIS")))  |> dplyr::mutate(side="CN", info="EIS")
  cn_EIOS <- wald_try(c(int2("ORGANIC","EIOS"), int2("SPEC","EIOS"),
                        int3("CHINA","ORGANIC","EIOS"), int3("CHINA","SPEC","EIOS"))) |> dplyr::mutate(side="CN", info="EIOS")
  
  ## —— 跨国差异：只检验三重交互（中国-日本差异的那部分）——
  diff_EIO  <- wald_try(c(int3("CHINA","ORGANIC","EIO"), int3("CHINA","SPEC","EIO")))   |> dplyr::mutate(info="EIO")
  diff_EIS  <- wald_try(c(int3("CHINA","ORGANIC","EIS"), int3("CHINA","SPEC","EIS")))   |> dplyr::mutate(info="EIS")
  diff_EIOS <- wald_try(c(int3("CHINA","ORGANIC","EIOS"),int3("CHINA","SPEC","EIOS")))  |> dplyr::mutate(info="EIOS")
  
  list(
    model        = m,
    wald_within  = dplyr::bind_rows(jp_EIO, jp_EIS, jp_EIOS, cn_EIO, cn_EIS, cn_EIOS),
    wald_cross   = dplyr::bind_rows(diff_EIO, diff_EIS, diff_EIOS)
  )
}

## ===== 5) 路径（按你现有的 C:/data/）=====
path_china_cab <- "C:/data/C_CABBAGE.txt"
path_japan_cab <- "C:/data/group Total.Cabbage.txt"

path_china_tom <- "C:/data/C_TOMATO.txt"
path_japan_tom <- "C:/data/group Total.Tomato.txt"

path_china_car <- "C:/data/C_CARROT.txt"
path_japan_car <- "C:/data/group Total.Carrot.txt"

## ===== 6) 运行：基础跨国 + WTP =====
out_cab <- run_cc_ce(path_china_cab, path_japan_cab)
out_tom <- run_cc_ce(path_china_tom, path_japan_tom)
out_car <- run_cc_ce(path_china_car, path_japan_car)

## ===== 7) 运行：EI 组内/跨国检验 =====
ei_cab <- run_ei_tests(path_china_cab, path_japan_cab)
ei_tom <- run_ei_tests(path_china_tom, path_japan_tom)
ei_car <- run_ei_tests(path_china_car, path_japan_car)

## ===== 8) 导出你要的“Wald检验表格”和合并的 WTP =====
# 8.1) 合并 WTP，并以 est/se/lwr/upr 列导出
wtp_all <- bind_rows(
  mutate(out_cab$wtp, vegetable = "Cabbage"),
  mutate(out_tom$wtp, vegetable = "Tomato"),
  mutate(out_car$wtp, vegetable = "Carrot")
) |>
  select(vegetable, country, attribute, est, se, lwr, upr)

readr::write_csv(wtp_all, "C:/data/wtp_crosscountry.csv")

# 8.2) 基础“跨国”联合Wald（每个蔬菜一行）
wald_cc_all <- bind_rows(
  tibble(vegetable="Cabbage", chisq = out_cab$wald_cc$Chisq[2], df = out_cab$wald_cc$Df[2], p = out_cab$wald_cc$`Pr(>Chisq)`[2]),
  tibble(vegetable="Tomato",  chisq = out_tom$wald_cc$Chisq[2], df = out_tom$wald_cc$Df[2], p = out_tom$wald_cc$`Pr(>Chisq)`[2]),
  tibble(vegetable="Carrot",  chisq = out_car$wald_cc$Chisq[2], df = out_car$wald_cc$Df[2], p = out_car$wald_cc$`Pr(>Chisq)`[2])
)
readr::write_csv(wald_cc_all, "C:/data/wald_crosscountry_basic.csv")

# 8.3) EI 组内与跨国 Wald 表
ei_within_all <- bind_rows(
  mutate(ei_cab$wald_within, vegetable="Cabbage"),
  mutate(ei_tom$wald_within, vegetable="Tomato"),
  mutate(ei_car$wald_within, vegetable="Carrot")
) |> relocate(vegetable, .before = test)
readr::write_csv(ei_within_all, "C:/data/wald_EI_within_country.csv")

ei_cross_all <- bind_rows(
  mutate(ei_cab$wald_cross, vegetable="Cabbage"),
  mutate(ei_tom$wald_cross, vegetable="Tomato"),
  mutate(ei_car$wald_cross, vegetable="Carrot")
) |> relocate(vegetable, .before = test)
readr::write_csv(ei_cross_all, "C:/data/wald_EI_crosscountry.csv")
## ==== 附录表：整理 + 导出（CSV；可选 Word）====

safeload <- function(pkgs){
  new <- pkgs[!pkgs %in% installed.packages()[,1]]
  if(length(new)) install.packages(new)
  invisible(lapply(pkgs, library, character.only = TRUE))
}
safeload(c("tidyverse"))

# ===== 安装并加载 officer/flextable（若尚未安装会自动装）=====
for(pk in c("officer","flextable")) if(!requireNamespace(pk, quietly=TRUE)) install.packages(pk)
library(officer); library(flextable)

# 小工具：显著性星号
p_stars <- function(p){
  dplyr::case_when(
    is.na(p)        ~ "",
    p < 0.001       ~ "***",
    p < 0.01        ~ "**",
    p < 0.05        ~ "*",
    TRUE            ~ ""
  )
}

## A1. 基础跨国 Wald（每个蔬菜一行）
wald_cc_all <- dplyr::bind_rows(
  tibble::tibble(vegetable="Cabbage",
                 chisq = out_cab$wald_cc$Chisq[2],
                 df    = out_cab$wald_cc$Df[2],
                 p     = out_cab$wald_cc$`Pr(>Chisq)`[2]),
  tibble::tibble(vegetable="Tomato",
                 chisq = out_tom$wald_cc$Chisq[2],
                 df    = out_tom$wald_cc$Df[2],
                 p     = out_tom$wald_cc$`Pr(>Chisq)`[2]),
  tibble::tibble(vegetable="Carrot",
                 chisq = out_car$wald_cc$Chisq[2],
                 df    = out_car$wald_cc$Df[2],
                 p     = out_car$wald_cc$`Pr(>Chisq)`[2])
) |>
  dplyr::mutate(sig = p_stars(p)) |>
  dplyr::rename(`Chi-square`=chisq, `df`=df, `p-value`=p)

readr::write_csv(wald_cc_all, "C:/data/Appendix_A1_Wald_CrossCountry.csv")

## A2. EI 组内 Wald（日本/中国各自是否有效）
ei_within_all <- dplyr::bind_rows(
  dplyr::mutate(ei_cab$wald_within, vegetable="Cabbage"),
  dplyr::mutate(ei_tom$wald_within, vegetable="Tomato"),
  dplyr::mutate(ei_car$wald_within, vegetable="Carrot")
) |>
  dplyr::select(vegetable, side, info, test, Chisq, Df, p) |>
  dplyr::mutate(sig = p_stars(p)) |>
  dplyr::rename(Side=side, Info=info, `Chi-square`=Chisq, `df`=Df, `p-value`=p)

readr::write_csv(ei_within_all, "C:/data/Appendix_A2_Wald_EI_Within.csv")

## A3. EI 跨国 Wald（三重交互）
ei_cross_all <- dplyr::bind_rows(
  dplyr::mutate(ei_cab$wald_cross, vegetable="Cabbage"),
  dplyr::mutate(ei_tom$wald_cross, vegetable="Tomato"),
  dplyr::mutate(ei_car$wald_cross, vegetable="Carrot")
) |>
  dplyr::select(vegetable, info, test, Chisq, Df, p) |>
  dplyr::mutate(sig = p_stars(p)) |>
  dplyr::rename(Info=info, `Chi-square`=Chisq, `df`=Df, `p-value`=p)

readr::write_csv(ei_cross_all, "C:/data/Appendix_A3_Wald_EI_CrossCountry.csv")

cat("\nSaved appendix CSVs:\n",
    "  C:/data/Appendix_A1_Wald_CrossCountry.csv\n",
    "  C:/data/Appendix_A2_Wald_EI_Within.csv\n",
    "  C:/data/Appendix_A3_Wald_EI_CrossCountry.csv\n")

## （可选）导出到 Word：一页放三表
try({
  safeload(c("flextable","officer"))
  library(flextable); library(officer)
  
  ft1 <- flextable::regulartable(wald_cc_all) |> flextable::autofit()
  ft2 <- flextable::regulartable(ei_within_all) |> flextable::autofit()
  ft3 <- flextable::regulartable(ei_cross_all)  |> flextable::autofit()
  
  doc <- read_docx() |>
    body_add_par("Appendix A1. Cross-country Wald tests", style = "heading 2") |>
    body_add_flextable(ft1) |>
    body_add_par("") |>
    body_add_par("Appendix A2. Within-country Wald tests for EI effects", style = "heading 2") |>
    body_add_flextable(ft2) |>
    body_add_par("") |>
    body_add_par("Appendix A3. Cross-country Wald tests for EI effects", style = "heading 2") |>
    body_add_flextable(ft3) |>
    body_add_par("") |>
    body_add_par("Notes: Wald tests use cluster-robust (ID) variance from conditional logit; significance codes: *** p<0.001, ** p<0.01, * p<0.05.", style = "Normal")
  
  print(doc, target = "C:/data/Appendix_Wald_Tables.docx")
  cat("Saved Word doc:\n  C:/data/Appendix_Wald_Tables.docx\n")
}, silent = TRUE)