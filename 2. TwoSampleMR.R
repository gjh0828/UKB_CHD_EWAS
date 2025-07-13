library(data.table); library(R.utils); library(TwoSampleMR)
library(ieugwasr);library(readxl);library(tidyverse)
# harmonise_finngen_data --------------------------------------------------
for(exposure_ in unique(exposure_finngen$phenocode)){
  data1 <- exposure_finngen %>% filter(phenocode %in% exposure_)
  EXP <- fread(paste0("F:/finngen_R11/summary_stats_finngen_R11_",exposure_,".gz")) %>%
    mutate(id.exposure = data1$ID, exposure = exposure_, samplesize.exposure = data1$samplesize)
  p_value_threshold <- 5e-08
  EXP <- EXP[,c("rsids","alt","ref","af_alt","beta","sebeta","pval","exposure", "id.exposure",  "samplesize.exposure")]
  colnames(EXP) <- c("SNP","effect_allele.exposure","other_allele.exposure","eaf.exposure",
                     "beta.exposure","se.exposure","pval.exposure", "id.exposure", "exposure", "samplesize.exposure")
  new_EXP<-subset(EXP,pval.exposure < p_value_threshold)
  if (nrow(new_EXP) < 3) {
    p_value_threshold <- 5e-06
    new_EXP <- subset(EXP, pval.exposure < p_value_threshold)
  }
  new_EXP<-clump_data(new_EXP,clump_kb = 10000,clump_r2 = 0.001,clump_p1 = p_value_threshold, pop = "EUR")
  new_CHD <- CHD %>% 
    transmute(SNP = rsids,effect_allele.outcome = alt, other_allele.outcome = ref,
              eaf.outcome = af_alt, beta.outcome = beta, se.outcome = sebeta, pval.outcome = pval,
              id.outcome = "I9_CHD", outcome = "Coronary heart disease",
              samplesize.outcome = samplesize.outcome, case.outcome = NCAS, control.outcome = NCON)
  total <- merge(new_EXP,new_CHD,by.x="SNP",by.y="SNP",all = F)
  exposure_dat <- total[, c("SNP", "effect_allele.exposure", "other_allele.exposure",
                            "eaf.exposure", "beta.exposure", "se.exposure", "pval.exposure",
                            "id.exposure", "exposure", "samplesize.exposure")]
  outcome_dat <- total[,c("SNP","effect_allele.outcome","other_allele.outcome", "eaf.outcome", 
                          "beta.outcome", "se.outcome","pval.outcome","id.outcome","outcome",
                          "samplesize.outcome", "case.outcome", "control.outcome")]
  data <- harmonise_data(exposure_dat = exposure_dat, outcome_dat = outcome_dat)
  write_csv(data, paste0("MR/save_harmonise_dat/",exposure_,".csv"))
}
# harmonise_ieu_data --------------------------------------------------
for(var_ in exposure_ieu$`GWAS ID`){
  print(var_)
  data1 <- exposure_ieu %>% filter(`GWAS ID` %in% var_)
  #options(ieugwasr_api = "gwas-api.mrcieu.ac.uk/")
  exposure_data <- tryCatch({
    extract_instruments(outcome = var_, p1 = 5e-08, clump = TRUE)
  }, error = function(e) {
    message("An error occurred: ", e$message)
    NULL
  })
  if (is.null(exposure_data) || nrow(exposure_data) < 3) {
    exposure_data <- tryCatch({
      extract_instruments(outcome = var_, p1 = 5e-06, clump = TRUE)
    }, error = function(e) {
      message("An error occurred: ", e$message)
      NULL
    })
  }
  if (is.null(exposure_data)) {
    next
  }
  new_CHD <- CHD %>% 
    transmute(SNP = rsids,effect_allele.outcome = alt, other_allele.outcome = ref,
              eaf.outcome = af_alt, beta.outcome = beta, se.outcome = sebeta, pval.outcome = pval,
              id.outcome = "I9_CHD", outcome = "Coronary heart disease",
              samplesize.outcome = samplesize.outcome, case.outcome = NCAS, control.outcome = NCON)
  
  total <- merge(exposure_data,new_CHD,by.x="SNP",by.y="SNP",all = F)
  exposure_dat <- total[, c("SNP", "effect_allele.exposure", "other_allele.exposure",
                            "eaf.exposure", "beta.exposure", "se.exposure", "pval.exposure",
                            "id.exposure", "exposure", "samplesize.exposure")]
  outcome_dat <- total[,c("SNP","effect_allele.outcome","other_allele.outcome", "eaf.outcome", 
                          "beta.outcome", "se.outcome","pval.outcome","id.outcome","outcome",
                          "samplesize.outcome", "case.outcome", "control.outcome")]
  data <- harmonise_data(exposure_dat = exposure_dat, outcome_dat = outcome_dat) %>%
    mutate(field = data1$ID)
  if (anyNA(data$samplesize.exposure)) {
    message("Samplesize.exposure is NA：")
    print(var_)
  }
  write_csv(data, paste0("MR/save_harmonise_dat/",var_,".csv"))
}
# Two-sample MR --------------------------------------------------
field_gwasid <- readRDS("field_gwasid.rds")
result_MR <- data.frame()
result_MR_PRESSO <- data.frame()
MR_PRESSO <- list()
for (i in seq_along(field_gwasid$`GWAS ID`)) {
  gwas_id <- field_gwasid$`GWAS ID`[i]
  field_gwasid1 <- field_gwasid %>% filter(`GWAS ID` == gwas_id)
  file_path <- paste0("MR/save_harmonise_dat/", gwas_id, ".csv")
  if (!file.exists(file_path) || nrow(read_csv(file_path)) == 0) {
    message(paste("Skip:", file_path, "(file missing or empty)"))
    a <- field_gwasid1
    presso_results <- field_gwasid1
    result_MR <- bind_rows(result_MR, a)
    result_MR_PRESSO <- bind_rows(result_MR_PRESSO, presso_results)
    next
  }
  data <- read_csv(file_path) %>% as.data.frame() %>%
    mutate(across(c(case.outcome, control.outcome), as.numeric)) %>%
    mutate(prevalence = case.outcome / (case.outcome + control.outcome))
  if(nrow(data)>2){
    data$r.outcome <- get_r_from_lor(lor = data$beta.outcome, af = data$eaf.outcome, ncase = data$case.outcome,
                                     ncontrol = data$control.outcome, prevalence = data$prevalence)
    whole_test2 <- directionality_test(dat =data)
    mr_pleiotropy_test = mr_pleiotropy_test(data)
    heterogeneity <- mr_heterogeneity(data)
    a <- generate_odds_ratios(mr(data)) %>%
      mutate(p.for.Egger.intercept = mr_pleiotropy_test$pval) %>%
      left_join(heterogeneity[5:8], by = "method") %>%
      left_join(whole_test2[4:8], by = "outcome") %>%
      mutate(F.stat = (data$samplesize.exposure[1] - dim(data)[1] - 1) / 
               (dim(data)[1]) * whole_test2$snp_r2.exposure / (1 - whole_test2$snp_r2.exposure))
  } else {a <- mr(data)
  a <- generate_odds_ratios(a)}
  a <- a %>% left_join(field_gwasid1, by = c("id.exposure" = "GWAS ID"))
  result_MR <- bind_rows(result_MR, a)
  if (nrow(data) > 3) {
    set.seed(8001)
    try({
      mr_presso_results <- run_mr_presso(data, NbDistribution = 8000)
      MR_PRESSO[[gwas_id]] <- mr_presso_results
      distortion <- mr_presso_results[[1]][["MR-PRESSO results"]][["Distortion Test"]]
      presso_results <- mr_presso_results[[1]]$`Main MR results` %>%
        mutate(RSSobs = mr_presso_results[[1]][["MR-PRESSO results"]][["Global Test"]][["RSSobs"]],
               Pvalue = paste0("_", mr_presso_results[[1]][["MR-PRESSO results"]][["Global Test"]][["Pvalue"]]),
               Distortion_Test_Pvalue = if (!is.null(distortion)) paste0("_", distortion[["Pvalue"]]) else NA,
               Distortion_Coefficient = if (!is.null(distortion)) distortion[["Distortion Coefficient"]] else NA,
               `GWAS ID` = gwas_id, Field = field_gwasid$Field[field_gwasid$`GWAS ID` == gwas_id])
    }, silent = TRUE)
  }else {
    presso_results <- field_gwasid1
  }
  result_MR_PRESSO <- bind_rows(presso_results, result_MR_PRESSO)
}
saveRDS(MR_PRESSO, "MR/MR_PRESSO.rds")
write_csv(result_MR, "MR/result_MR.csv")
write_csv(result_MR_PRESSO, "MR/result_MR_PRESSO.csv")
