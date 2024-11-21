library(data.table); library(R.utils); library(TwoSampleMR)
CHD <- fread("F:/finngen_R11/summary_stats_finngen_R11_I9_CHD.gz")
result_MR <- data.frame()
result_MR_PRESSO <- data.frame()
for(exposure_ in unique(exposure_finngen$phenocode)){
  options(ieugwasr_api = 'gwas-api.mrcieu.ac.uk/') 
  data1 <- exposure_finngen %>% filter(phenocode %in% exposure_)
  data <- read_csv(paste0("MR/save_harmonise_dat/",exposure_,".csv")) %>% as.data.frame()
  if(nrow(data)>2){
    data$r.outcome <- get_r_from_lor(lor = data$beta.outcome, af = data$eaf.outcome, ncase = 75592,
                                     ncontrol = 378141, prevalence = 75592/(75592+378141))
    whole_test2 <- directionality_test(dat =data)
    mr_pleiotropy_test = mr_pleiotropy_test(data)
    heterogeneity <- mr_heterogeneity(data)
    a <- mr(data)
    a <- generate_odds_ratios(a) %>%
      mutate(p.for.Egger.intercept = mr_pleiotropy_test$pval) %>%
      left_join(heterogeneity[5:8], by = "method") %>%
      left_join(whole_test2[4:8], by = "outcome") %>%
      mutate(F.stat = (data$samplesize.exposure[1] - dim(data)[1] - 1) / 
               (dim(data)[1]) * whole_test2$snp_r2.exposure / (1 - whole_test2$snp_r2.exposure))
  }else {a <- mr(data)
  a <- generate_odds_ratios(a)}
  result_MR <- bind_rows(result_MR, a)
  
  # MR-PRESSO analysis (requires >3 SNPs)
  if (nrow(data) > 3) {
    mr_presso_results <- run_mr_presso(data, NbDistribution = 8000)
    MR_PRESSO[exposure_] <- mr_presso_results
    presso_results <- mr_presso_results[[1]]$`Main MR results` %>%
      mutate(RSSobs = mr_presso_results[[1]][["MR-PRESSO results"]][["Global Test"]][["RSSobs"]],
             Pvalue = paste0("_",mr_presso_results[[1]][["MR-PRESSO results"]][["Global Test"]][["Pvalue"]]),
             Distortion_Coefficient = mr_presso_results[[1]][["MR-PRESSO results"]][["Distortion Test"]][["Distortion Coefficient"]],
             Distortion_Test_Pvalue = mr_presso_results[[1]][["MR-PRESSO results"]][["Distortion Test"]][["Pvalue"]],
             x=exposure_,ID = data1$ID)
    result_MR_PRESSO <- bind_rows(presso_results, result_MR_PRESSO)
  }
}
for(var_ in var){
  print(var_)
  data1 <- exposure_ieu %>% filter(`GWAS ID` %in% var_)
  options(ieugwasr_api = "gwas-api.mrcieu.ac.uk/")
  data <- read_csv(paste0("MR/save_harmonise_dat/",var_,".csv")) %>% as.data.frame()
  if(nrow(data)>2 & !(var_ =="ukb-e-1647_p1_CSA")){
    data$r.outcome <- get_r_from_lor(lor = data$beta.outcome, af = data$eaf.outcome, ncase = 75592,
                                     ncontrol = 378141, prevalence = 75592/(75592+378141))
    whole_test2 <- directionality_test(dat =data)
    mr_pleiotropy_test = mr_pleiotropy_test(data)
    heterogeneity <- mr_heterogeneity(data)
    a <- mr(data)
    a <- generate_odds_ratios(a) %>%
      mutate(p.for.Egger.intercept = mr_pleiotropy_test$pval) %>%
      left_join(heterogeneity[5:8], by = "method") %>%
      left_join(whole_test2[4:8], by = "outcome") %>%
      mutate(F.stat = (data$samplesize.exposure[1] - dim(data)[1] - 1) / 
               (dim(data)[1]) * whole_test2$snp_r2.exposure / (1 - whole_test2$snp_r2.exposure),
             id.exposure=data1$ID, exposure=data1$`GWAS ID`)
  }else{a <- mr(data)
  a <- generate_odds_ratios(a) %>%
    mutate(id.exposure=data1$ID, exposure=data1$`GWAS ID`)}
  result_MR <- bind_rows(result_MR, a)
  # MR-PRESSO analysis (requires >3 SNPs)
  if (nrow(data) > 4) {
    mr_presso_results <- run_mr_presso(data, NbDistribution = 8000)
    MR_PRESSO[var_] <- mr_presso_results
    presso_results <- mr_presso_results[[1]]$`Main MR results` %>%
      mutate(RSSobs = mr_presso_results[[1]][["MR-PRESSO results"]][["Global Test"]][["RSSobs"]],
             Pvalue = paste0("_",mr_presso_results[[1]][["MR-PRESSO results"]][["Global Test"]][["Pvalue"]]),
             Distortion_Coefficient = mr_presso_results[[1]][["MR-PRESSO results"]][["Distortion Test"]][["Distortion Coefficient"]],
             Distortion_Test_Pvalue = mr_presso_results[[1]][["MR-PRESSO results"]][["Distortion Test"]][["Pvalue"]],
             x=var_,ID = data1$ID)
    result_MR_PRESSO <- bind_rows(presso_results, result_MR_PRESSO)
  }
}
saveRDS(MR_PRESSO, "MR/MR_PRESSO.rds")
write_csv(result_MR, "MR/result_MR.csv")
write_csv(result_MR_PRESSO, "MR/result_MR_PRESSO.csv")
