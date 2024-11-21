EWAS_cox_model = function(x) {
  if (is.numeric(df[[x]])) {
    P25 <- quantile(df[[x]], probs = 0.25, na.rm = TRUE)
    P75 <- quantile(df[[x]], probs = 0.75, na.rm = TRUE)
    IQR_value <- IQR(df[[x]], na.rm = TRUE)
    df[[x]] <- df[[x]] / IQR(df[[x]], na.rm = TRUE)
  }
  FML <- as.formula(paste0("Surv(time, outcome == 1) ~ ", x, "+ p31+p21022+p54_i0+p21000_i0+Family_history"))
  glm1 = coxph(FML, data = df) 
  GSum <- summary(glm1)[["coefficients"]]
  # PH test
  PH_test <- tryCatch({
    cox.zph(glm1, transform = "km", terms = TRUE, singledf = FALSE, global = FALSE)
  }, error = function(e) {
    NA
  })
  
  if (PH_test$table[rownames(PH_test$table)==x, "p"] < 0.0001) {
  df$var <- as.numeric(as.character(df[[x]]))
    FML <- as.formula(paste0("Surv(time, outcome == 1) ~ ", x,"+ tt(var) + p31+p21022+p54_i0+p21000_i0+Family_history"))
    glm1 = coxph(FML, data = df, 
                 tt = function(x, t, ...) (x * log10(t)))
    GSum <- summary(glm1)[["coefficients"]]
  }
  
  # Determining the type of a variable
  if (is.factor(df[[x]]) || is.character(df[[x]])) {
    # category variable
    case_data <- df %>%
      group_by(!!sym(x)) %>%
      summarise(case = sum(outcome == 1), full_sample = n()) %>%
      ungroup()
    
    EWAS_cox_model <- as.data.frame(GSum) 
    EWAS_cox_model <- Uni_glm_model[grep(x, rownames(Uni_glm_model)), ]%>%
      mutate(Field = x, 
             exposure = rownames(.),
             code = gsub(paste0("`?", x, "`?"), "", rownames(.)),
             HR = round(exp(coef), 4), 
             se = `se(coef)`, 
             CI5 = round(exp(coef - 1.96 * se), 4),
             CI95 = round(exp(coef + 1.96 * se), 4),
             Pvalue = `Pr(>|z|)`,
             case = case_data$case[match(gsub(paste0("`?", x, "`?"), "", rownames(.)), case_data[[x]])],
             full_sample = case_data$full_sample[match(gsub(paste0("`?", x, "`?"), "", rownames(.)), case_data[[x]])],
             P_value_for_Schoenfeld_residuals = PH_test$table[rownames(PH_test$table)==x, "p"],
             P25 = NA,
             P75 = NA,
             IQR = NA) %>%
      select(c(1,4,6:19))
    
  } else {
    # continuous variables
    case_value <- sum(df$outcome == 1&!is.na(df[[x]]), na.rm = T)
    full_sample_value <- sum(!is.na(df[[x]]))
    
    EWAS_cox_model <- as.data.frame(GSum)[x,] %>%
      mutate(Field = x, 
             exposure = rownames(.),
             code = "",
             HR = round(exp(coef), 4), 
             se = `se(coef)`, 
             CI5 = round(exp(coef - 1.96 * se), 4),
             CI95 = round(exp(coef + 1.96 * se), 4),
             Pvalue = `Pr(>|z|)`,
             case = case_value, 
             full_sample = full_sample_value,
             P_value_for_Schoenfeld_residuals = PH_test$table[rownames(PH_test$table)==x, "p"],
             P25 = P25,
             P75 = P75,
             IQR = IQR_value) %>%
      select(c(1,4,6:19))
  }
  EWAS_cox_model <- EWAS_cox_model %>% filter(!grepl("tt", code))
  return(EWAS_cox_model)
}

###EWAS analysis
df <- readRDS("df.rds")                 
variable.names=readRDS("variable.names.rds")
result <- rbindlist(setNames(lapply(variable.names, EWAS_cox_model), variable.names))
rownames(result) <- seq_len(nrow(result))
                 
###Random Forests for Survival
data=df[c("time", "outcome", "p54_i0", "p21022", "p21022_1", "p26227", "p31", "p21000_i0", "Family_history", variable.names)]
rfsrc_obj <- rfsrc(Surv(time, outcome) ~ ., data = data, ntree = 100, nsplit = 10)
maxsub <- max.subtree(rfsrc_obj, max.order = TRUE)
important_vars <- data.frame(order=maxsub$order) %>% mutate(Field = row.names(.))
