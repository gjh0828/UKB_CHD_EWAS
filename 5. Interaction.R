result_interaction <- rbindlist(lapply(colnames(df)[c(3,5,7,9,11,13,15,17)], function(x_) {
  print(x_)
  df$var <- df[[x_]]
  covariates <- paste0(setdiff(colnames(df)[c(3,5,7,9,11,13,15,17)], x_), collapse = "+")
  FML <- as.formula(paste0("Surv(time, outcome == 1) ~  var*p21022_1 + ",covariates,"+ p31+p21022+p54_i0+p21000_i0+Family_history"))
  fit <- coxph(FML, data = df)
  a <- interactionR(fit, exposure_names = c("var2", "p21022_12"),
                    ci.type = "delta", ci.level = 0.95,
                    em = FALSE, recode = FALSE)
  a1 <- a$dframe[9:12,] %>% mutate(var1=x_,var2="p21022_1", age=NA)
  return(a1)
}))

result_interaction1 <- rbindlist(lapply(colnames(df)[c(3,5,7,9,11,13,15,17)], function(x_) {
  print(x_)
  df$var <- paste0(df[[x_]], df$p21022_1)
  a <- paste0(setdiff(colnames(df)[c(3,5,7,9,11,13,15,17)], x_), collapse = "+")
  FML <- as.formula(paste0("Surv(time, outcome == 1) ~ Family_history + var + ",a,"+ p31+p21022+p54_i0+p21000_i0+Family_history"))
  fit <- coxph(FML, data = df)
  a1 <- as.data.frame(summary(fit)[["coefficients"]])[c(2:9),] %>%
    mutate(Var = rownames(.), Var1 = str_sub(Var, 4, 4), Var2 = str_sub(Var, 5, 5), Var3 = "-1", Var4=x_,
           HR1 = sprintf("%.2f (%.2f, %.2f)", `exp(coef)`, exp(coef - 1.96*`se(coef)`),
                         exp(coef + 1.96*`se(coef)`)))
  return(a1)
}))
