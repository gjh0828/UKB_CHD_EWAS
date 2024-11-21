library(tidyverse);library(data.table);library(caret);library(interactionR);
library(graphPAF);library(psych);library(forestploter);library(grid)
Figure1 <- read_csv("Figure1.csv")
result <- read.csv("result/main_analysis.csv")
variable_groups <- list(
  "Socioeconomic_status" = unique(Figure1$Field[Figure1$Cat1=="Socioeconomic status"]),
  "Lifestyles" = unique(Figure1$Field[Figure1$Cat1=="Lifestyles"]),
  "Early_life_factor" = unique(Figure1$Field[Figure1$Cat1=="Early life factor"]),
  "Psychiosocial_factor" = unique(Figure1$Field[Figure1$Cat1=="Psychiosocial factor"]),
  "Local_environment" = unique(Figure1$Field[Figure1$Cat1=="Local environment"]),
  "Blood_assays" = unique(Figure1$Field[Figure1$Cat1=="Blood assays"]),
  "Physical_measures" = unique(Figure1$Field[Figure1$Cat1=="Physical measures"]),
  "Health_and_medical_history" = unique(Figure1$Field[Figure1$Cat1=="Health and medical history"])
)
data <- readRDS("df.rds")
Joint_exposure <- data[,"eid"]
Covariate <- data[,c("eid", "p31", "p21022", "p54_i0", "p26223", "p26227", "p21000_i0", "Family_history")]
outcomes <- data[,c("eid", "time", "outcome")]
for (group_name in names(variable_groups)) {
  print(group_name)
  df <- data[, c("eid", variable_groups[[group_name]])]
  dmy<-dummyVars(~.,data=df[2:ncol(df)],sep = "",fullRank=TRUE)
  b=as.data.frame(predict(dmy, df[2:ncol(df)]))
  b <- lapply(b, function(col) {
    if (all(col %in% c(0, 1))) {
      return(as.factor(col))
    } else {return(col)}})
  b$eid=df$eid
  b=dplyr::select(b,eid,everything())
  Protective_exposures=result$exposure[result$HR<1]
  matching_indices <- which(colnames(b) %in% Protective_exposures)
  if (length(matching_indices) > 0) {
    for (i in 1:length(matching_indices)) {
      column_name <- colnames(b)[matching_indices[i]]
      if (is.numeric(b[[column_name]])) {
        b[[column_name]] <- -b[[column_name]]
      } else if (is.factor(b[[column_name]])) {
        levels(b[[column_name]]) <- list("0" = "1", "1" = "0")
      }
    }
  } else {
    print("No matching columns found in 'b' for Protective_exposures. Skipping operation.")
  }
  
  df=inner_join(inner_join(b,Covariate,by="eid"),outcomes,by="eid")
  library(survival)
  a=colnames(df)[2:ncol(b)]
  variable.names=paste0(a,collapse = "+")
  FML=as.formula(paste0("Surv(time,outcome==1)~Family_history+",variable.names,"+ p31+p21022+p54_i0+p21000_i0+Family_history"))
  fit.full=coxph(FML,data = df)
  GSum<- summary(fit.full)
  coef=GSum$coefficients[-1,1]
  for (i in 2:ncol(b)) {
    b[[i]]=as.integer(b[[i]])
  }
  b$score_NW=rowSums(b[2:ncol(b)],na.rm = T)
  b$score_NW_3=cut(b$score_NW,breaks = quantile(b$score_NW,probs = c(0,0.666,1),na.rm = T),include.lowest = TRUE)
  df=inner_join(inner_join(b,Covariate,by="eid"),outcomes,by="eid") #df=data
  fit.full=coxph(Surv(time,outcome==1)~score_NW_3+ p31+p21022+p54_i0+p21000_i0+Family_history,data=df)
  summary(fit.full)
  data1 <- df[,c("eid", "score_NW")];names(data1) <- c("eid", paste0(group_name, "_NW"))
  Joint_exposure <- Joint_exposure %>% left_join(data1, by="eid")
  
  b_test=b[2:(ncol(b)-2)]
  b_test=as.matrix(b_test)
  coef=as.numeric(coef)
  b_test=b_test%*%diag(coef)
  b_test=as.data.frame(b_test)
  colnames(b_test)=colnames(b)[2:(ncol(b)-2)]
  b_test$eid=b$eid
  b_test=dplyr::select(b_test,eid,everything())
  b_test$score_W=rowSums(b_test[2:ncol(b_test)],na.rm = T)/sum(coef)
  b_test$score_W_3=cut(b_test$score_W,breaks = quantile(b_test$score_W,probs = c(0,1/3,2/3,1),na.rm = T),include.lowest = TRUE)
  df=inner_join(inner_join(b_test,Covariate,by="eid"),outcomes,by="eid") #df=data
  fit.full=coxph(Surv(time,outcome==1)~score_W_3+ p31+p21022+p54_i0+p21000_i0+Family_history,data=df)
  summary(fit.full)
  cox.zph(fit.full)
  data1 <- df[,c("eid", "score_W")];names(data1) <- c("eid", paste0(group_name,"_W"))
  Joint_exposure <- Joint_exposure %>% left_join(data1, by="eid")
}
