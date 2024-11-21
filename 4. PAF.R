result_PAF <- data.frame()
for(levels_ in c("lower", "higher")){
  data=inner_join(inner_join(Joint_exposure,Covariate,by="eid"),outcomes,by="eid")
  breaks_ <- if(levels_ == "lower") c(0, 2/3, 1) else c(0, 1/3, 1)
  
  data[,c(3,5,7,9,11,13,15,17)] <- lapply(data[,c(3,5,7,9,11,13,15,17)], function(x) {
    cut(x, breaks = quantile(x, probs = breaks_, na.rm = TRUE), 
        labels = c("0", "1"), include.lowest = TRUE)
  })
  for(age_ in c(0:3)){
    if(age_== 3){df=data} else {df=data %>% filter(p21022_1 %in% age_)}
    
    result_PAF_discrete = data.frame()
    a <- paste0(colnames(df)[c(3,5,7,9,11,13,15,17)],collapse = "+")
    FML=as.formula(paste0("Surv(time, outcome == 1) ~ ", a, "+ p31+p21022+p54_i0+p21000_i0+Family_history"))
    model_coxph <- coxph(FML, data = df)
    for(x_ in colnames(df)[c(3,5,7,9,11,13,15,17)]){
      print(x_)
      model <- PAF_calc_discrete(model=model_coxph, riskfactor = x_, refval = 0, data = df, 
                                 calculation_method = "D", #ci = TRUE, boot_rep = 50, ci_type = c("norm"),
                                 t_vector = 10, verbose=TRUE)
      model <- data.frame(predictors=x_, AF=model)
      result_PAF_discrete <- bind_rows(result_PAF_discrete, model)
    }
    for (i in c(3,5,7,9,11,13,15,17)) {
      df[[i]]=as.integer(df[[i]])
    }
    
    correlation=tetrachoric(df[c(3,5,7,9,11,13,15,17)],na.rm=T)
    cor=correlation$rho
    
    # eigenvalues and eigenvectors
    ev=eigen(cor) 
    val=ev$values # eigenvalues
    U=as.matrix(ev$vectors) # eigenvectors
    which(val>1)
    U=U[,c(1,2)] # retain eigenvalues>1
    
    # communality
    U=as.data.frame(U)
    U$communality=0
    for (i in 1:nrow(U)) {
      a <- c(U$V1[i],U$V2[i])
      U$communality[i]=sum(a^2)
    } 
    U$predictors=colnames(df)[c(3,5,7,9,11,13,15,17)]
    
    # combine with the result of PAF
    communality=U
    #PAF=result
    communality=U[,c("communality","predictors")]
    PAF=result_PAF_discrete %>% 
      left_join(communality,by="predictors")
    
    # overall adjusted PAF
    a=1-(1-PAF$communality)*PAF$AF
    b=cumprod(a)[length(a)]
    overall_PAF=1-b
    
    # single adjusted PAF
    PAF$weighted_PAF=(PAF$AF/sum(PAF$AF))*overall_PAF
    PAF$group = levels_; PAF$var = "Age"; PAF$var2=age_
    result_PAF <- bind_rows(PAF, result_PAF)
  }
}
