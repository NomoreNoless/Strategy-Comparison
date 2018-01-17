Cumulative_1vs1 <- function(matrix,Factors,PerformanceMetric,method,template){
  
  PerformanceMetric <- rlang::sym(PerformanceMetric)
  
  matrix <- matrix %>% filter(!is.na(rlang::UQ(PerformanceMetric)))

  dataset1 <- matrix %>% filter(id==1)
  all_obs1 <- dim(dataset1)[1]
  dataset2 <- matrix %>% filter(id==2)
  all_obs2 <- dim(dataset2)[1]
  
  if(method=="straight"){
    test_result <- matrix %>% group_by(bin_cumulative) %>%
      summarise(pvalue = ifelse(length(unique(id[which(!is.na(rlang::UQ(PerformanceMetric)))]))>1 & (length(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)) & id==1)])>1 & length(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)) & id==2)])>1),pnorm(t.test(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)))]~id[which(!is.na(rlang::UQ(PerformanceMetric)))])$statistic),NA),
                test_statistic = ifelse(length(unique(id[which(!is.na(rlang::UQ(PerformanceMetric)))]))>1 & (length(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)) & id==1)])>1 & length(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)) & id==2)])>1),t.test(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)))]~id[which(!is.na(rlang::UQ(PerformanceMetric)))])$statistic,NA),
                average1 = mean(rlang::UQ(PerformanceMetric)[which(id==1)],na.rm=TRUE),
                average2 = mean(rlang::UQ(PerformanceMetric)[which(id==2)],na.rm=TRUE),
                df1 = length(which(id==1)),
                df2 = length(which(id==2)),
                numobs1 = df1,
                numobs2 = df2,
                std_error1 = sd(rlang::UQ(PerformanceMetric)[which(id==1)],na.rm=TRUE)/sqrt(df1),
                std_error2 = sd(rlang::UQ(PerformanceMetric)[which(id==2)],na.rm=TRUE)/sqrt(df2),
                perc1 = numobs1/all_obs1,
                perc2 = numobs2/all_obs2)
    test_result <- test_result[,c(1,2,3,4,5,10,11,6,7,8,9,12,13)]
    
  }
  
  else if(method=="weighted"){
    test_result <- matrix %>% group_by(bin_cumulative) %>%
      summarise(wt_avg1 = wt.mean(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)) & id==1)],notional[which(!is.na(rlang::UQ(PerformanceMetric)) & id==1)]),
                wt_avg2 = wt.mean(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)) & id==2)],notional[which(!is.na(rlang::UQ(PerformanceMetric)) & id==2)]),
                df1 = sum(notional[which(!is.na(notional) & id==1)])**2/sum(notional[which(!is.na(notional) & id==1)]**2),
                df2 = sum(notional[which(!is.na(notional) & id==2)])**2/sum(notional[which(!is.na(notional) & id==2)]**2),
                numobs1 = length(which(id==1)),
                numobs2 = length(which(id==2)),
                std_error1 = wt.sd(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)) & id==1)],notional[which(!is.na(rlang::UQ(PerformanceMetric)) & id==1)])/sqrt(df1),
                std_error2 = wt.sd(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)) & id==2)],notional[which(!is.na(rlang::UQ(PerformanceMetric)) & id==2)])/sqrt(df2),
                pvalue = ifelse(length(unique(id[which(!is.na(rlang::UQ(PerformanceMetric)))]))>1 & (length(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)) & id==1)])>1 & length(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)) & id==2)])>1),pnorm((wt_avg1-wt_avg2)/(std_error1+std_error2)),NA),
                test_statistic = ifelse(length(unique(id[which(!is.na(rlang::UQ(PerformanceMetric)))]))>1 & (length(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)) & id==1)])>1 & length(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)) & id==2)])>1),(wt_avg1-wt_avg2)/(std_error1+std_error2),NA),
                perc1 = numobs1/all_obs1,
                perc2 = numobs2/all_obs2)
    
    test_result <- test_result[,c(1,10,11,2,3,8,9,4,5,6,7,12,13)]
  }
  else{
    test_result <- matrix %>% group_by(bin_cumulative) %>%
      summarise(median1 = median(rlang::UQ(PerformanceMetric)[which(id==1)],na.rm=TRUE),
                median2 = median(rlang::UQ(PerformanceMetric)[which(id==2)],na.rm=TRUE),
                df1 = length(which(id==1)),
                df2 = length(which(id==2)),
                numobs1 = df1,
                numobs2 = df2,
                perc1 = numobs1/all_obs1,
                perc2 = numobs2/all_obs2,
                std_error1 = sd(rlang::UQ(PerformanceMetric)[which(id==1)],na.rm=TRUE)/sqrt(df1),
                std_error2 = sd(rlang::UQ(PerformanceMetric)[which(id==2)],na.rm=TRUE)/sqrt(df2),
                pvalue = ifelse(length(unique(id[which(!is.na(rlang::UQ(PerformanceMetric)))]))>1 & (length(id[which(!is.na(rlang::UQ(PerformanceMetric)) & id==1)])>1 & length(id[which(!is.na(rlang::UQ(PerformanceMetric)) & id==2)])>1), pnorm((wilcox.test(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)))]~id[which(!is.na(rlang::UQ(PerformanceMetric)))])$statistic-numobs1*numobs2/2)/sqrt(numobs1*numobs2*(numobs1+numobs2+1)/12)),NA),
                test_statistic = ifelse(length(unique(id[which(!is.na(rlang::UQ(PerformanceMetric)))]))>1 & (length(id[which(!is.na(rlang::UQ(PerformanceMetric)) & id==1)])>1 & length(id[which(!is.na(rlang::UQ(PerformanceMetric)) & id==2)])>1),wilcox.test(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)))]~id[which(!is.na(rlang::UQ(PerformanceMetric)))])$statistic,NA))
    
    test_result <- test_result[,c(1,12,13,2,3,10,11,4,5,6,7,8,9)]
    
  }
  
  test_result[,"bin_cumulative"] = NULL
  test_result_ <- t(test_result)
  print(test_result_)
  writeWorksheetToFile(template,test_result_[1:4,],sheet="Cumulative",styleAction = XLC$STYLE_ACTION.NONE,startRow = 3, startCol = 2,header=F)
  writeWorksheetToFile(template,test_result_[5:12,],sheet="Cumulative",styleAction = XLC$STYLE_ACTION.NONE,startRow = 8, startCol = 2,header=F)
  
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #Calculate test statistics of other factors
  Factors_PILOT_TICK <- setdiff(Factors,c("PILOT_SECURITY","TICK_SIZE_USE","start_time","end_time"))
  num_factors <- length(Factors_PILOT_TICK)
  #------------------------------------------------------------------------------------------------------------------------------------
  test_pvalue <- data.frame(temp=rep(NA,1))
  test_pvalue <- test_pvalue %>% mutate(bin_cumulative=unique(matrix[,"bin_cumulative"]))
  test_pvalue$temp <- NULL
  test_statistic <- data.frame(temp = rep(NA,1))
  test_statistic <- test_statistic %>% mutate(bin_cumulative=unique(matrix[,"bin_cumulative"]))
  test_statistic$temp <- NULL
  
  if(method=="straight"){
    for (var in Factors_PILOT_TICK){
      var_quo <- rlang::sym(var)
      
      result_pvalue <- matrix %>% group_by(bin_cumulative) %>%
        summarise(temp = ifelse(length(unique(id[which(!is.na(rlang::UQ(var_quo)))]))>1 & (length(unique(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==1)]))>1 & length(unique(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==2)]))>1),pnorm(t.test(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)))]~id[which(!is.na(rlang::UQ(var_quo)))])$statistic),NA))
      test_pvalue <- left_join(test_pvalue,result_pvalue,by="bin_cumulative")
      
      result_statistic <- matrix %>% group_by(bin_cumulative) %>%
        summarise(temp = ifelse(length(unique(id[which(!is.na(rlang::UQ(var_quo)))]))>1 & (length(unique(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==1)]))>1 & length(unique(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==2)]))>1),t.test(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)))]~id[which(!is.na(rlang::UQ(var_quo)))])$statistic,NA))
      test_statistic <- left_join(test_statistic,result_statistic,by="bin_cumulative")
    }
  }
  
  else if (method=="weighted"){
    for (var in Factors_PILOT_TICK){
      var_quo <- rlang::sym(var)
      
      result_pvalue <- matrix %>% group_by(bin_cumulative) %>%
        summarise(wt_avg1 = wt.mean(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==1)],notional[which(!is.na(rlang::UQ(var_quo)) & id==1)]),
                  wt_avg2 = wt.mean(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==2)],notional[which(!is.na(rlang::UQ(var_quo)) & id==2)]),
                  df1 = sum(notional[which(!is.na(notional) & id==1)])**2/sum(notional[which(!is.na(notional) & id==1)]**2),
                  df2 = sum(notional[which(!is.na(notional) & id==2)])**2/sum(notional[which(!is.na(notional) & id==2)]**2),
                  std_error1 = wt.sd(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==1)],notional[which(!is.na(rlang::UQ(var_quo)) & id==1)])/sqrt(df1),
                  std_error2 = wt.sd(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==2)],notional[which(!is.na(rlang::UQ(var_quo)) & id==2)])/sqrt(df2),
                  pvalue = ifelse(length(unique(id[which(!is.na(rlang::UQ(var_quo)))]))>1 & (length(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==1)])>1 & length(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==2)])>1),pnorm((wt_avg1-wt_avg2)/(std_error1+std_error2)),NA),
                  test_statistic = ifelse(length(unique(id[which(!is.na(rlang::UQ(var_quo)))]))>1 & (length(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==1)])>1 & length(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==2)])>1),(wt_avg1-wt_avg2)/(std_error1+std_error2),NA))
      
      result_pvalue$wt_avg1 = NULL
      result_pvalue$wt_avg2 = NULL
      result_pvalue$df1 = NULL
      result_pvalue$df2 = NULL
      result_pvalue$std_error1 = NULL
      result_pvalue$std_error2 = NULL
      result_pvalue$test_statistic = NULL
      test_pvalue <- left_join(test_pvalue,result_pvalue,by="bin_cumulative")
      
      result_statistic <- matrix %>% group_by(bin_cumulative) %>%
        summarise(wt_avg1 = wt.mean(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==1)],notional[which(!is.na(rlang::UQ(var_quo)) & id==1)]),
                  wt_avg2 = wt.mean(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==2)],notional[which(!is.na(rlang::UQ(var_quo)) & id==2)]),
                  df1 = sum(notional[which(!is.na(notional) & id==1)])**2/sum(notional[which(!is.na(notional) & id==1)]**2),
                  df2 = sum(notional[which(!is.na(notional) & id==2)])**2/sum(notional[which(!is.na(notional) & id==2)]**2),
                  std_error1 = wt.sd(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==1)],notional[which(!is.na(rlang::UQ(var_quo)) & id==1)])/sqrt(df1),
                  std_error2 = wt.sd(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==2)],notional[which(!is.na(rlang::UQ(var_quo)) & id==2)])/sqrt(df2),
                  pvalue = ifelse(length(unique(id[which(!is.na(rlang::UQ(var_quo)))]))>1 & (length(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==1)])>1 & length(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==2)])>1),pnorm((wt_avg1-wt_avg2)/(std_error1+std_error2)),NA),
                  test_statistic = ifelse(length(unique(id[which(!is.na(rlang::UQ(var_quo)))]))>1 & (length(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==1)])>1 & length(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==2)])>1),(wt_avg1-wt_avg2)/(std_error1+std_error2),NA))
      result_statistic$wt_avg1 = NULL
      result_statistic$wt_avg2 = NULL
      result_statistic$df1 = NULL
      result_statistic$df2 = NULL
      result_statistic$std_error1 = NULL
      result_statistic$std_error2 = NULL
      result_statistic$pvalue = NULL
      test_statistic <- left_join(test_statistic,result_statistic,by="bin_cumulative")
    }
  }
  
  else{
    for (var in Factors_PILOT_TICK){
      var_quo <- rlang::sym(var)
      
      result_pvalue <- matrix %>% group_by(bin_cumulative) %>%
        summarise(numobs1 = length(which(!is.na(!!var_quo) & id==1)),
                  numobs2 = length(which(!is.na(!!var_quo) & id==2)),
                  temp = ifelse(length(unique(id[which(!is.na(rlang::UQ(var_quo)))]))>1 & (length(unique(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==1)]))>1 | length(unique(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==2)]))>1), pnorm((wilcox.test(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)))]~id[which(!is.na(rlang::UQ(var_quo)))])$statistic-numobs1*numobs2/2)/sqrt(numobs1*numobs2*(numobs1+numobs2+1)/12)),NA))
      result_pvalue$numobs1 <- NULL
      result_pvalue$numobs2 <- NULL
      test_pvalue <- left_join(test_pvalue,result_pvalue,by="bin_cumulative")
      
      result_statistic <- matrix %>% group_by(bin_cumulative) %>%
        summarise(numobs1 = length(which(!is.na(!!var_quo) & id==1)),
                  numobs2 = length(which(!is.na(!!var_quo) & id==2)),
                  temp = ifelse(length(unique(id[which(!is.na(rlang::UQ(var_quo)))]))>1 & (length(unique(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==1)]))>1 | length(unique(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==2)]))>1), wilcox.test(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)))]~id[which(!is.na(rlang::UQ(var_quo)))])$statistic,NA))
      
      test_statistic$numobs1 <- NULL
      test_statistic$numobs2 <- NULL
      test_statistic <- left_join(test_statistic,result_statistic,by="bin_cumulative")
      
    }
  }
  
  test_pvalue[,"bin_cumulative"] <- NULL
  test_statistic[,"bin_cumulative"] <- NULL
  test_pvalue_ <- t(test_pvalue)
  test_statistic_ <- t(test_statistic)
  
  writeWorksheetToFile(template,test_pvalue_[1:num_factors,],sheet="Cumulative",styleAction = XLC$STYLE_ACTION.NONE,startRow = 22, startCol = 2,header=F)
  writeWorksheetToFile(template,test_statistic_[1:num_factors,],sheet="Cumulative",styleAction = XLC$STYLE_ACTION.NONE,startRow = 45, startCol = 2,header=F)
  
  
  
  
  #Calculate average of other factors
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  other_factor1 <- data.frame(temp=rep(NA,1))
  other_factor1 <- other_factor1 %>% mutate(bin_cumulative=unique(matrix[,"bin_cumulative"]))
  other_factor1$temp <- NULL
  other_factor2 <- data.frame(temp=rep(NA,1))
  other_factor2 <- other_factor2 %>% mutate(bin_cumulative=unique(matrix[,"bin_cumulative"]))
  other_factor2$temp <- NULL
  
  if(method=="weighted"){
    for (var in Factors_PILOT_TICK){
      
      var_quo <- rlang::sym(var)
      
      result1 <- matrix %>% filter(id==1) %>%
        group_by(bin_cumulative) %>%
        summarise(average1 = wt.mean(!!var_quo,notional))
      other_factor1 <- left_join(other_factor1,result1,by="bin_cumulative")
      
      result2 <- matrix %>% filter(id==2) %>%
        group_by(bin_cumulative) %>%
        summarise(average2 = wt.mean(!!var_quo,notional))
      other_factor2 <- left_join(other_factor2,result2,by="bin_cumulative")
    }
  }
  
  else if(method=="straight"){
    for (var in Factors_PILOT_TICK){
      
      var_quo <- rlang::sym(var)
      
      result1 <- matrix %>% filter(id==1) %>%
        group_by(bin_cumulative) %>%
        summarise(average1 = mean(!!var_quo,na.rm=TRUE))
      other_factor1 <- left_join(other_factor1,result1,by="bin_cumulative")
      
      result2 <- matrix %>% filter(id==2) %>%
        group_by(bin_cumulative) %>%
        summarise(average2 = mean(!!var_quo,na.rm=TRUE))
      other_factor2 <- left_join(other_factor2,result2,by="bin_cumulative")
    }
  }
  
  else{
    for (var in Factors_PILOT_TICK){
      
      var_quo <- rlang::sym(var)
      
      result1 <- matrix %>% filter(id==1) %>%
        group_by(bin_cumulative) %>%
        summarise(average1 = median(!!var_quo,na.rm=TRUE))
      other_factor1 <- left_join(other_factor1,result1,by="bin_cumulative")
      
      result2 <- matrix %>% filter(id==2) %>%
        group_by(bin_cumulative) %>%
        summarise(average2 = median(!!var_quo,na.rm=TRUE))
      other_factor2 <- left_join(other_factor2,result2,by="bin_cumulative")
    }
  }
  
  #--------------------------------------------------------------------------------------------------------------------------------------
  other_factor1[,"bin_cumulative"] <- NULL
  other_factor2[,"bin_cumulative"] <- NULL
  other_factor1_ <- t(other_factor1)
  other_factor2_ <- t(other_factor2)
  writeWorksheetToFile(template,other_factor1_[1:num_factors,],sheet="Cumulative",styleAction = XLC$STYLE_ACTION.NONE,startRow = 68, startCol = 2,header=F)
  writeWorksheetToFile(template,other_factor2_[1:num_factors,],sheet="Cumulative",styleAction = XLC$STYLE_ACTION.NONE,startRow = 91, startCol = 2,header=F)
  
}