
source("MannWhitney_test.R")
#method in c("straight","weighted","nonparametric")

EvaluatePerformance_1vs1 <- function(matrix,Factors,PerformanceMetric,method,percentage,algo,filename,default_bins){
  
  if(percentage <0){
    template <- "C:/project/Strategy Comparison-Auction/running/Template-1vs1-defaultbins.xlsx"
    filename <- paste0(filename, "_","Default","_",method,"_",PerformanceMetric)
  }
  
  
  if(percentage > 0){
    template <- "C:/project/Strategy Comparison-Auction/running/Template-1vs1-quantile.xlsx"
    filename <- paste0(filename,"_","Quantile","_",method,"_",PerformanceMetric)
  }
  
  Cumulative_1vs1(matrix,Factors,PerformanceMetric,method,template)
  
  PerformanceMetric <- rlang::sym(PerformanceMetric)
  
  for(factor in Factors){
    
    print(factor)
    
    factor_quo <- rlang::sym(factor)
    
    bin_factor <- paste0("bin_",factor)
    bin_factor_quo <- rlang::sym(bin_factor)
    
    #Total number of observations filtering out NA values of independent factor
    if(factor=="PILOT_SECURITY"){
      dataset <- matrix %>% filter(!is.na(rlang::UQ(PerformanceMetric)))
    }
    else{
      dataset <- matrix %>% filter(!is.na(rlang::UQ(factor_quo)),!is.na(rlang::UQ(PerformanceMetric)))
    }
    
    all_obs <- dim(dataset)[1]
    
    if (all_obs>0){
      
      print(paste0(factor," has observations."))
      print(all_obs)
      
      #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      #Pay attention to sequence of factors
      
      num_bin <- length(levels(dataset[,bin_factor]))
      temp_result <- data.frame(first_col=rep(1,num_bin))
      temp_result <- temp_result %>% mutate(rlang::UQ(bin_factor_quo) := as.factor(levels(dataset[,bin_factor])))
      temp_result$first_col <- NULL
      
      
      #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
      dataset1 <- dataset %>% filter(id==1)
      all_obs1 <- dim(dataset1)[1]
      print(all_obs1)
      dataset2 <- dataset %>% filter(id==2)
      all_obs2 <- dim(dataset2)[1]
      print(all_obs2)
      #-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      if(factor=="PILOT_SECURITY"|factor=="TICK_SIZE_USE"){
        result <- dataset %>% group_by(!!bin_factor_quo) %>% 
          summarise(upper_bin = max(!!factor_quo,na.rm=TRUE),
                    lower_bin = min(!!factor_quo,na.rm=TRUE))
      }
      else{
        result <- dataset %>% group_by(!!bin_factor_quo) %>% 
          summarise(upper_bin = max(!!factor_quo,na.rm=TRUE),
                    lower_bin = min(!!factor_quo,na.rm=TRUE),
                    median_bin = median(!!factor_quo,na.rm=TRUE))
        
      }
  
      
      if(method=="straight"){
        test_result <- dataset %>% group_by(!!bin_factor_quo) %>%
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
        
        test_result <- dataset %>% group_by(!!bin_factor_quo) %>%
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
        
        test_result <- dataset %>% group_by(!!bin_factor_quo) %>%
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
                    pvalue = ifelse(length(unique(id[which(!is.na(rlang::UQ(PerformanceMetric)))]))>1 & (length(id[which(!is.na(rlang::UQ(PerformanceMetric)) & id==1)])>1 & length(id[which(!is.na(rlang::UQ(PerformanceMetric)) & id==2)])>1),MannWhitney_test(rlang::UQ(PerformanceMetric)[which(id==1)],rlang::UQ(PerformanceMetric)[which(id==2)])$pvalue,NA),
                    test_statistic = ifelse(length(unique(id[which(!is.na(rlang::UQ(PerformanceMetric)))]))>1 & (length(id[which(!is.na(rlang::UQ(PerformanceMetric)) & id==1)])>1 & length(id[which(!is.na(rlang::UQ(PerformanceMetric)) & id==2)])>1),MannWhitney_test(rlang::UQ(PerformanceMetric)[which(id==1)],rlang::UQ(PerformanceMetric)[which(id==2)])$statistic,NA))
        
        test_result <- test_result[,c(1,12,13,2,3,10,11,4,5,6,7,8,9)]
        
      }
      
      
      result <- left_join(temp_result,result,by=bin_factor)
      test_result <- left_join(temp_result,test_result,by=bin_factor)
    
      
      if(factor=="PILOT_SECURITY"){
        test_result[,bin_factor] <- NULL
        test_result_ <- t(test_result)
        writeWorksheetToFile(template,test_result_[1:4,],sheet="pilot_cat",styleAction = XLC$STYLE_ACTION.NONE,startRow = 5, startCol = 2,header=F)
        writeWorksheetToFile(template,test_result_[5:12,],sheet="pilot_cat",styleAction = XLC$STYLE_ACTION.NONE,startRow = 10, startCol = 2,header=F)
        
      }
      
      else if(factor=="TICK_SIZE_USE"){
        test_result[,bin_factor] <- NULL
        test_result_ <- t(test_result)
        writeWorksheetToFile(template,test_result_[1:4,],sheet="tick_size",styleAction = XLC$STYLE_ACTION.NONE,startRow = 5, startCol = 2,header=F)
        writeWorksheetToFile(template,test_result_[5:12,],sheet="tick_size",styleAction = XLC$STYLE_ACTION.NONE,startRow = 10, startCol = 2,header=F)
        
      }
      else{
        
        if(percentage<0|factor=="start_time"|factor=="end_time"){
          result$upper_bin <- default_bins[[factor]][2:11]
          result$upper_bin[10] <- NA
          result$lower_bin <- default_bins[[factor]][1:10]
          result$lower_bin[1] <- NA
        }
        else{
          result$upper_bin[10] <- NA
          result$lower_bin[1] <- NA
        }
        
        result[,bin_factor] <- NULL
        result_ <- t(result)
        test_result[,bin_factor] <- NULL
        test_result_ <- t(test_result)
        writeWorksheetToFile(template,result_[1:2,],sheet=factor,styleAction = XLC$STYLE_ACTION.NONE,startRow = 3, startCol = 2,header=F)
        writeWorksheetToFile(template,test_result_[1:4,],sheet=factor,styleAction = XLC$STYLE_ACTION.NONE,startRow = 5, startCol = 2,header=F)
        writeWorksheetToFile(template,test_result_[5:12,],sheet=factor,styleAction = XLC$STYLE_ACTION.NONE,startRow = 10, startCol = 2,header=F)
        
        #writeWorksheetToFile(template,result_[2:3,],sheet=factor,styleAction = XLC$STYLE_ACTION.NONE,startRow = 166, startCol = 2,header=F)
        
      }
      
      
      #------------------------------------------------------------------------------------------------------------------------------------
      
      
      
      
      #Calculate test statistics of other factors
      Factors_PILOT_TICK <- setdiff(Factors,c("PILOT_SECURITY","TICK_SIZE_USE","start_time","end_time"))
      num_factors <- length(Factors_PILOT_TICK)
      #------------------------------------------------------------------------------------------------------------------------------------
      test_pvalue <- data.frame(temp=rep(NA,num_bin))
      test_pvalue <- test_pvalue %>% mutate(rlang::UQ(bin_factor_quo):=temp_result[,bin_factor])
      test_pvalue$temp <- NULL
      test_statistic <- data.frame(temp = rep(NA,num_bin))
      test_statistic <- test_statistic %>% mutate(rlang::UQ(bin_factor_quo):=temp_result[,bin_factor])
      test_statistic$temp <- NULL
    
      if(method=="straight"){
        for (var in Factors_PILOT_TICK){
          var_quo <- rlang::sym(var)
          result_pvalue <- dataset %>% group_by(!!bin_factor_quo) %>%
            summarise(temp = ifelse(length(unique(id[which(!is.na(rlang::UQ(var_quo)))]))>1 & (length(unique(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==1)]))>1 & length(unique(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==2)]))>1),pnorm(t.test(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)))]~id[which(!is.na(rlang::UQ(var_quo)))])$statistic),NA))
          test_pvalue <- left_join(test_pvalue,result_pvalue,by=bin_factor)
          
          result_statistic <- dataset %>% group_by(!!bin_factor_quo) %>%
            summarise(temp = ifelse(length(unique(id[which(!is.na(rlang::UQ(var_quo)))]))>1 & (length(unique(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==1)]))>1 & length(unique(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==2)]))>1),t.test(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)))]~id[which(!is.na(rlang::UQ(var_quo)))])$statistic,NA))
          test_statistic <- left_join(test_statistic,result_statistic,by=bin_factor)
        }
      }
      
      else if (method=="weighted"){
        for (var in Factors_PILOT_TICK){
          var_quo <- rlang::sym(var)
          
          result_pvalue <- dataset %>% group_by(!!bin_factor_quo) %>%
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
          test_pvalue <- left_join(test_pvalue,result_pvalue,by=bin_factor)
          
          result_statistic <- dataset %>% group_by(!!bin_factor_quo) %>%
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
          test_statistic <- left_join(test_statistic,result_statistic,by=bin_factor)
        }
      }
      
      else{
        for (var in Factors_PILOT_TICK){
          var_quo <- rlang::sym(var)
        
          result_pvalue <- dataset %>% group_by(!!bin_factor_quo) %>%
            summarise(numobs1 = length(which(!is.na(!!var_quo) & id==1)),
                      numobs2 = length(which(!is.na(!!var_quo) & id==2)),
                      temp = ifelse(length(unique(id[which(!is.na(rlang::UQ(var_quo)))]))>1 & (length(unique(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==1)]))>1 | length(unique(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==2)]))>1), pnorm((wilcox.test(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)))]~id[which(!is.na(rlang::UQ(var_quo)))])$statistic-numobs1*numobs2/2)/sqrt(numobs1*numobs2*(numobs1+numobs2+1)/12)),NA))
          result_pvalue$numobs1 <- NULL
          result_pvalue$numobs2 <- NULL
          test_pvalue <- left_join(test_pvalue,result_pvalue,by=bin_factor)
        
          result_statistic <- dataset %>% group_by(!!bin_factor_quo) %>%
            summarise(numobs1 = length(which(!is.na(!!var_quo) & id==1)),
                      numobs2 = length(which(!is.na(!!var_quo) & id==2)),
                      temp = ifelse(length(unique(id[which(!is.na(rlang::UQ(var_quo)))]))>1 & (length(unique(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==1)]))>1 | length(unique(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)) & id==2)]))>1), wilcox.test(rlang::UQ(var_quo)[which(!is.na(rlang::UQ(var_quo)))]~id[which(!is.na(rlang::UQ(var_quo)))])$statistic,NA))
          
          test_statistic$numobs1 <- NULL
          test_statistic$numobs2 <- NULL
          test_statistic <- left_join(test_statistic,result_statistic,by=bin_factor)
          
        }
      }
      
      test_pvalue[,bin_factor] <- NULL
      test_statistic[,bin_factor] <- NULL
      test_pvalue_ <- t(test_pvalue)
      test_statistic_ <- t(test_statistic)
      
      if(factor=="PILOT_SECURITY"){
        writeWorksheetToFile(template,test_pvalue_[1:num_factors,],sheet="pilot_cat",styleAction = XLC$STYLE_ACTION.NONE,startRow = 22, startCol = 2,header=F)
        writeWorksheetToFile(template,test_statistic_[1:num_factors,],sheet="pilot_cat",styleAction = XLC$STYLE_ACTION.NONE,startRow = 45, startCol = 2,header=F)
      }
      else if (factor=="TICK_SIZE_USE"){
        writeWorksheetToFile(template,test_pvalue_[1:num_factors,],sheet="tick_size",styleAction = XLC$STYLE_ACTION.NONE,startRow = 22, startCol = 2,header=F)
        writeWorksheetToFile(template,test_statistic_[1:num_factors,],sheet="tick_size",styleAction = XLC$STYLE_ACTION.NONE,startRow = 45, startCol = 2,header=F)
      }
      else{
        writeWorksheetToFile(template,test_pvalue_[1:num_factors,],sheet=factor,styleAction = XLC$STYLE_ACTION.NONE,startRow = 22, startCol = 2,header=F)
        writeWorksheetToFile(template,test_statistic_[1:num_factors,],sheet=factor,styleAction = XLC$STYLE_ACTION.NONE,startRow = 45, startCol = 2,header=F)
      }
    
      #Calculate average of other factors
      #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      
      other_factor1 <- data.frame(temp=rep(NA,num_bin))
      other_factor1 <- other_factor1 %>% mutate(rlang::UQ(bin_factor_quo):=temp_result[,bin_factor])
      other_factor1$temp <- NULL
      other_factor2 <- data.frame(temp=rep(NA,num_bin))
      other_factor2 <- other_factor2 %>% mutate(rlang::UQ(bin_factor_quo):=temp_result[,bin_factor])
      other_factor2$temp <- NULL
      
      if(method=="weighted"){
        for (var in Factors_PILOT_TICK){
          
          var_quo <- rlang::sym(var)
          
          result1 <- dataset %>% filter(id==1) %>%
            group_by(!!bin_factor_quo) %>%
            summarise(average1 = wt.mean(!!var_quo,notional))
          other_factor1 <- left_join(other_factor1,result1,by=bin_factor)
          
          result2 <- dataset %>% filter(id==2) %>%
            group_by(!!bin_factor_quo) %>%
            summarise(average2 = wt.mean(!!var_quo,notional))
          other_factor2 <- left_join(other_factor2,result2,by=bin_factor)
        }
      }
      
      else if(method=="straight"){
        for (var in Factors_PILOT_TICK){
          
          var_quo <- rlang::sym(var)
          
          result1 <- dataset %>% filter(id==1) %>%
            group_by(!!bin_factor_quo) %>%
            summarise(average1 = mean(!!var_quo,na.rm=TRUE))
          other_factor1 <- left_join(other_factor1,result1,by=bin_factor)
          
          result2 <- dataset %>% filter(id==2) %>%
            group_by(!!bin_factor_quo) %>%
            summarise(average2 = mean(!!var_quo,na.rm=TRUE))
          other_factor2 <- left_join(other_factor2,result2,by=bin_factor)
        }
      }
      
      else{
        for (var in Factors_PILOT_TICK){
          
          var_quo <- rlang::sym(var)
          
          result1 <- dataset %>% filter(id==1) %>%
            group_by(!!bin_factor_quo) %>%
            summarise(average1 = median(!!var_quo,na.rm=TRUE))
          other_factor1 <- left_join(other_factor1,result1,by=bin_factor)
          
          result2 <- dataset %>% filter(id==2) %>%
            group_by(!!bin_factor_quo) %>%
            summarise(average2 = median(!!var_quo,na.rm=TRUE))
          other_factor2 <- left_join(other_factor2,result2,by=bin_factor)
        }
      }
      
    #--------------------------------------------------------------------------------------------------------------------------------------
      other_factor1[,bin_factor] <- NULL
      other_factor2[,bin_factor] <- NULL
      other_factor1_ <- t(other_factor1)
      other_factor2_ <- t(other_factor2)
      if(factor=="PILOT_SECURITY"){
        writeWorksheetToFile(template,other_factor1_[1:num_factors,],sheet="pilot_cat",styleAction = XLC$STYLE_ACTION.NONE,startRow = 68, startCol = 2,header=F)
        writeWorksheetToFile(template,other_factor2_[1:num_factors,],sheet="pilot_cat",styleAction = XLC$STYLE_ACTION.NONE,startRow = 91, startCol = 2,header=F)
      }
      else if (factor=="TICK_SIZE_USE"){
        writeWorksheetToFile(template,other_factor1_[1:num_factors,],sheet="tick_size",styleAction = XLC$STYLE_ACTION.NONE,startRow = 68, startCol = 2,header=F)
        writeWorksheetToFile(template,other_factor2_[1:num_factors,],sheet="tick_size",styleAction = XLC$STYLE_ACTION.NONE,startRow = 91, startCol = 2,header=F)
      }
      else{
        writeWorksheetToFile(template,other_factor1_[1:num_factors,],sheet=factor,styleAction = XLC$STYLE_ACTION.NONE,startRow = 68, startCol = 2,header=F)
        writeWorksheetToFile(template,other_factor2_[1:num_factors,],sheet=factor,styleAction = XLC$STYLE_ACTION.NONE,startRow = 91, startCol = 2,header=F)
      }
    }
  }
  
  filename <- gsub("'","",filename)
  filename <- gsub(":","",filename)
  template2 <- paste0("C:/project/Strategy Comparison-Auction/running/",filename,".xlsx")
  file.rename(template,template2)

}