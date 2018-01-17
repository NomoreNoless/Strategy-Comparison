
#method in c("straight","weighted")

source("Cumulative.R")

EvaluatePerformance_reverse <- function(matrix,Factors,PerformanceMetric,benchmark,method,percentage,filename,default_bins){
  
  num_factor <- length(Factors)
  
  if(percentage <0){
    template <- "C:/project/Strategy Comparison-Auction/running/Template-1vsB-defaultbins.xlsx"
    filename <- paste0(filename, "_","Default","_",method,"_",PerformanceMetric)
  }
  
  if(percentage > 0){
    template <- "C:/project/Strategy Comparison-Auction/running/Template-1vsB-quantile.xlsx"
    filename <- paste0(filename,"_","quantile","_",method,"_",PerformanceMetric)
  }
  
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #Cumulative Metric
  benchmark <- benchmark
  
  Cumulative(matrix,Factors,PerformanceMetric,method,template,benchmark=benchmark)
  
  
  PerformanceMetric <- rlang::sym(PerformanceMetric)
  
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
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
    benchmark_vec <- rep(0,all_obs)
    
    if (all_obs>0){
      
      print(paste0(factor," has observations."))
      print(all_obs)
      
      #-------------------------------------------------------------------------------------------------------------------------------
      #Find number of levels for each binned variable
      
      
      # if(factor=="TICK_SIZE_USE"){
      #   num_bin <- length(levels(dataset[,bin_factor]))
      #   temp_result <- data.frame(first_col=rep(1,num_bin))
      #   temp_result <- temp_result %>% mutate(rlang::UQ(bin_factor_quo) := as.factor(c(0.0001,0.01,0.05)))
      # }
      # else{
      #   num_bin <- length(levels(dataset[,bin_factor]))
      #   temp_result <- data.frame(first_col=rep(1,num_bin))
      #   temp_result <- temp_result %>% mutate(rlang::UQ(bin_factor_quo) := as.factor(levels(dataset[,bin_factor])))
      # }
      
      
      num_bin <- length(levels(dataset[,bin_factor]))
      temp_result <- data.frame(first_col=rep(1,num_bin))
      temp_result <- temp_result %>% mutate(rlang::UQ(bin_factor_quo) := as.factor(levels(dataset[,bin_factor])))
      
      temp_result$first_col <- NULL
      #print(temp_result)
      #-------------------------------------------------------------------------------------------------------------------------------
      
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
      
      #one-sample t test
      if(method=="straight"){
        test_result <- dataset %>% group_by(!!bin_factor_quo) %>%
          summarise(#pvalue = pnorm(t.test(rlang::UQ(PerformanceMetric),mu=benchmark)$statistic),
            #test_statistic = t.test(rlang::UQ(PerformanceMetric),mu=benchmark)$statistic)
            pvalue = ifelse(length(which(!is.na(rlang::UQ(PerformanceMetric))))>1, pnorm(t.test(rlang::UQ(PerformanceMetric),mu=benchmark)$statistic),NA),
            test_statistic = ifelse(length(which(!is.na(rlang::UQ(PerformanceMetric))))>1, t.test(rlang::UQ(PerformanceMetric),mu=benchmark)$statistic,NA))
        
        test_result$df <- NULL
        test_result <- left_join(result,test_result,by=bin_factor)
        
      }
      
      #method="weighted"
      else if(method=="weighted"){
        test_result <- dataset %>% group_by(!!bin_factor_quo) %>%
          summarise(wt_avg1 = wt.mean(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)))],notional[which(!is.na(rlang::UQ(PerformanceMetric)))]),
                    df1 = sum(notional[which(!is.na(notional))])**2/sum(notional[which(!is.na(notional))]**2),
                    std_error1 = wt.sd(rlang::UQ(PerformanceMetric)[which(!is.na(rlang::UQ(PerformanceMetric)))],notional[which(!is.na(rlang::UQ(PerformanceMetric)))])/sqrt(df1),
                    pvalue = ifelse(length(!is.na(rlang::UQ(PerformanceMetric)))>1,pnorm((wt_avg1-benchmark)/std_error1),NA),
                    test_statistic = ifelse(length(!is.na(rlang::UQ(PerformanceMetric)))>1,(wt_avg1-benchmark)/std_error1,NA))
        test_result$wt_avg1 <- NULL
        test_result$df1 <- NULL
        test_result$std_error1 <- NULL
        test_result <- left_join(result,test_result,by=bin_factor)
        
      }
      
      else{
        test_result <- dataset %>% group_by(!!bin_factor_quo) %>%
          summarise(df = n(),
                    pvalue = ifelse(length(which(!is.na(rlang::UQ(PerformanceMetric))))>1, pnorm((wilcox.test(rlang::UQ(PerformanceMetric),rep(0,df))$statistic-df*df/2)/sqrt(df*df*(2*df+1)/12)),NA),
                    test_statistic = ifelse(length(which(!is.na(rlang::UQ(PerformanceMetric))))>1, wilcox.test(rlang::UQ(PerformanceMetric),rep(0,df))$statistic,NA))
        
        test_result$df <- NULL
        test_result <- left_join(result,test_result,by=bin_factor)
      }
      
      
      
      if(method=="straight"){
        result1 <- dataset %>% group_by(!!bin_factor_quo) %>%
          summarise(average = mean(!!PerformanceMetric,na.rm=TRUE),
                    numobs = length(!is.na(!!factor_quo)),
                    std_error = sd(!!PerformanceMetric,na.rm=TRUE)/sqrt(numobs),
                    df = numobs,
                    perc = numobs/all_obs)
        
        result1 <- left_join(result,result1,by=bin_factor)
      }
      
      else if (method=="weighted"){
        result1 <- dataset %>% group_by(!!bin_factor_quo) %>%
          summarise(average = wt.mean(rlang::UQ(PerformanceMetric),notional),
                    df = sum(notional,na.rm=TRUE)**2/sum(notional**2,na.rm=TRUE),
                    std_error = wt.sd(rlang::UQ(PerformanceMetric),notional)/sqrt(df),
                    numobs = length(!is.na(!!factor_quo)),
                    perc = numobs/all_obs)
        
        result1 <- left_join(result,result1,by=bin_factor)
        
      }
      else{
        result1 <- dataset %>% group_by(!!bin_factor_quo) %>%
          summarise(average = median(!!PerformanceMetric,na.rm=TRUE),
                    numobs = length(!is.na(!!factor_quo)),
                    std_error = sd(!!PerformanceMetric,na.rm=TRUE)/sqrt(numobs),
                    df = numobs,
                    perc = numobs/all_obs)
        
        result1 <- left_join(result,result1,by=bin_factor)
      }
      
      result$pvalue <- test_result$pvalue
      result$statistic <- test_result$test_statistic
      result$average1 <- result1$average
      result$average2 <- benchmark
      result$std_error1 <- result1$std_error
      result$df1 <- result1$df
      result$numobs1 <- result1$numobs
      result$perc1 <- result1$perc
      result$median
      
      result <- left_join(temp_result,result,by=bin_factor)
      
      
      
      
      if(factor=="PILOT_SECURITY"){
        result[,bin_factor] <- NULL
        result[,c(1,2)] <- NULL
        result_ <- t(result)
        writeWorksheetToFile(template,result_[1:8,],sheet="pilot_cat",styleAction = XLC$STYLE_ACTION.NONE,startRow = 5, startCol = 2,header=F)
      }
      else if (factor=="TICK_SIZE_USE"){
        result[,bin_factor] <- NULL
        result[,c(1,2)] <- NULL
        result_ <- t(result)
        writeWorksheetToFile(template,result_[1:8,],sheet="tick_size",styleAction = XLC$STYLE_ACTION.NONE,startRow = 5, startCol = 2,header=F)
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
        writeWorksheetToFile(template,result_[1:2,],sheet=factor,styleAction = XLC$STYLE_ACTION.NONE,startRow = 3, startCol = 2,header=F)
        writeWorksheetToFile(template,result_[4:11,],sheet=factor,styleAction = XLC$STYLE_ACTION.NONE,startRow = 5, startCol = 2,header=F)
        writeWorksheetToFile(template,result_[3:4,],sheet=factor,styleAction = XLC$STYLE_ACTION.NONE,startRow = 76, startCol = 2,header=F)
        
        
      }
      
      #------------------------------------------------------------------------------------------------------------------------------------
      
      
      #Calculate average of other factors
      #------------------------------------------------------------------------------------------------------------------------------------
      Factors_PILOT_TICK <- setdiff(Factors,c("PILOT_SECURITY","TICK_SIZE_USE","start_time","end_time"))
      num_factors <- length(Factors_PILOT_TICK)
      #------------------------------------------------------------------------------------------------------------------------------------
      
      other_factor1 <- data.frame(first = rep(1,num_bin))
      if(factor=="TICK_SIZE_USE"){
        other_factor1 <- other_factor1 %>% mutate(rlang::UQ(bin_factor_quo):= as.factor(c(0.0001,0.01,0.05)))
      }
      else{
        other_factor1 <- other_factor1 %>% mutate(rlang::UQ(bin_factor_quo):=temp_result[,bin_factor])
      }
      other_factor1$first <- NULL
      if(method=="straight"){
        
        for (var in Factors_PILOT_TICK){
          
          var_quo <- rlang::sym(var)
          
          result1 <- dataset %>% 
            group_by(!!bin_factor_quo) %>%
            summarise(average1 = mean(rlang::UQ(var_quo),na.rm=TRUE))
          result1 <- left_join(other_factor1,result1,by=bin_factor)
          other_factor1[,var] = result1$average1
        }
      }
      
      else if (method=="weighted"){
        for (var in Factors_PILOT_TICK){
          
          var_quo <- rlang::sym(var)
          
          if(var!="notional"){
            result1 <- dataset %>% 
              group_by(!!bin_factor_quo) %>%
              summarise(average1 = wt.mean(rlang::UQ(var_quo),notional))
          }
          else{
            result1 <- dataset %>% 
              group_by(!!bin_factor_quo) %>%
              summarise(average1 = mean(rlang::UQ(var_quo),na.rm=TRUE))
          }
          
          result1 <- left_join(other_factor1,result1,by=bin_factor)
          other_factor1[,var] = result1$average1
        }
      }
      
      else{
        for (var in Factors_PILOT_TICK){
          
          var_quo <- rlang::sym(var)
          
          result1 <- dataset %>% 
            group_by(!!bin_factor_quo) %>%
            summarise(average1 = median(rlang::UQ(var_quo),na.rm=TRUE))
          result1 <- left_join(other_factor1,result1,by=bin_factor)
          other_factor1[,var] = result1$average1
        }
      }
      
      
      
      
      #--------------------------------------------------------------------------------------------------------------------------------------
      
      other_factor1[,bin_factor] <- NULL
      other_factor1_ <- t(other_factor1)
      if(factor=="TICK_SIZE_USE"){
        writeWorksheetToFile(template,other_factor1_[1:num_factors,],sheet="tick_size",styleAction = XLC$STYLE_ACTION.NONE,startRow = 21, startCol = 2,header=F)
        
      }
      else if (factor=="PILOT_SECURITY"){
        writeWorksheetToFile(template,other_factor1_[1:num_factors,],sheet="pilot_cat",styleAction = XLC$STYLE_ACTION.NONE,startRow = 21, startCol = 2,header=F)
      }
      else{
        writeWorksheetToFile(template,other_factor1_[1:num_factors,],sheet=factor,styleAction = XLC$STYLE_ACTION.NONE,startRow = 21, startCol = 2,header=F)
      }
    }
  }
  
  filename <- gsub("'","",filename)
  filename <- gsub(":","",filename)
  template2 <- paste0("C:/project/Strategy Comparison-Auction/running/",filename,".xlsx")
  file.rename(template,template2)
  
}