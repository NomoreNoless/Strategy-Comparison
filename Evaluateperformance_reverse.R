
#method in c("straight","weighted")

source("Cumulative.R")

EvaluatePerformance_reverse <- function(matrix,Factors,PerformanceMetric,method,percentage,filename,default_bins){
  
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
  
  Cumulative(matrix,Factors,PerformanceMetric,method,template,benchmark=0.5)
  
  PerformanceMetric_quo <- rlang::sym(PerformanceMetric)
  
  bin_PerformanceMetric <- paste0("bin_",PerformanceMetric)
  bin_PerformanceMetric_quo <- rlang::sym(bin_PerformanceMetric)
  
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  for(factor in Factors){
    
    print(factor)
    
    factor_quo <- rlang::sym(factor)
    
    bin_factor <- paste0("bin_",factor)
    bin_factor_quo <- rlang::sym(bin_factor)
    
    #filter out NA values of independent and dependent factor
    dataset <- matrix %>% filter(!is.na(rlang::UQ(factor_quo)),!is.na(rlang::UQ(PerformanceMetric_quo)))
    
    all_obs <- dim(dataset)[1]
    
    if (all_obs>0){
      
      print(paste0(factor," has observations with PerformanceMetric not NA "))
      print(all_obs)
      
      #-------------------------------------------------------------------------------------------------------------------------------
      
      #calculate straight average factor values
      benchmark_straight <- mean(dataset[,factor])
      #calculate weighted average factor values
      benchmark_weighted <- wt.mean(dataset[,factor],dataset[,"notional"])
      
      #-------------------------------------------------------------------------------------------------------------------------------
      
      #one-sample t test
      
      if(method=="straight"){
        test_result <- dataset %>% group_by(!!bin_PerformanceMetric_quo) %>%
          summarise(upper_bin = max(!!PerformanceMetric_quo),
                    lower_bin = min(!!PerformanceMetric_quo),
                    pvalue = pnorm(t.test(rlang::UQ(factor_quo),mu=benchmark_straight)$statistic),
                    test_statistic = t.test(rlang::UQ(factor_quo),mu=benchmark_straight)$statistic,
                    average = mean(rlang::UQ(factor_quo)),
                    benchmark = benchmark_straight,
                    std_error = sd(rlang::UQ(factor_quo))/sqrt(n()),
                    df = n(),
                    numobs = n(),
                    perc = n()/all_obs,
                    median_bin = median(!!PerformanceMetric_quo))
      }
      
      #wt.mean() ignore NA values automatically
      if(method=="weighted"){
        
        if(factor!="notional"){
          test_result <- dataset %>% group_by(!!bin_PerformanceMetric_quo) %>%
            summarise(upper_bin = max(!!PerformanceMetric_quo),
                      lower_bin = min(!!PerformanceMetric_quo),
                      pvalue = pnorm((wt.mean(rlang::UQ(factor_quo),notional)-benchmark_weighted)/(wt.sd(rlang::UQ(factor_quo),notional)/sqrt(sum(notional,na.rm=TRUE)**2/sum(notional**2,na.rm=TRUE)))),
                      test_statistic = (wt.mean(rlang::UQ(factor_quo),notional)-benchmark_weighted)/(wt.sd(rlang::UQ(factor_quo),notional)/sqrt(sum(notional,na.rm=TRUE)**2/sum(notional**2,na.rm=TRUE))),
                      average = wt.mean(rlang::UQ(factor_quo),notional),
                      benchmark = benchmark_weighted,
                      std_error = wt.sd(rlang::UQ(factor_quo),notional)/sqrt(sum(notional,na.rm=TRUE)**2/sum(notional**2,na.rm=TRUE)),
                      df = sum(notional,na.rm=TRUE)**2/sum(notional**2,na.rm=TRUE),
                      numobs = n(),
                      perc = n()/all_obs,
                      median_bin = median(!!PerformanceMetric_quo))
        }
        else{
          test_result <- dataset %>% group_by(!!bin_PerformanceMetric_quo) %>%
            summarise(upper_bin = max(!!PerformanceMetric_quo),
                      lower_bin = min(!!PerformanceMetric_quo),
                      pvalue = pnorm(t.test(rlang::UQ(factor_quo),mu=benchmark_straight)$statistic),
                      test_statistic = t.test(rlang::UQ(factor_quo),mu=benchmark_straight)$statistic,
                      average = mean(rlang::UQ(factor_quo)),
                      benchmark = benchmark_straight,
                      std_error = sd(rlang::UQ(factor_quo))/sqrt(n()),
                      df = n(),
                      numobs = n(),
                      perc = n()/all_obs,
                      median_bin = median(!!PerformanceMetric_quo))
        }
       
      }
      
      
      # test_result$upper_bin[10] <- NA
      # test_result$lower_bin[1] <- NA
      
      test_result_T <- t(test_result)
      
      writeWorksheetToFile(template,test_result_T[2:11,],sheet=factor,styleAction = XLC$STYLE_ACTION.NONE,startRow = 3, startCol = 2,header=F)
      writeWorksheetToFile(template,test_result_T[11:12,],sheet=factor,styleAction = XLC$STYLE_ACTION.NONE,startRow = 80, startCol = 2,header=F)
        
      
      
      #------------------------------------------------------------------------------------------------------------------------------------
      
      
      #Calculate average of other factors
      #------------------------------------------------------------------------------------------------------------------------------------
      num_factors <- length(Factors)
      #------------------------------------------------------------------------------------------------------------------------------------
      
      result_average <- data.frame()
      
      if(method=="straight"){
        
        for (var in Factors){
          
          var_quo <- rlang::sym(var)
          
          result_temp <- dataset %>% group_by(!!bin_PerformanceMetric_quo) %>%
            summarise(rlang::UQ(factor_quo) := mean(rlang::UQ(var_quo),na.rm=TRUE))
          
          if(dim(result_average)[1]==0){
            result_average <- result_temp
          }
          else{
            result_average <- left_join(result_average,result_temp,by=bin_PerformanceMetric)
          }
          
        }
      }
      
      if(method=="weighted"){
        
        for (var in Factors){
          
          var_quo <- rlang::sym(var)
          
          if(var!="notional"){
            result_temp <- dataset %>% group_by(!!bin_PerformanceMetric_quo) %>%
              summarise(rlang::UQ(factor_quo) := wt.mean(rlang::UQ(var_quo),notional))
          }
          else{
            result_temp <- dataset %>% group_by(!!bin_PerformanceMetric_quo) %>%
              summarise(rlang::UQ(factor_quo) := mean(rlang::UQ(var_quo),na.rm=TRUE))
          }
          
          if(dim(result_average)[1]==0){
            result_average <- result_temp
          }
          else{
            result_average <- left_join(result_average,result_temp,by=bin_PerformanceMetric)
          }

        }
      }
      
      
      result_average_T <- t(result_average)
      
      writeWorksheetToFile(template,result_average_T[2:(num_factors+1),],sheet=factor,styleAction = XLC$STYLE_ACTION.NONE,startRow = 21, startCol = 2,header=F)
    
      
    }
  }
  
  filename <- gsub("'","",filename)
  filename <- gsub(":","",filename)
  template2 <- paste0("C:/project/Strategy Comparison-Auction/running/",filename,".xlsx")
  file.rename(template,template2)
  
}