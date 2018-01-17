Cumulative <- function(matrix,Factors,PerformanceMetric,method,template,benchmark=0){
  
  benchmark <- benchmark
  
  PerformanceMetric_quo <- rlang::sym(PerformanceMetric)
  
  matrix <- matrix %>% filter(!is.na(rlang::UQ(PerformanceMetric_quo)))
  
  
  Factors_PILOT_TICK <- setdiff(Factors,c("PILOT_SECURITY","TICK_SIZE_USE","start_time","end_time"))
  num_factors <- length(Factors_PILOT_TICK)
  
  
  if(method=="straight"){
    test_result <- data.frame(pvalue = pnorm(t.test(matrix[,PerformanceMetric],mu=benchmark)$statistic),
                              test_statistic = t.test(matrix[,PerformanceMetric],mu=benchmark)$statistic)
    
    test_result_ <- t(test_result)
    writeWorksheetToFile(template,test_result_,sheet="Cumulative",styleAction = XLC$STYLE_ACTION.NONE,startRow = 3, startCol = 2,header=F)
    
  }
  
  else if (method=="weighted"){
    
    notional <- matrix[,"notional"]
    
    df = sum(notional[which(!is.na(notional))])**2/sum(notional[which(!is.na(notional))]**2)
    average=wt.mean(matrix[,PerformanceMetric],notional)
    std_error = wt.sd(matrix[,PerformanceMetric],matrix[,"notional"])/sqrt(df)
    
    test_result <- data.frame(average=wt.mean(matrix[,PerformanceMetric],notional),
                              df = sum(notional[which(!is.na(notional))])**2/sum(notional[which(!is.na(notional))]**2),
                              std_error = wt.sd(matrix[,PerformanceMetric],matrix[,"notional"])/sqrt(df),
                              pvalue = pnorm((average-benchmark)/std_error),
                              test_statistic = (average-benchmark)/std_error)
    test_result$average <- NULL
    test_result$df <- NULL
    test_result$std_error <- NULL
    
    test_result_ <- t(test_result)
    writeWorksheetToFile(template,test_result_,sheet="Cumulative",styleAction = XLC$STYLE_ACTION.NONE,startRow = 3, startCol = 2,header=F)
    
  }
  else{
    
    df <- length(matrix[,PerformanceMetric])
    test_result <- data.frame(df=length(matrix[,PerformanceMetric]),
                              pvalue = pnorm((wilcox.test(matrix[,PerformanceMetric],rep(0,df))$statistic-df*df/2)/sqrt(df*df*(2*df+1)/12)),
                              test_statistic = (wilcox.test(matrix[,PerformanceMetric],rep(0,df))$statistic-df*df/2)/sqrt(df*df*(2*df+1)/12))
    
    test_result$df <- NULL
    
    test_result_ <- t(test_result)
    writeWorksheetToFile(template,test_result_,sheet="Cumulative",styleAction = XLC$STYLE_ACTION.NONE,startRow = 3, startCol = 2,header=F)
    
  }
  
  if(method=="straight"){
    
    df <- length(which(!is.na(matrix[,PerformanceMetric])))
           
    result1 <- data.frame(average=mean(matrix[,PerformanceMetric],na.rm=TRUE),
                          benchmark = benchmark,
                          df = length(matrix[,PerformanceMetric]),
                          std_error = sd(matrix[,PerformanceMetric],na.rm=TRUE)/sqrt(df),
                          numobs = length(matrix[,PerformanceMetric]),
                          perc = 1)
    
    result1 <- result1[,c(1,2,4,3,5,6)]
    result1_ <- t(result1)
    
    writeWorksheetToFile(template,result1_,sheet="Cumulative",styleAction = XLC$STYLE_ACTION.NONE,startRow = 5, startCol = 2,header=F)
  
  }
  
  else if (method=="weighted"){
    
    notional <- matrix[,"notional"]
    
    df = sum(notional[which(!is.na(notional))])**2/sum(notional[which(!is.na(notional))]**2)
  
    result1 <- data.frame(average=wt.mean(matrix[,PerformanceMetric],matrix[,"notional"]),
                          benchmark = benchmark,
                          df = sum(notional[which(!is.na(notional))])**2/sum(notional[which(!is.na(notional))]**2),
                          std_error = wt.sd(matrix[,PerformanceMetric],matrix[,"notional"])/sqrt(df),
                          numobs = length(matrix[,PerformanceMetric]),
                          perc = 1)
    
    result1 <- result1[,c(1,2,4,3,5,6)]
    result1_ <- t(result1)
    
    writeWorksheetToFile(template,result1_,sheet="Cumulative",styleAction = XLC$STYLE_ACTION.NONE,startRow = 5, startCol = 2,header=F)
    
  }
  else {
    
    df <- length(which(!is.na(matrix[,PerformanceMetric])))
    
    result1 <- data.frame(median_=median(matrix[,PerformanceMetric],na.rm=TRUE),
                          benchmark = benchmark,
                          df = length(matrix[,PerformanceMetric]),
                          std_error = sd(matrix[,PerformanceMetric],na.rm=TRUE)/sqrt(df),
                          numobs = length(matrix[,PerformanceMetric]),
                          perc = 1)
    
    result1 <- result1[,c(1,2,4,3,5,6)]
    result1_ <- t(result1)
    
    writeWorksheetToFile(template,result1_,sheet="Cumulative",styleAction = XLC$STYLE_ACTION.NONE,startRow = 5, startCol = 2,header=F)
    
  }
  
  #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #Calculate average of other factors
  #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  result2 <- data.frame(temp=1)
  if(method=="straight"){
    
    for (factor in Factors_PILOT_TICK){
      
      factor_quo <- rlang::sym(factor)
        
      result2 <- result2 %>% mutate(rlang::UQ(factor_quo):= mean(matrix[,factor],na.rm=TRUE))
    }
  }
  
  else if (method=="weighted"){
  
    for (factor in Factors_PILOT_TICK){
      
      factor_quo <- rlang::sym(factor)
        
      if(factor != "notional"){
        result2 <- result2 %>% mutate(rlang::UQ(factor_quo):= wt.mean(matrix[,factor],matrix[,"notional"]))
      }
      else{
        result2 <- result2 %>% mutate(rlang::UQ(factor_quo):= mean(matrix[,factor],na.rm=TRUE))
      }
       
    }
  }
  
  else{
    
    for (factor in Factors_PILOT_TICK){
      
      factor_quo <- rlang::sym(factor)
        
      result2 <- result2 %>% mutate(rlang::UQ(factor_quo):= median(matrix[,factor],na.rm=TRUE))
        
    }
  }
  
    
  result2$temp <- NULL
  result2_ <- t(result2)
  writeWorksheetToFile(template,result2_[1:num_factors,],sheet="Cumulative",styleAction = XLC$STYLE_ACTION.NONE,startRow = 17, startCol = 2,header=F)
    
 
  
}