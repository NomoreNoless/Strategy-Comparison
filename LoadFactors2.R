LoadFactors2 <- function(Factors=c(),PerformanceMetric="",matrix,benchmark=Inf){
  
  
  if(benchmark<Inf){
    select_col <- c(Factors,PerformanceMetric)
    
  }
  else{
    select_col <- c(Factors,PerformanceMetric,"id")
  }
  
  if("PILOT_SECURITY" %in% Factors){
    select_col <- c(select_col,c("G","C","None"))
  }
  
  if(!("notional" %in% Factors)){
    select_col <- c(select_col,"notional")
  }
  
  if(PerformanceMetric=="VWAP_Slip_bps"){
    
    select_col <- c(select_col,"VWAP_Slip_bps")
    
    selected <- matrix[,select_col]
    
    selected <- data.frame(selected)

    selected <- selected %>% mutate(VWAP_Slip_bps = ifelse(id==1,-ratio_bps,VWAP_Slip_bps))
    
  }
  
  if(PerformanceMetric=="VWAP_Slip_spr"){
    
    select_col <- c(select_col,"VWAP_Slip_spr")
    
    selected <- matrix[,select_col]
    
    selected <- data.frame(selected)
    
    selected <- selected %>% mutate(VWAP_Slip_spr = ifelse(id==1,-ratio_spread,VWAP_Slip_spr))
  
  }
  
  
  return(list(selected=selected,Factors=Factors,PerformanceMetric=PerformanceMetric))
  
}

