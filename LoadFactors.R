LoadFactors <- function(Factors=c(),PerformanceMetric="",matrix,benchmark=Inf){
  
  
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

  selected <- matrix[,select_col]
  
  return(list(selected=selected,Factors=Factors,PerformanceMetric=PerformanceMetric))
  
}

