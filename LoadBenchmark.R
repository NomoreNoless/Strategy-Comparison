LoadBenchmark <- function(algo,PerformanceMetric){
  
  if(algo=="ESENS"){
    benchmark <- -2.0
  }
  
  if(algo=="EVWAP"){
    benchmark <- -1.5
  }
  
  if(algo=="EMINI" & PerformanceMetric=="VWAP_Slip_bps"){
    benchmark <- 1.35
  }
  
  if(algo=="EMINI" & PerformanceMetric=="Arrival_Slip_bps"){
    benchmark <- -3
  }
  return(benchmark)
}