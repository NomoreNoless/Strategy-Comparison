
source("LoadBins.R")

BinbyFactor <- function(matrix,Factors,percentage,default_bins=default_bins){

  
  matrix <- data.frame(matrix)
  
  if(percentage > 0){
    
    num_of_bin <- 1/percentage
    
    for (factor in Factors){
      
      if(factor == "PILOT_SECURITY"){
        
        print(paste0("Binning factor: ",factor))
        
        factor_quo <- rlang::sym(factor)
        bin_factor <- paste0("bin_",factor)
        bin_factor_quo <- rlang::sym(bin_factor)
      
        matrix <- matrix %>% mutate(rlang::UQ(bin_factor_quo) := as.factor(ifelse(is.na(rlang::UQ(factor_quo)),"None",rlang::UQ(factor_quo))))

      }
      
      else if(factor=="TICK_SIZE_USE"){
        
        print(paste0("Binning factor: ",factor))
        
        factor_quo <- rlang::sym(factor)
        bin_factor <- paste0("bin_",factor)
        bin_factor_quo <- rlang::sym(bin_factor)
        
        matrix <- matrix %>% mutate(rlang::UQ(bin_factor_quo):= as.factor(rlang::UQ(factor_quo)))
      }
      
      else if(factor=="start_time" | factor=="end_time"){
        
        print(paste0("Binning factor: ",factor))
        
        factor_quo <- rlang::sym(factor)
        bin_factor <- paste0("bin_",factor)
        bin_factor_quo <- rlang::sym(bin_factor)
        
        #use labels=FALSE to use integer to identify the group, this way when use group_by the result will be automatically order by number
        matrix <- matrix %>% mutate(rlang::UQ(bin_factor_quo) := cut(rlang::UQ(factor_quo),default_bins[[factor]],include.lowest=TRUE))
      }
      
      else{
        
        print(paste0("Binning factor: ",factor))
        
        factor_quo <- rlang::sym(factor)
        bin_factor <- paste0("bin_",factor)
        bin_factor_quo <- rlang::sym(bin_factor)
        temp <- matrix %>% filter(!is.na(rlang::UQ(factor_quo)))
        if(dim(temp)[1]>0){
          matrix <- matrix %>% mutate(RowNumber = row_number(rlang::UQ(factor_quo)))
          matrix <- matrix %>% mutate(rlang::UQ(bin_factor_quo) := cut(RowNumber,num_of_bin,labels=FALSE,include.lowest=TRUE))
        }
        else{
          matrix[,bin_factor] <- NA
          msgs <- paste0(factor," has no observations")
          print(msgs)
        }
      }
    }
  }
  
  else{
    
    for(factor in Factors){
      
      if(factor == "PILOT_SECURITY"){
        
        print(paste0("Binning factor: ",factor))
        
        factor_quo <- rlang::sym(factor)
        bin_factor <- paste0("bin_",factor)
        bin_factor_quo <- rlang::sym(bin_factor)
        
        matrix <- matrix %>% mutate(rlang::UQ(bin_factor_quo) := as.factor(ifelse(is.na(rlang::UQ(factor_quo)),"None",rlang::UQ(factor_quo))))

      }
      
      else if(factor=="TICK_SIZE_USE"){
        print(paste0("Binning factor: ",factor))
        
        factor_quo <- rlang::sym(factor)
        bin_factor <- paste0("bin_",factor)
        bin_factor_quo <- rlang::sym(bin_factor)
        
        matrix <- matrix %>% mutate(rlang::UQ(bin_factor_quo):= as.factor(rlang::UQ(factor_quo)))
      }
      
      else if(factor=="start_time"| factor=="end_time"){
        print(paste0("Binning factor: ",factor))
        
        factor_quo <- rlang::sym(factor)
        bin_factor <- paste0("bin_",factor)
        bin_factor_quo <- rlang::sym(bin_factor)
        
        matrix <- matrix %>% mutate(rlang::UQ(bin_factor_quo) := cut(rlang::UQ(factor_quo),default_bins[[factor]],include.lowest=TRUE))
      }
      
      else{
        print(paste0("Binning factor: ",factor))
        
        factor_quo <- rlang::sym(factor)
        bin_factor <- paste0("bin_",factor)
        bin_factor_quo <- rlang::sym(bin_factor)
        temp <- matrix %>% filter(!is.na(rlang::UQ(factor_quo)))
        if(dim(temp)[1]>0){
          
          matrix <- matrix %>% mutate(rlang::UQ(bin_factor_quo) := cut(rlang::UQ(factor_quo),default_bins[[factor]],labels=FALSE,include.lowest=TRUE))
          
        }
        else{
          matrix[,bin_factor] <- NA
          msgs <- paste0(factor," has no observations")
          print(msgs)
        }
      }
     
    }
  }
  
  matrix <- matrix %>% mutate(bin_cumulative=as.factor(1))
  return(matrix)
  
}