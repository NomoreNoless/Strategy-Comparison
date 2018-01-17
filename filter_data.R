filter_data <- function(matrix,factor,range,filename){
  factor_quo <- rlang::sym(factor)
  
  filtered_data <- matrix %>% filter(rlang::UQ(factor_quo)>range[1],rlang::UQ(factor_quo)<=range[2])
  
  filename <- paste0(filename,factor,">",as.character(filename[1]),"&",factor,"<",as.character(filename[2]))
  return(filtered_data)
}