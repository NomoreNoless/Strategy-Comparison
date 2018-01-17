MannWhitney_test <- function(sample1,sample2){
  
  #sample1 and sample2 have already filter out the NA values
  
  n1 <- length(sample1)
  n2 <- length(sample2)

  id1 <- rep(1,n1)
  id2 <- rep(2,n2)
  
  sample <- c(sample1,sample2)
  id <- c(id1,id2)
  
  temp <- data.frame(data=sample, id=id)
  
  temp <- temp %>% mutate(rank = row_number(data))
  
  result_temp <- temp %>% group_by(id) %>% summarise(ranksum = sum(rank,na.rm=TRUE))
  
  result_temp <- result_temp %>% arrange(ranksum)
  
  if(result_temp$id[1]==1){
    return(list(statistic=result_temp$ranksum[1],pvalue = pnorm((result_temp$ranksum[1]-n1*(n1+n2+1)/2)/sqrt(n1*n2*(n1+n2+1)/12))))
  }
  else{
    return(list(statistic=result_temp$ranksum[1],pvalue = 1 - pnorm((result_temp$ranksum[1]-n1*(n1+n2+1)/2)/sqrt(n1*n2*(n1+n2+1)/12))))
  }

}