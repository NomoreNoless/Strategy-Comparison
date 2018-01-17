Limit_condition_filter <- function(dataset){
  
  dataset <- dataset %>% mutate(Limit_price_condition = ifelse(VERSION__>="V10" & !is.na(LIMIT_PROBLEM) & LIMIT_PROBLEM==2,1,0))
  
  dataset1 <- dataset %>% filter(!is.na(E_LIMIT_PRC))
  dataset2 <- dataset %>% filter(is.na(E_LIMIT_PRC))
  
  dataset1 <- dataset1 %>% mutate(Limit_price_condition = ifelse(E_QTY_DONE>0,ifelse(E_LIMIT_PRC <= pmax(MARK,LAST_CHG,na.rm=TRUE),1,ifelse(!is.na(PRD_VWAP) & E_LIMIT_PRC<=PRD_VWAP,1,Limit_price_condition)),ifelse(E_LIMIT_PRC >= pmin(MARK,LAST_CHG,na.rm=TRUE),1,ifelse(!is.na(PRD_VWAP) & E_LIMIT_PRC>=PRD_VWAP,1,Limit_price_condition))))
  dataset <- rbind(dataset1, dataset2)
  
  dataset <- dataset %>% filter(Limit_price_condition==0)
  
  gc(verbose = FALSE)
  return(dataset)
  
}