source("load_msgs.R")
source("LoadVariables_compound.R")

Load_compound_variables <- function(dataset,FirmName,time_range){
  
  result_dataset <- data.frame()
  
  for(year_month in time_range){
    
    
    msgs_list <- load_msgs(FirmName,year_month)
    
    msgs10_sequence <- msgs_list$msgs10_sequence
    msgs10_separate <- msgs_list$msgs10_separate
    msgs21 <- msgs_list$msgs21
    
    join_dataset_21 <- inner_join(dataset,msgs21,by=c("FirmName","TethysClientID","StrategyID","CompoundID","Ticker"))
    join_dataset_sequence <- inner_join(dataset,msgs10_sequence,by=c("FirmName","TethysClientID","StrategyID","CompoundID","Ticker"))
    join_dataset_separate <- inner_join(dataset,msgs10_separate,by=c("FirmName","TethysClientID","StrategyID","CompoundID","Ticker"))
    #Calculate AveExecPx and CloseAuctionPx
    
    
    fill_price <- join_dataset_21 %>% group_by(FirmName,TethysClientID,StrategyID,CompoundID,Ticker) %>%
      summarise(AveExecPx = wt.mean(lastpx[which(time_in_sec<16*3600)],lastshares[which(time_in_sec<16*3600)]),
                last10fill = wt.mean(tail(lastpx[which(time_in_sec<16*3600)],10),tail(lastshares[which(time_in_sec<16*3600)],10)),
                last_fill = ifelse(length(lastpx[which(time_in_sec<16*3600)])>0,tail(lastpx[which(time_in_sec<16*3600)],1),NA),
                last_fill_sec = ifelse(length(lastpx[which(time_in_sec<16*3600)])>0,max(time_in_sec[which(time_in_sec<16*3600)]),NA),
                CloseAuctionPx = ifelse(length(lastpx[which(time_in_sec>=16*3600)])>0,unique(lastpx[which(time_in_sec>=16*3600)]),NA),
                CloseAuction_sec = ifelse(length(lastpx[which(time_in_sec>=16*3600)])>0,min(time_in_sec[which(time_in_sec>=16*3600)]),NA),
                Alpha = head(spread,1)/unique(AVERAGE_SPREAD),
                Alpha = ifelse(!is.na(Alpha) & Alpha > -Inf & Alpha < Inf, Alpha, NA),
                sff_bps = head(spread,1)/head(price,1)*10000,
                effective_spread_tics = mean(effective_spread_abs/TICK_SIZE_USE,na.rm=TRUE),
                effective_spread_bps = mean(effective_spread_bps,na.rm=TRUE))
    
    cancel_cost <- join_dataset_sequence %>% group_by(FirmName,TethysClientID,StrategyID,CompoundID,Ticker) %>%
      summarise(cancel_cost_bps = mean(cancel_cost_bps,na.rm=TRUE),
                cancel_cost_spread = mean(cancel_cost_spread,na.rm=TRUE),
                fill_ratio = sum(fill_or_not)/length(fill_or_not),
                #fill_ratio = ifelse(is.na(cancel_cost_bps) & is.na(cancel_cost_spread),NA,fill_ratio),
                Alpha2 = mean(spread_after_cancel,na.rm=TRUE)/unique(AVERAGE_SPREAD),
                Alpha2 = ifelse(!is.na(Alpha2) & Alpha2>-Inf & Alpha2 < Inf, Alpha2, NA),
                new_metric1_spread = -fill_ratio*0.5*unique(AVERAGE_SPREAD) + (1-fill_ratio)*cancel_cost_spread,
                new_metric1_bps = -fill_ratio*0.5*unique(AVERAGE_SPREAD)/unique(AVG_PRC)*10000 + (1-fill_ratio)*cancel_cost_bps)
    
    all_fill_ratio <- join_dataset_separate %>% group_by(FirmName,TethysClientID,StrategyID,CompoundID,Ticker) %>%
      summarise(cancel_rate = 1-sum(fill_or_not)/n())
    
    dataset_temp <- full_join(cancel_cost,fill_price,by=c("FirmName","TethysClientID","StrategyID","CompoundID","Ticker"))
    dataset_temp <- full_join(dataset_temp,all_fill_ratio,by=c("FirmName","TethysClientID","StrategyID","CompoundID","Ticker"))
    dataset_temp <- left_join(dataset_temp,dataset,by=c("FirmName","TethysClientID","StrategyID","CompoundID","Ticker"))
    
    dataset_temp <- dataset_temp %>% mutate(new_metric2_bps = -0.5*sff_bps*fill_ratio + (1-fill_ratio)*cancel_cost_bps,
                                            new_metric2_spread = -0.5*Alpha*(AVERAGE_SPREAD/AVG_PRC)*10000*fill_ratio + (1-fill_ratio)*cancel_cost_bps)
    
    
    
    if(dim(result_dataset)[[1]]==0){
      result_dataset <- dataset_temp
    }else{
      result_dataset <- rbind(result_dataset,dataset_temp)
    }
    
    remove(dataset_temp)
    remove(msgs21)
    remove(msgs10_separate)
    remove(msgs10_sequence)
    remove(join_dataset_21)
    remove(join_dataset_separate)
    remove(join_dataset_sequence)
    remove(fill_price)
    remove(cancel_cost)
    remove(all_fill_ratio)
  
  }
  
  result_dataset <- LoadVariables_compound(result_dataset)
  
  gc(verbose = FALSE)
  
  return(result_dataset)

}
