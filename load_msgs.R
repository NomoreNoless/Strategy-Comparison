 
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#find msgs10,21,32 for QIM 

load_msgs <- function(firmname,year_month){
  

    print(paste0("Calculating month ",year_month))
    
    msgs10_21_32 <- order_msgs %>% filter(FirmName==firmname) %>% filter(msg_id %in% c(10,21,32)) %>%
      filter(as.integer(substring(transacttime,1,6))==year_month) %>%
      select(FirmName,tethysclientid,strategyid,compoundid,symbol,clordid,origclordid,msg_id,price,lastshares,lastpx,transacttime,t_ask,t_bid,side,ordtype) %>% collect()
    
    
    #fill msgs32 clordid wich origclordid
    msgs10_21_32 <- msgs10_21_32 %>% mutate(clordid = ifelse(msg_id==32,origclordid,clordid),
                                            mark21 = ifelse(msg_id==21, 1, 0))
    
    #dealing with limit orders
    #___________________________________________________________________________________________________________________________________________________________________
    msgs10_21_32 <- msgs10_21_32 %>% group_by(clordid) %>% 
      mutate(ordtype = ifelse(length(ordtype[which(!is.na(ordtype))])>0,unique(ordtype[which(!is.na(ordtype))]),NA))
    
    msgs10_21_32 <- msgs10_21_32 %>% filter(!is.na(ordtype),ordtype==2)
    #___________________________________________________________________________________________________________________________________________________________________
    
    #calculate date, spread, mid and time_in_second of msgs
    msgs10_21_32 <- msgs10_21_32 %>% mutate(day = as.integer(substring(transacttime,5,6))*100 + as.integer(substring(transacttime,7,8)),
                                            time_in_sec = (as.numeric(substring(transacttime,10,11))-4)*3600 + as.numeric(substring(transacttime,13,14))*60 + as.numeric(substring(transacttime,16,21)),
                                            spread = t_ask - t_bid,
                                            mid = 0.5*(t_ask+t_bid))
    
    msgs10_21_32 <- msgs10_21_32 %>% group_by(clordid) %>% mutate(fill_or_not = ifelse(sum(mark21)>0 & n()>1,1,ifelse(sum(mark21)==0 & n()>1,0,NA)))
    
    #If bad data, then fill_or_not should be NA
    msgs10_21_32 <- msgs10_21_32 %>% filter(!is.na(fill_or_not))
    
    
    msgs10_21_32 <- msgs10_21_32 %>% group_by(day,symbol) %>% 
      arrange(transacttime) %>%
      mutate(before_time = lag(time_in_sec,1),
             before_id = lag(msg_id,1),
             time_interval = ifelse(before_id %in% c(21,32),time_in_sec - before_time,0)) 
    
    
    msgs10 <- msgs10_21_32 %>% filter(msg_id==10,spread>0)
    
    #select passive orders
    # msgs10 <- msgs10 %>% mutate(Is_Passive = ifelse(((side==1 | side=="A") & price==t_bid )|((side==2 | side==5) & price==t_ask),1,0)) %>%
    #   filter(Is_Passive==1)
    
    msgs10 <- msgs10 %>% group_by(day,symbol) %>%
      arrange(transacttime) %>%
      mutate(pre_fill = lag(fill_or_not,1),
             post_fill = lead(fill_or_not,1),
             pre_fill = ifelse(is.na(pre_fill),1,pre_fill),
             post_fill = ifelse(is.na(post_fill),1,post_fill),
             temp = 100*pre_fill + 10*fill_or_not + post_fill,
             pre_far_price = ifelse(lag(side,1)==1|lag(side,1)=="A",lag(t_ask,1),lag(t_bid,1)))
    
    msgs10 <- msgs10 %>% mutate(start_mid = ifelse(temp %in% c(101,100),mid,NA),
                                end_fill = ifelse(temp %in% c(11,10) & time_interval <= 30,price,NA),
                                end_fill = ifelse(temp %in% c(11,10) & time_interval > 30, pre_far_price, end_fill))
    
    
    msgs10_1 <- msgs10 %>% filter(!is.na(start_mid) | !is.na(end_fill))
    msgs10_2 <- msgs10 %>% filter(fill_or_not==1)
    
    msgs10_1 <- msgs10_1 %>% group_by(day,symbol) %>% 
      arrange(transacttime) %>%
      mutate(move_end = lead(end_fill,1),
             move_spread = lead(spread,1),
             cancel_cost_bps = ifelse((side==1 | side=="A"),(move_end-start_mid)/start_mid*10000, -(move_end-start_mid)/start_mid*10000),
             cancel_cost_spread = ifelse((side==1 | side=="A"), (move_end-start_mid)/move_spread, -(move_end-start_mid)/move_spread),
             cancel_cost_bps = ifelse(!is.na(cancel_cost_bps) & cancel_cost_bps>-Inf & cancel_cost_bps<Inf,cancel_cost_bps,NA),
             cancel_cost_spread = ifelse(!is.na(cancel_cost_spread) & cancel_cost_spread > -Inf & cancel_cost_spread < Inf, cancel_cost_spread,NA))
    
    msgs10_1 <- msgs10_1 %>% filter(!is.na(cancel_cost_bps),!is.na(cancel_cost_spread))
    
    #delete the support column
    msgs10_1$move_end <- NULL
    
    msgs10_2 <- msgs10_2 %>% mutate(spread_after_cancel = NA,
                                    cancel_cost_bps = NA,
                                    cancel_cost_spread = NA)
    
    msgs32 <- rbind(msgs10_1,msgs10_2)
    
    
    #extract msgs21
    #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    msgs10_21_32 <- msgs10_21_32 %>% group_by(clordid) %>% mutate(price = ifelse(length(price[which(!is.na(price))])>0,unique(price[which(!is.na(price))]),NA),
                                                                  t_bid = ifelse(length(t_bid[which(!is.na(t_bid))])>0,unique(t_bid[which(!is.na(t_bid))]),NA),
                                                                  t_ask = ifelse(length(t_ask[which(!is.na(t_ask))])>0,unique(t_ask[which(!is.na(t_ask))]),NA))
    
    msgs21 <- msgs10_21_32 %>% filter(msg_id==21) %>% mutate(spread=t_ask-t_bid)
    
    msgs21 <- msgs21 %>% mutate(mid_calced = ifelse((side==1 | side=="A"),(t_ask + pmax(price,t_bid,na.rm=TRUE))/2, ifelse((side==2 | side==5),(t_bid + pmin(price,t_ask,na.rm=TRUE))/2,NA)))
    
    msgs21 <- msgs21 %>% mutate(Is_Passive_Fill = ifelse((side==1 | side=="A") & lastpx<t_ask,1,ifelse((side==2 | side==5) & lastpx>t_bid,1,0)))
    
    msgs21 <- msgs21 %>% mutate(effective_spread_abs = ifelse(Is_Passive_Fill==1,abs(lastpx-mid_calced),NA),
                                effective_spread_bps = ifelse(Is_Passive_Fill==1,abs(lastpx-mid_calced)/price*10000,NA))
    
    # msgs21 <- msgs21 %>% mutate(Is_Passive = ifelse(((side==1 | side=="A") & price==t_bid )|((side==2 | side==5) & price==t_ask),1,0))
    # 
    # msgs21 <- msgs21 %>% filter(Is_Passive==1)
    
    
    
    colnames(msgs32)[c(2,3,4,5)] <- c("TethysClientID","StrategyID","CompoundID","Ticker")
    colnames(msgs10)[c(2,3,4,5)] <- c("TethysClientID","StrategyID","CompoundID","Ticker")
    colnames(msgs21)[c(2,3,4,5)] <- c("TethysClientID","StrategyID","CompoundID","Ticker")
    
    remove(msgs10_1)
    remove(msgs10_2)
    remove(msgs10_21_32)
    
    return(list(msgs10_sequence = msgs32,msgs10_separate = msgs10,msgs21=msgs21))
    
    
  
}

                                                                  