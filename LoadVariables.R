source("loadData1on1.R")
LoadVariables <- function(dataset){
  
  #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #notional
  dataset <- dataset %>% mutate(notional = abs(E_QTY_DONE)*AVG_PRC)
  #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #Duration
  dataset <- dataset %>% mutate(ALGO_START = ifelse(is.na(E_START_TIME),ADD_TIME_EXCH,E_START_TIME),
                                ALGO_END = ifelse(is.na(FIN_TIME),E_END_TIME,FIN_TIME))
  
  dataset <- dataset %>% mutate(ALGO_START_sec = as.numeric(substring(ALGO_START,1,2))*3600 + as.numeric(substring(ALGO_START,4,5))*60 + as.numeric(substring(ALGO_START,7,8)),
                                ALGO_END_sec = as.numeric(substring(ALGO_END,1,2))*3600 + as.numeric(substring(ALGO_END,4,5))*60 + as.numeric(substring(ALGO_END,7,8)))
  
  dataset <- dataset %>% mutate(duration = ALGO_END_sec-ALGO_START_sec,
                                duration = ifelse(duration<0,0,duration))
  
  dataset <- dataset %>% mutate(duration_sec = ALGO_END_sec-ALGO_START_sec,
                                duration_sec = ifelse(duration_sec<0,0,duration_sec))
  
  
  dataset <- dataset %>% mutate(duration_exe = duration_sec/EXEC_CT,
                                duration_exe = ifelse(!is.na(duration_exe) & duration_exe<Inf,duration_exe,NA))
  
  dataset <- dataset %>% mutate(duration_vol = PRD_VOL/VOL1,
                                duration_vol = ifelse(!is.na(duration_vol) & duration_vol<Inf,duration_vol,NA))
  
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #convert E_START_TIME to numeric values
  dataset <- dataset %>% mutate(start_time = as.numeric(gsub(":",".",substring(E_START_TIME,1,5))))
  
  #convert E_START_TIME to numeric values
  dataset <- dataset %>% mutate(end_time = as.numeric(gsub(":",".",substring(E_END_TIME,1,5))))
  
  #convert E_START_TIME to seconds
  dataset <- dataset %>% mutate(E_START_sec = as.numeric(substring(E_START_TIME,1,2))*3600 + as.numeric(substring(E_START_TIME,4,5))*60 + as.numeric(substring(E_START_TIME,7,8)))
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  dataset <- dataset %>% mutate(Vduration = PRD_VOL/VOL1,
                                Vduration = ifelse(!is.na(Vduration) & Vduration<Inf,Vduration,NA))
  
  dataset <- dataset %>% mutate(Eduration = duration/EXEC_CT,
                                Eduration = ifelse(!is.na(Eduration) & Eduration<Inf, Eduration,NA))
  
  dataset <- dataset %>% mutate(TopBook_to_OrderSize = AVERAGE_BA_SZ/E_QUANTITY_TARGET,
                                TopBook_to_OrderSize = ifelse(!is.na(TopBook_to_OrderSize) & TopBook_to_OrderSize<Inf,TopBook_to_OrderSize,NA),
                                Volume_to_TopBook = PRD_VOL/AVERAGE_BA_SZ,
                                Volume_to_TopBook = ifelse(!is.na(Volume_to_TopBook) & Volume_to_TopBook<Inf, Volume_to_TopBook,NA))
  
  dataset <- dataset %>% mutate(book = log(AVERAGE_BA_SZ))
  
  dataset <- dataset %>% mutate(size_pct_vol = abs(E_QUANTITY_TARGET)/PRD_VOL,
                                size_pct_vol = ifelse(!is.na(size_pct_vol) & size_pct_vol<Inf, size_pct_vol,NA))
  
  dataset <- dataset %>% mutate(realpart = abs(E_QTY_DONE)/PART_VOL,
                                realpart = ifelse(!is.na(realpart) & realpart<Inf,realpart,NA))
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #Order Size, Participation & Liquidity
  dataset <- dataset %>% mutate(adv = VOL1,
                                size_pct = abs(E_QUANTITY_TARGET)/PRD_VOL,
                                size_pct = ifelse(!is.na(size_pct) & size_pct<=1,size_pct,NA),
                                real_part = abs(E_QTY_DONE)/PART_VOL,
                                real_part = ifelse(!is.na(real_part) & real_part<=1,real_part,NA),
                                book_size = AVERAGE_BA_SZ/abs(E_QTY_DONE),
                                book_size = ifelse(!is.na(book_size) & book_size<Inf,book_size,NA),
                                vol_book = PRD_VOL/AVERAGE_BA_SZ,
                                vol_book = ifelse(!is.na(vol_book) & vol_book<Inf,vol_book,NA))
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #PILOT_SECURITY
  dataset <- dataset %>% mutate(PILOT_SECURITY = ifelse(PILOT_SECURITY %in% c("G1","G2","G3"),"G",PILOT_SECURITY))
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #Spread
  dataset <- dataset %>% mutate(spread = ifelse(SEC_COUNTRY=="USA",AVERAGE_SPREAD/0.01,NA),
                                spread_bps = AVERAGE_SPREAD/AVG_PRC*10000,
                                spread_bps = ifelse(spread_bps<Inf,spread_bps,NA),
                                spread_tic =AVERAGE_SPREAD/TICK_SIZE_USE)
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #ave_midrange/ave_spread
  
  dataset <- dataset %>% mutate(midrange_spread = AVERAGE_MID_RANGE/AVERAGE_SPREAD,
                                midrange_spread = ifelse(midrange_spread<Inf, midrange_spread,NA))
  
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #Volatility
  dataset <- dataset %>% mutate(midrange = ifelse(SEC_COUNTRY=="USA",AVERAGE_MID_RANGE/0.01,NA))
  dataset <- dataset %>% mutate(midrange_pct = 100*AVERAGE_MID_RANGE/AVG_PRC,
                                midrange_pct = ifelse(!is.na(midrange_pct) & midrange_pct<Inf,midrange_pct,NA))
  
  dataset <- dataset %>% mutate(mid_range_bps = AVERAGE_MID_RANGE/AVG_PRC*10000,
                                mid_range_bps = ifelse(!is.na(mid_range_bps) & mid_range_bps<Inf, mid_range_bps,NA))
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #PILOT_SECURITY
  dataset <- dataset %>% mutate(C = ifelse(PILOT_SECURITY=="C",1,0),
                                G = ifelse(PILOT_SECURITY=="G",1,0),
                                None = ifelse(is.na(PILOT_SECURITY),1,0))
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #return
  dataset <- dataset %>% mutate(return_pct = ifelse(is.na(LAST_CHG),-sign(E_QTY_DONE)*log(PREV_WAVE_MID_AT_END/MARK)*100,-sign(E_QTY_DONE)*log(LAST_CHG/MARK)*100),
                                return_pct = ifelse(!is.na(return_pct) & return_pct>-Inf & return_pct<Inf,return_pct,NA))
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  #Performance metrics
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  dataset <- dataset %>% mutate(VWAP_Slip_bps = -sign(E_QTY_DONE)*(1-AVG_PRC/PRD_VWAP)*10000,
                                VWAP_Slip_bps = ifelse(VWAP_Slip_bps>-Inf & VWAP_Slip_bps<Inf,VWAP_Slip_bps,NA),
                                VWAP_Slip_dollar = E_QTY_DONE*(PRD_VWAP-AVG_PRC),
                                VWAP_Slip_perc = VWAP_Slip_dollar/abs(sum(VWAP_Slip_dollar)),
                                VWAP_Slip_spr = sign(E_QTY_DONE)*(PRD_VWAP-AVG_PRC)/AVERAGE_SPREAD,
                                VWAP_Slip_cps = sign(E_QTY_DONE)*(PRD_VWAP-AVG_PRC)*100,
                                Arrival_Slip_bps = -sign(E_QTY_DONE)*(1-AVG_PRC/MARK)*10000,
                                Arrival_Slip_bps = ifelse(Arrival_Slip_bps>-Inf & Arrival_Slip_bps<Inf,Arrival_Slip_bps,NA),
                                Arrival_Slip_dollar = E_QTY_DONE*(MARK-AVG_PRC),
                                Arrival_Slip_perc = Arrival_Slip_dollar/abs(sum(Arrival_Slip_dollar)),
                                Arrival_Slip_spr = sign(E_QTY_DONE)*(MARK-AVG_PRC)/AVERAGE_SPREAD,
                                Arrival_Slip_cps = sign(E_QTY_DONE)*(MARK-AVG_PRC)*100,
                                Price_change_bps = sign(E_QTY_DONE)*(LAST_CHG-AVG_PRC)/AVG_PRC*10000)
                                #Reversion_bps = -sign(E_QTY_DONE)*(AveExecPx-CloseAuctionPx)/AveExecPx*10000,
                                # Reversion_bps = sign(E_QTY_DONE)*(CloseAuctionPx-last_fill)/last_fill*10000,
                                # Reversion_spread = sign(E_QTY_DONE)*(CloseAuctionPx-last_fill)/AVERAGE_SPREAD,
                                # Reversion_spread = ifelse(Reversion_spread>-Inf & Reversion_spread<Inf,Reversion_spread,NA),
                                # diff_perc1 = -sign(E_QTY_DONE)*(last_fill-MARK)/MARK - (-sign(E_QTY_DONE)*(CloseAuctionPx-last_fill)/last_fill),
                                #diff_slope = -sign(E_QTY_DONE)*(last_fill-MARK)/(last_fill_sec-E_START_sec) - (-sign(E_QTY_DONE)*(CloseAuctionPx-last_fill)/(CloseAuction_sec-last_fill_sec)),
                                #diff_perc_10_fill = -sign(E_QTY_DONE)*(last10fill-MARK)/MARK - (-sign(E_QTY_DONE)*(CloseAuctionPx-last10fill)/last10fill),
                                #diff_slope_10_fill = -sign(E_QTY_DONE)*(last10fill-MARK)/(last_fill_sec-E_START_sec) - (-sign(E_QTY_DONE)*(CloseAuctionPx-last10fill)/(CloseAuction_sec-last_fill_sec)),
                                # slope_before = -sign(E_QTY_DONE)*(last_fill-MARK)/(last_fill_sec-E_START_sec),
                                # slope_after = -sign(E_QTY_DONE)*(CloseAuctionPx-last_fill)/(CloseAuction_sec-last_fill_sec),
                                # slope_diff = slope_before - slope_after,
                                # slope_diff_normalize = (slope_before - slope_after)/abs(slope_before),
                                # slope_diff_normalize = ifelse(!is.na(slope_diff_normalize) & slope_diff_normalize>-Inf & slope_diff_normalize<Inf,slope_diff_normalize,NA),
                                # diff_VWAP_cancelcost = VWAP_Slip_bps - cancel_cost_bps)
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #change the name of EXEC_CT
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  dataset <- dataset %>% mutate(exec_ct = EXEC_CT)
  dataset <- dataset %>% mutate(mark = MARK)
  dataset$EXEC_CT <- NULL
  dataset$MARK <- NULL
  
  return(dataset)
  
}