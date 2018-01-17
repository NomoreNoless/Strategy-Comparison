LoadDataSQL <- function(FirmName="",E_ALGORITHM="",E_DEFAULT_URGENCY=Inf, E_MAXPCT_PRDVOL=Inf, E_MINPCT_PRDVOL=Inf, TradeDate_Start="",TradeDate_End="",E_START_TIME="",E_END_TIME="",SEC_COUNTRY="",SEC_TYPE="",VERSION__="",ordertype=""){
  
  matrix <- performance_table %>% 
    select(E_ALGORITHM,
  #1. time-related variables
           E_START_TIME, ADD_TIME_EXCH, E_END_TIME, FIN_TIME, TIME_AT_MARK, TradeDate,
  #2. volume-related variables
           E_QTY_DAY_TARGET, E_QTY_DONE, E_QUANTITY_TARGET, VOL_EMA, VOL1, VOL_PROJ, PART_VOL, PRD_VOL, TGT_PCT_ADV, E_MAXPCT_PRDVOL, E_MINPCT_PRDVOL, AVERAGE_BA_SZ,
  #3. spread-related variables
           AVERAGE_SPREAD, SPR_EMA,
  #4. volatility-related variables
           AVERAGE_MID_RANGE, MID_RANGE_EMA, OPEN, HIGH, LOW, CLOSE,
  #5. price-related variables
           LAST_CHG, MID_EMA, MARK, PREV_WAVE_MID_AT_END, E_LIMIT_PRC, LIMIT_PROBLEM,
  #6. performance-related variables
           AVG_PRC, PRD_VWAP, SLIPPAGE, E_PL,
  #7. urgency-related variables
           E_DEFAULT_URGENCY, E_URGENCY_OVERRIDE,
  #8. other variables
           FirmName, TethysClientID, StrategyID, CompoundID, Ticker,TICK_SIZE_USE, EXEC_CT, PILOT_SECURITY, VERSION__, SEC_TYPE, SEC_COUNTRY)
  
  
  if(nchar(FirmName)>0){
    var <- FirmName
    matrix <- matrix %>% filter(FirmName==var)
  }
  
  if(nchar(E_ALGORITHM)>0){
    var <- E_ALGORITHM
    matrix <- matrix %>% filter(E_ALGORITHM==var)
  }
    
  if(E_MAXPCT_PRDVOL<Inf){
    var <- E_MAXPCT_PRDVOL
    matrix <- matrix %>% filter(E_MAXPCT_PRDVOL==var)
  }
  
  if(E_MINPCT_PRDVOL<Inf){
    var <- E_MINPCT_PRDVOL
    matrix <- matrix %>% filter(E_MINPCT_PRDVOL==var)
  }
  
  if(nchar(TradeDate_Start)>0){
    matrix <- matrix %>% filter(TradeDate>=TradeDate_Start)
  }
  
  if(nchar(TradeDate_End)>0){
    matrix <- matrix %>% filter(TradeDate<=TradeDate_End)
  }
  
  if(nchar(E_START_TIME)>0){
    var <- E_START_TIME
    matrix <- matrix %>% filter(E_START_TIME==var)
  }
  
  if(nchar(E_END_TIME)>0){
    var <- E_END_TIME
    matrix <- matrix %>% filter(E_END_TIME==var)
  }
  
  if(nchar(SEC_COUNTRY)>0){
    var <- SEC_COUNTRY
    matrix <- matrix %>% filter(SEC_COUNTRY==var)
  }
  
  if(nchar(SEC_TYPE)>0){
    var <- SEC_TYPE
    matrix <- matrix %>% filter(SEC_TYPE==var)
  }
  
  if(nchar(VERSION__)>0){
    var <- VERSION__
    matrix <- matrix %>% filter(VERSION__==var)
  }
  
  matrix <- matrix %>% collect()
  
  #deal with NA values, should collect the data first
  if(E_DEFAULT_URGENCY<Inf){

    if(E_DEFAULT_URGENCY==2){
      matrix <- matrix %>% filter(E_DEFAULT_URGENCY==2  | E_URGENCY_OVERRIDE==2 | (is.na(E_DEFAULT_URGENCY) & is.na(E_URGENCY_OVERRIDE)))
    }
    else{
      var <- E_DEFAULT_URGENCY
      matrix <- matrix %>% filter(E_DEFAULT_URGENCY==var | E_URGENCY_OVERRIDE==var)
    }

  }
  
  if(nchar(ordertype)>0){
    if(ordertype=="LO"){
      matrix <- matrix %>% filter(!is.na(E_LIMIT_PRC))
    }
    else{
      matrix <- matrix %>% filter(is.na(E_LIMIT_PRC))
    }
  }

  gc(reset = TRUE)
  
  return(matrix)
  
}
  