source("LoadDataSQL.R")

#comparison=(“account”,”strategy”,”urgency”,”time”,”partrate”,"version") 
loadData1on1 <- function(FirmName="",E_ALGORITHM="",E_DEFAULT_URGENCY=Inf,E_MAXPCT_PRDVOL=Inf,E_MINPCT_PRDVOL=Inf,TradeDate_Start="",TradeDate_End="",E_START_TIME="",E_END_TIME="",SEC_COUNTRY="",SEC_TYPE="",VERSION__="",comparison,ordertype="")
{
  
  if(comparison=="account"){
    dataset1 <- LoadDataSQL(FirmName=FirmName[1],E_ALGORITHM=E_ALGORITHM,E_DEFAULT_URGENCY=E_DEFAULT_URGENCY,E_MAXPCT_PRDVOL=E_MAXPCT_PRDVOL,E_MINPCT_PRDVOL=E_MINPCT_PRDVOL,
                            TradeDate_Start=TradeDate_Start,TradeDate_End=TradeDate_End,E_START_TIME=E_START_TIME,E_END_TIME=E_END_TIME,SEC_COUNTRY=SEC_COUNTRY,SEC_TYPE=SEC_TYPE,VERSION__=VERSION__,ordertype=ordertype)
    dataset2 <- LoadDataSQL(FirmName=FirmName[2],E_ALGORITHM=E_ALGORITHM,E_DEFAULT_URGENCY=E_DEFAULT_URGENCY,E_MAXPCT_PRDVOL=E_MAXPCT_PRDVOL,E_MINPCT_PRDVOL=E_MINPCT_PRDVOL,
                            TradeDate_Start=TradeDate_Start,TradeDate_End=TradeDate_End,E_START_TIME=E_START_TIME,E_END_TIME=E_END_TIME,SEC_COUNTRY=SEC_COUNTRY,SEC_TYPE=SEC_TYPE,VERSION__=VERSION__,ordertype=ordertype)
    
    filename <- paste0(FirmName[1],"_VS_",FirmName[2],"_",E_ALGORITHM,"_",E_DEFAULT_URGENCY)
  }
  
  if(comparison=="strategy"){
    dataset1 <- LoadDataSQL(FirmName=FirmName,E_ALGORITHM=E_ALGORITHM[1],E_DEFAULT_URGENCY=E_DEFAULT_URGENCY,E_URGENCY_OVERRIDE=E_URGENCY_OVERRIDE,E_MAXPCT_PRDVOL=E_MAXPCT_PRDVOL,E_MINPCT_PRDVOL=E_MINPCT_PRDVOL,
                            TradeDate_Start=TradeDate_Start,TradeDate_End=TradeDate_End,E_START_TIME=E_START_TIME,E_END_TIME=E_END_TIME,SEC_COUNTRY=SEC_COUNTRY,SEC_TYPE=SEC_TYPE, VERSION__=VERSION__,ordertype=ordertype)
    dataset2 <- LoadDataSQL(FirmName=FirmName,E_ALGORITHM=E_ALGORITHM[2],E_DEFAULT_URGENCY=E_DEFAULT_URGENCY,E_URGENCY_OVERRIDE=E_URGENCY_OVERRIDE,E_MAXPCT_PRDVOL=E_MAXPCT_PRDVOL,E_MINPCT_PRDVOL=E_MINPCT_PRDVOL,
                            TradeDate_Start=TradeDate_Start,TradeDate_End=TradeDate_End,E_START_TIME=E_START_TIME,E_END_TIME=E_END_TIME,SEC_COUNTRY=SEC_COUNTRY,SEC_TYPE=SEC_TYPE, VERSION__=VERSION__,ordertype=ordertype)
  
    filename <- paste0(E_ALGORITHM[1],"_VS_",E_ALGORITHM[2],"_",FirmName,"_",E_DEFAULT_URGENCY)
  }
  
  if(comparison=="urgency"){
    dataset1 <- LoadDataSQL(FirmName=FirmName,E_ALGORITHM=E_ALGORITHM,E_DEFAULT_URGENCY=E_DEFAULT_URGENCY[1],E_MAXPCT_PRDVOL=E_MAXPCT_PRDVOL,E_MINPCT_PRDVOL=E_MINPCT_PRDVOL,
                            TradeDate_Start=TradeDate_Start,TradeDate_End=TradeDate_End,E_START_TIME=E_START_TIME,E_END_TIME=E_END_TIME,SEC_COUNTRY=SEC_COUNTRY,SEC_TYPE=SEC_TYPE, VERSION__=VERSION__,ordertype=ordertype)
    dataset2 <- LoadDataSQL(FirmName=FirmName,E_ALGORITHM=E_ALGORITHM,E_DEFAULT_URGENCY=E_DEFAULT_URGENCY[2],E_MAXPCT_PRDVOL=E_MAXPCT_PRDVOL,E_MINPCT_PRDVOL=E_MINPCT_PRDVOL,
                            TradeDate_Start=TradeDate_Start,TradeDate_End=TradeDate_End,E_START_TIME=E_START_TIME,E_END_TIME=E_END_TIME,SEC_COUNTRY=SEC_COUNTRY,SEC_TYPE=SEC_TYPE, VERSION__=VERSION__,ordertype=ordertype)
  
    filename <- paste0(E_DEFAULT_URGENCY[1],"_VS_",E_DEFAULT_URGENCY[2],"_",FirmName,"_",E_ALGORITHM)
  }
  if(comparison=="time"){
    dataset1 <- LoadDataSQL(FirmName=FirmName,E_ALGORITHM=E_ALGORITHM,E_DEFAULT_URGENCY=E_DEFAULT_URGENCY,E_URGENCY_OVERRIDE=E_URGENCY_OVERRIDE,E_MAXPCT_PRDVOL=E_MAXPCT_PRDVOL,E_MINPCT_PRDVOL=E_MINPCT_PRDVOL,
                            TradeDate_Start=TradeDate_Start[1],TradeDate_End=TradeDate_End[1],E_START_TIME=E_START_TIME,E_END_TIME=E_END_TIME,SEC_COUNTRY=SEC_COUNTRY,SEC_TYPE=SEC_TYPE, VERSION__=VERSION__, ordertype=ordertype)
    dataset2 <- LoadDataSQL(FirmName=FirmName,E_ALGORITHM=E_ALGORITHM,E_DEFAULT_URGENCY=E_DEFAULT_URGENCY,E_URGENCY_OVERRIDE=E_URGENCY_OVERRIDE,E_MAXPCT_PRDVOL=E_MAXPCT_PRDVOL,E_MINPCT_PRDVOL=E_MINPCT_PRDVOL,
                            TradeDate_Start=TradeDate_Start[2],TradeDate_End=TradeDate_End[2],E_START_TIME=E_START_TIME,E_END_TIME=E_END_TIME,SEC_COUNTRY=SEC_COUNTRY,SEC_TYPE=SEC_TYPE, VERSION__=VERSION__, ordertype=ordertype)
  
    filename <- paste0(TradeDate_Start[1],"-",TradeDate_End[1],"_VS_",
                       TradeDate_Start[2],"-",TradeDate_End[2],"_",
                       FirmName,"_",E_ALGORITHM,"_",E_DEFAULT_URGENCY)
  }
  if(comparison=="partrate"){
    dataset1 <- LoadDataSQL(FirmName=FirmName,E_ALGORITHM=E_ALGORITHM,E_DEFAULT_URGENCY=E_DEFAULT_URGENCY,E_URGENCY_OVERRIDE=E_URGENCY_OVERRIDE,E_MAXPCT_PRDVOL=E_MAXPCT_PRDVOL[1],E_MINPCT_PRDVOL=E_MINPCT_PRDVOL,
                            TradeDate_Start=TradeDate_Start,TradeDate_End=TradeDate_End,E_START_TIME=E_START_TIME,E_END_TIME=E_END_TIME,SEC_COUNTRY=SEC_COUNTRY,SEC_TYPE=SEC_TYPE, VERSION__=VERSION__, ordertype=ordertype)
    dataset2 <- LoadDataSQL(FirmName=FirmName,E_ALGORITHM=E_ALGORITHM,E_DEFAULT_URGENCY=E_DEFAULT_URGENCY,E_URGENCY_OVERRIDE=E_URGENCY_OVERRIDE,E_MAXPCT_PRDVOL=E_MAXPCT_PRDVOL[2],E_MINPCT_PRDVOL=E_MINPCT_PRDVOL,
                            TradeDate_Start=TradeDate_Start,TradeDate_End=TradeDate_End,E_START_TIME=E_START_TIME,E_END_TIME=E_END_TIME,SEC_COUNTRY=SEC_COUNTRY,SEC_TYPE=SEC_TYPE, VERSION__=VERSION__, ordertype=ordertype)
  
    filename <- paste0(E_MAXPCT_PRDVOL[1],"_VS_",E_MAXPCT_PRDVOL[2],"_",FirmName,"_",E_ALGORITHM,"_",E_DEFAULT_URGENCY)
  }
  if(comparison=="version"){
    
    dataset1 <- LoadDataSQL(FirmName=FirmName,E_ALGORITHM=E_ALGORITHM,E_DEFAULT_URGENCY=E_DEFAULT_URGENCY,E_URGENCY_OVERRIDE=E_URGENCY_OVERRIDE,E_MAXPCT_PRDVOL=E_MAXPCT_PRDVOL,E_MINPCT_PRDVOL=E_MINPCT_PRDVOL,
                            TradeDate_Start=TradeDate_Start,TradeDate_End=TradeDate_End,E_START_TIME=E_START_TIME,E_END_TIME=E_END_TIME,SEC_COUNTRY=SEC_COUNTRY,SEC_TYPE=SEC_TYPE, VERSION__=VERSION__[1:2], ordertype=ordertype)
    dataset2 <- LoadDataSQL(FirmName=FirmName,E_ALGORITHM=E_ALGORITHM,E_DEFAULT_URGENCY=E_DEFAULT_URGENCY,E_URGENCY_OVERRIDE=E_URGENCY_OVERRIDE,E_MAXPCT_PRDVOL=E_MAXPCT_PRDVOL,E_MINPCT_PRDVOL=E_MINPCT_PRDVOL,
                            TradeDate_Start=TradeDate_Start,TradeDate_End=TradeDate_End,E_START_TIME=E_START_TIME,E_END_TIME=E_END_TIME,SEC_COUNTRY=SEC_COUNTRY,SEC_TYPE=SEC_TYPE, VERSION__=VERSION__, ordertype=ordertype)
    
    if(ordertype==1){
      filename <- paste0(FirmName,"_",E_ALGORITHM,"_","END_TIME=",E_END_TIME,"_",
                         VERSION__[1],"_and_",VERSION__[2],"_VS_OTHERS_X_",VERSION__[3],"_MO")
    }
    
  }
  if(comparison=="performance"){
    
    dataset1 <- LoadDataSQL(FirmName=FirmName,E_ALGORITHM=E_ALGORITHM,E_DEFAULT_URGENCY=E_DEFAULT_URGENCY,E_URGENCY_OVERRIDE=E_URGENCY_OVERRIDE,E_MAXPCT_PRDVOL=E_MAXPCT_PRDVOL,E_MINPCT_PRDVOL=E_MINPCT_PRDVOL,
                            TradeDate_Start=TradeDate_Start,TradeDate_End=TradeDate_End,E_START_TIME=E_START_TIME,E_END_TIME=E_END_TIME,SEC_COUNTRY=SEC_COUNTRY,SEC_TYPE=SEC_TYPE, VERSION__=VERSION__, ordertype=ordertype)
    dataset2 <- LoadDataSQL(FirmName=FirmName,E_ALGORITHM=E_ALGORITHM,E_DEFAULT_URGENCY=E_DEFAULT_URGENCY,E_URGENCY_OVERRIDE=E_URGENCY_OVERRIDE,E_MAXPCT_PRDVOL=E_MAXPCT_PRDVOL,E_MINPCT_PRDVOL=E_MINPCT_PRDVOL,
                            TradeDate_Start=TradeDate_Start,TradeDate_End=TradeDate_End,E_START_TIME=E_START_TIME,E_END_TIME=E_END_TIME,SEC_COUNTRY=SEC_COUNTRY,SEC_TYPE=SEC_TYPE, VERSION__=VERSION__, ordertype=ordertype)
    
    if(ordertype==1 & length(VERSION__)==2){
      filename <- paste0(FirmName,"_",E_ALGORITHM,"_","END_TIME=",E_END_TIME, "_with_auction_","MO")
    }
    if(ordertype==1 & length(VERSION__)==3){
      filename <- paste0(FirmName,"_",E_ALGORITHM,"_","END_TIME=",E_END_TIME, "_without_auction_","MO")
    }
    
  }
  
  
  #number to identify the matrix
  dataset1$id <- 1
  dataset2$id <- 2
  
  dataset <- rbind(dataset1,dataset2)
  
  
  return(list(dataset=dataset,filename=filename))
}



#*********************E_DEFAULT_URGENCY****************************************
#"NULL", "1","2","3","4","5"
loadData1vsB <- function(FirmName="",E_ALGORITHM="",E_DEFAULT_URGENCY=Inf,E_MAXPCT_PRDVOL=Inf,E_MINPCT_PRDVOL=Inf,TradeDate_Start="",TradeDate_End="",E_START_TIME="",E_END_TIME="",SEC_COUNTRY="",SEC_TYPE="",VERSION__="",ordertype="")
{
  dataset <- LoadDataSQL(FirmName=FirmName,E_ALGORITHM=E_ALGORITHM,E_DEFAULT_URGENCY=E_DEFAULT_URGENCY,E_MAXPCT_PRDVOL=E_MAXPCT_PRDVOL,E_MINPCT_PRDVOL=E_MINPCT_PRDVOL,
                          TradeDate_Start=TradeDate_Start,TradeDate_End=TradeDate_End,E_START_TIME=E_START_TIME,E_END_TIME=E_END_TIME,SEC_COUNTRY=SEC_COUNTRY,SEC_TYPE=SEC_TYPE,VERSION__=VERSION__,ordertype=ordertype)

  filename <- ""
  
  if(nchar(FirmName)!=0){
    filename <- paste0(filename,FirmName,"_")
  }
  if(nchar(E_ALGORITHM)!=0){
    filename <- paste0(filename,E_ALGORITHM,"_")
  }
  
  if(nchar(E_START_TIME)!=0){
    filename <- paste0(filename,E_START_TIME,"_")
  }
  if(nchar(E_END_TIME)!=0){
    filename <- paste0(filename,E_END_TIME,"_")
  }
  if(nchar(VERSION__)>0){
    filename <- paste0(filename,"_",VERSION__)
  }
  if(nchar(SEC_TYPE)>0){
    filename <- paste0(filename, "_", SEC_TYPE,"_")
  }
  
  filename <- paste0(filename,"1vsB_")
  
  if(nchar(ordertype)>0 & ordertype=="LO"){
    filename <- paste0(filename,ordertype)
  }
  if(nchar(ordertype)>0 & ordertype=="MO"){
    filename <- paste0(filename,ordertype)
  }
  
  return(list(dataset=dataset,filename=filename))
}
  