options(java.parameters = "-Xmx8000m")
library(XLConnect)
library(tidyverse)
library(reshape)
library(gridExtra)
library(SDMTools)
library(dplyr)
library(odbc)
library(stringr)
library(rlang)
library(stats)
library(formattable)


source("LoadDataSQL.R")
source("loadData1on1.R")
source("LoadVariables.R")
source("LoadFactors.R")
source("LoadFactors2.R")
source("LoadBins.R")
source("BinbyFactor.R")
source("LoadBenchmark.R")
source("Cumulative.R")
source("EvaluatePerformance_1vs1.R")
source("EvaluatePerformance_1vsB.R")
source("Cumulative_1vs1.R")
source("filter_data.R")
source("EvaluatePerformance_1vsB_cancelcost.R")
source("load_msgs.R")
source("Limit_condition_filter.R")
source("Load_compound_variables.R")
source("MannWhitney_test.R")
source("EvaluatePerformance_reverse.R")

#__________________________________________________________________________
con1 <- DBI::dbConnect(odbc::odbc(),
                       Driver = "{SQL Server Native Client 11.0}",
                       #Driver = "{SQL Server}",
                       Server = "192.168.1.110",
                       Database = "TethysAnalytics",
                       UID = "tethys",
                       PWD = "tethys100",
                       MARS_connection="yes")

performance_table <- tbl(con1,"PerformanceExports")

#----------------------------------------------------------------------------
con2 <- DBI::dbConnect(odbc::odbc(),
                       Driver = "{SQL Server Native Client 11.0}",
                       #Driver = "{SQL Server}",
                       Server = "192.168.1.110",
                       Database = "tethystrader_R",
                       UID = "tethys",
                       PWD = "tethys100",
                       MARS_connection="yes")

order_msgs <- tbl(con2,"order_msgs")
#____________________________________________________________________________


#1VS1
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

dataset_filename <- loadData1on1(FirmName="Fidessa", E_ALGORITHM="EVWAP", E_DEFAULT_URGENCY = c(2,3), TradeDate_Start = "20170101", comparison="urgency", ordertype="")
dataset_filename <- LoadDataSQL(FirmName = "Fidessa", E_ALGORITHM = "EVWAP", E_DEFAULT_URGENCY = 2, TradeDate_Start = "20170101", ordertype = "LO")

filename <- dataset_filename$filename
dataset <- dataset_filename$dataset

dataset <- dataset %>% Limit_condition_filter()

dataset <- LoadVariables(dataset)

#selected_list <- LoadFactors(Factors=c("duration","Vduration","Eduration","adv","TopBook_to_OrderSize","Volume_to_TopBook","size_pct_vol","realpart","spread_bps","mid_range_bps","return"),PerformanceMetric = "VWAP_Slip_bps",dataset)
selected_list <- LoadFactors(Factors=c("start_time","end_time","fill_ratio","new_metric","duration_sec","duration_exe","duration_vol","adv","size_pct","real_part","book_size","vol_book","spread_bps","spread_tic","midrange_pct","return_pct","notional","PILOT_SECURITY",
                                       "spread","midrange","midrange_spread","mark","book","exec_ct","TICK_SIZE_USE"),PerformanceMetric = "cancel_cost_spread",dataset,benchmark=Inf)


#when performance metric is fill_ratio, have different number of factors
selected_list <- LoadFactors(Factors=c("start_time","end_time","cancel_cost_bps","cancel_cost_spread","new_metric","duration_sec","duration_exe","duration_vol","adv","size_pct","real_part","book_size","vol_book","spread_bps","spread_tic","midrange_pct","return_pct","notional","PILOT_SECURITY",
                                       "spread","midrange","midrange_spread","mark","book","exec_ct","TICK_SIZE_USE"),PerformanceMetric = "fill_ratio",dataset,benchmark=Inf)


selected_list <- LoadFactors(Factors=c("start_time","end_time","cancel_cost_bps","cancel_cost_spread","fill_ratio","duration_sec","duration_exe","duration_vol","adv","size_pct","real_part","book_size","vol_book","spread_bps","spread_tic","midrange_pct","return_pct","notional","PILOT_SECURITY",
                                       "spread","midrange","midrange_spread","mark","book","exec_ct","TICK_SIZE_USE"),PerformanceMetric = "new_metric",dataset,benchmark=Inf)


# selected_list <- LoadFactors2(Factors=c("start_time","end_time","fill_ratio","ratio_bps","ratio_spread","duration_sec","duration_exe","duration_vol","adv","size_pct","real_part","book_size","vol_book","spread_bps","spread_tic","midrange_pct","return_pct","notional","PILOT_SECURITY",
#                                        "spread","midrange","midrange_spread","mark","book","exec_ct","TICK_SIZE_USE"),PerformanceMetric = "VWAP_Slip_bps",dataset,benchmark=Inf)



selected_list <- LoadFactors(Factors=c("duration_sec","duration_exe","duration_vol","adv","size_pct","real_part","book_size","vol_book","spread_bps","spread_tic","midrange_pct","return_pct","notional","PILOT_SECURITY",
                                       "spread","midrange","midrange_spread","mark","book","exec_ct","TICK_SIZE_USE"),PerformanceMetric = "VWAP_Slip_bps",dataset,benchmark=Inf)


#all data 
temp_matrix <- selected_list$selected

#filter out the data
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#filtered_data <- filter_data(temp_matrix,"return_pct",c(-Inf,-1),filename)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


filtered_data <- temp_matrix %>% filter(book_size>0.4)

filtered_data <- temp_matrix %>% filter(spread_bps>5.5)

filtered_data <- temp_matrix %>% filter(midrange_spread<=0.45)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
filtered_data <- temp_matrix %>% filter(book_size>0.4,vol_book <= 350)
filtered_data <- temp_matrix %>% filter(book_size>0.4,spread_tic<=1.1)
filtered_data <- temp_matrix %>% filter(book_size>0.4,midrange_pct <=0.02)
filtered_data <- temp_matrix %>% filter(book_size>0.4,adv>1300000)

filtered_data <- temp_matrix %>% filter(spread_bps>5.5,size_pct>0.012)
filtered_data <- temp_matrix %>% filter(spread_bps>5.5,real_part>0.012)
filtered_data <- temp_matrix %>% filter(spread_bps>5.5,book_size<=0.4)
filtered_data <- temp_matrix %>% filter(spread_bps>5.5,midrange_pct>0.02)



filtered_data <- temp_matrix %>% filter(spread_bps<=3, adv>5500000)
filtered_data <- temp_matrix %>% filter(spread_bps<=3, vol_book<=200)
filtered_data <- temp_matrix %>% filter(spread_bps<=3, midrange_spread>1)
filtered_data <- temp_matrix %>% filter(spread_bps<=3, return_pct > -0.2 , return_pct<=0.2)




#filtered_data <- temp_matrix %>% filter(vol_book<=200,return_pct>0)
#filtered_data <- temp_matrix %>% filter(vol_book<=200,midrange_pct<=0.03)
#filtered_data <- temp_matrix %>% filter(size_pct>0.02,real_part>0.02,duration_sec>21300)
#filtered_data <- temp_matrix %>% filter(return_pct<=-1,duration_sec>21060)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

default_bins <- LoadBins(algo="EVWAP",ordertype=1)

#if want to use neat pre-determined bins, put percentage = -1
binned_dataset <- BinbyFactor(temp_matrix,selected_list$Factors,percentage=0.1,default_bins)

EvaluatePerformance_1vs1(binned_dataset,selected_list$Factors,selected_list$PerformanceMetric,method="nonparametric",percentage=0.1,algo="EVWAP",filename=filename,default_bins)







#1_VS_B
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#ordertype=1-MO ordertype=2-LO ordertype=1.5-marketable limit order
#***********************parameter**********************************
#E_DEFAULT_URGENCY: "NULL","0","1","2","3","4","5"
#ordertype: market_order=1, limit_order=2


#load data
dataset_filename <- loadData1vsB(FirmName = "QIM", SEC_TYPE = "S",E_ALGORITHM = "EVWAP")
dataset <- dataset_filename$dataset
filename <- dataset_filename$filename


#only have order msgs after 2017-05-22, filter out order msgs before that time
dataset <- dataset %>% filter(TradeDate>='2017-05-22')


firmname <- "QIM"
time_range <- c("201705","201706","201707","201708","201709","201710","201711","201712","201801")

#Load_compound_variables(): combine with the order_msgs table
#LoadVariables(): use only performance table
#________________________________________________________________________________________________________________________________________
dataset <- Load_compound_variables(dataset,"QIM",time_range)

dataset <- LoadVariables(dataset)
#________________________________________________________________________________________________________________________________________



#selected_list <- LoadFactors(Factors = c("duration","Vduration","Eduration","adv","TopBook_to_OrderSize","Volume_to_TopBook","size_pct_vol","realpart","spread_bps","mid_range_bps","return_pct","PILOT_SECURITY"),PerformanceMetric = "VWAP_Slip_bps",dataset,benchmark=0)

#Performance metric: cancel_rate
selected_list <- LoadFactors(Factors=c("VWAP_Slip_bps","cancel_cost_bps","fill_ratio","effective_spread_tics","effective_spread_bps","duration_sec","duration_exe","duration_vol","adv","size_pct","real_part","book_size","vol_book","spread_bps","spread_tic","midrange_pct","return_pct","notional",
                                       "spread","midrange","midrange_spread","mark","book","exec_ct"),PerformanceMetric = "cancel_rate",dataset,0)

#___________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________

selected_list <- LoadFactors(Factors=c("start_time","end_time","fill_ratio","new_metric","duration_sec","duration_exe","duration_vol","adv","size_pct","real_part","book_size","vol_book","spread_bps","spread_tic","midrange_pct","return_pct","notional","PILOT_SECURITY",
                                       "spread","midrange","midrange_spread","mark","book","exec_ct","TICK_SIZE_USE"),PerformanceMetric = "cancel_cost_spread",dataset,0)

selected_list <- LoadFactors(Factors=c("start_time","end_time","cancel_cost_bps","cancel_cost_spread","new_metric","duration_sec","duration_exe","duration_vol","adv","size_pct","real_part","book_size","vol_book","spread_bps","spread_tic","midrange_pct","return_pct","notional","PILOT_SECURITY",
                                       "spread","midrange","midrange_spread","mark","book","exec_ct","TICK_SIZE_USE"),PerformanceMetric = "fill_ratio",dataset,0)

selected_list <- LoadFactors(Factors=c("start_time","end_time","VWAP_Slip_bps","Alpha","Alpha2","fill_ratio","overall_fill_ratio","cancel_cost_bps","cancel_cost_spread","duration_sec","duration_exe","duration_vol","adv","size_pct","real_part","book_size","vol_book","spread_bps","spread_tic","midrange_pct","return_pct","notional","PILOT_SECURITY",
                                       "spread","midrange","midrange_spread","mark","book","exec_ct","TICK_SIZE_USE"),PerformanceMetric = "new_metric1_bps",dataset,0)

selected_list <- LoadFactors(Factors=c("start_time","cancel_cost_bps","cancel_cost_spread","fill_ratio","new_metric1_bps","new_metric2_bps","duration_sec","duration_exe","duration_vol","adv","size_pct","real_part","book_size","vol_book","spread_bps","spread_tic","midrange_pct","return_pct","notional","PILOT_SECURITY",
                                       "spread","midrange","midrange_spread","mark","book","exec_ct","TICK_SIZE_USE"),PerformanceMetric = "Alpha",dataset,0)


selected_list <- LoadFactors(Factors=c("start_time","end_time","duration_sec","duration_exe","duration_vol","adv","size_pct","real_part","book_size","vol_book","spread_bps","spread_tic","midrange_pct","return_pct","notional","PILOT_SECURITY",
                                        "spread","midrange","midrange_spread","mark","book","exec_ct","TICK_SIZE_USE"),PerformanceMetric = "Arrival_Slip_bps",dataset,benchmark=0)

#all data 
temp_matrix <- selected_list$selected


#Filter process
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#filter out 1% 99%
#-----------------------------------------------------------------------------------
temp_matrix <- selected_list$selected %>% filter(!is.na(Notional))
num_obs <- dim(temp_matrix)[1]

temp_matrix <- temp_matrix %>% mutate(RowNum=row_number(Notional)) %>%
  filter(RowNum>=num_obs*0.01,RowNum<=num_obs*0.99)

#------------------------------------------------------------------------------------

#filter out 5% 95%
#-------------------------------------------------------------------------------------
temp_matrix <- selected_list$selected %>% filter(!is.na(Notional))
num_obs <- dim(temp_matrix)[1]

temp_matrix <- temp_matrix %>% mutate(RowNum=row_number(Notional)) %>%
  filter(RowNum>=num_obs*0.05,RowNum<=num_obs*0.95)

#------------------------------------------------------------------------------------


#filter out 10% 90%
#-------------------------------------------------------------------------------------
temp_matrix <- selected_list$selected %>% filter(!is.na(Notional))
num_obs <- dim(temp_matrix)[1]

temp_matrix <- temp_matrix %>% mutate(RowNum=row_number(Notional)) %>%
  filter(RowNum>=num_obs*0.1,RowNum<=num_obs*0.9)

#---------------------------------------------------------------------------------------
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

filter_matrix <- temp_matrix %>% filter(real_part>0.015,size_pct>0.02)
filter_matrix <- temp_matrix %>% filter(size_pct>0.012)

#-----------------------------------------------------------------------------------------------------------------------------

default_bins <- LoadBins(algo="EVWAP",ordertype=1)

binned_dataset <- BinbyFactor(temp_matrix,c(selected_list$Factors,selected_list$PerformanceMetric),percentage=0.1,default_bins)

#benchmark <- LoadBenchmark("EVWAP",PerformanceMetric = "VWAP_Slip_bps")

EvaluatePerformance_1vsB(binned_dataset,selected_list$Factors,selected_list$PerformanceMetric,benchmark=0.5,method="weighted",percentage=0.1,filename=filename,default_bins)

#Put Performance Metric on the X-axis
EvaluatePerformance_reverse(binned_dataset,selected_list$Factors,selected_list$PerformanceMetric,method="weighted",percentage=0.1,filename=filename,default_bins)




