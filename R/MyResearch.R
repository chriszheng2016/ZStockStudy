source("R/MyResearch_tools.R")

#=====load get gta from datasource ====================
library(RODBC)
con_gta <- odbcConnect(dsn = "GTA_SQLData")
sqlTables(con_gta, tableType = "TABLE")
ds_trd_company.df <- sqlFetch(con_gta, "TRD_Co_公司基本情况")
colnames(ds_trd_company.df) <- tolower(colnames(ds_trd_company.df))

ds_trd_mnth.df <- sqlFetch(con_gta, "TRD_Mnth_月个股回报率")
colnames(ds_trd_mnth.df) <- tolower(colnames(ds_trd_mnth.df))


#===== get individual stock monthly return ===================
library(timeSeries)
library(xts)

#---- 宇通客车--------
#---宇通客车(600066)---

#提取个股数据
ds_600066.df <- na.omit(ds_trd_mnth.df[ds_trd_mnth.df$stkcd == 600066,])
dim(ds_600066.df)
head(ds_600066.df)

#收益数据
#a. df --> timeSeries --> xts########
#timeSereis time Sereis
ds_600066_mretwd.fts <- timeSeries(ds_600066.df['mretwd'], as.Date(as.yearmon(ds_600066.df$trdmnt)), units = "600066")
head(ds_600066_mretwd.fts)

#xts time Series
ds_600066_mretwd.xts <- as.xts(ds_600066_mretwd.fts,.RECLASS = T)
class(Reclass(ds_600066_mretwd.xts))
head(ds_600066_mretwd.xts)

#b. df --> xts --> timeSeries########
#xts time Series
ds_600066_mretwd.xts <- as.xts(ds_600066.df['mretwd'], as.yearmon(ds_600066.df$trdmnt))
head(ds_600066_mretwd.xts)

#timeSereis time Sereis
ds_600066_mretwd.fts <- as.timeSeries(convertIndex(ds_600066_mretwd.xts, "timeDate"), units = "600066")
head(ds_600066_mretwd.fts)


# Test tools functions of MyResearch -----
source('R/MyResearch_tools.R')

# Test get_stock_monthly_return
ds_stock_monthly_return.fts <- get_stock_monthly_return(ds_trd_mnth.df, stock_cd = 600066, tseries_type = "timeSeries")

# Test get_stock_data
ds_stock_mretnd.fts <- get_stock_data(ds_source.df = ds_trd_mnth.df, stock_cd = 600066, target_field = "mretnd", date_field = "trdmnt")

# Test fetch_stocks_data
stock_cd_list <- c(600066,000550, 600031, 000157,000651, 000333)
ds_stocks_mretnd.fts <- fetch_stocks_data(ds_source.df = ds_trd_mnth.df, stock_cd_list = stock_cd_list, target_field = "mretnd", date_field = "trdmnt")




