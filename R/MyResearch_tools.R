require(timeSeries)
require(xts)

#Get a timeseries of stock return for specified stock from stock peroidic return dataset
get_stock_monthly_return <- function(ds_trd_mnth.df, stock_cd, tseries_type = c("timeSeries", "xts")) {
  
  # Validate params
  if (is.null(ds_trd_mnth.df) || missing(stock_cd))
    stop("ds_trd_mnth.df, stock_cd mustn't be null")
  
  if (!is.data.frame(ds_trd_mnth.df)) {
    stop("ds_source.df should be a dataframe")
  }
  
  tseries_type <- match.arg(tseries_type)
  if (is.null(tseries_type)) {
    warning("teries_type should be timeSeries or xts, set as timeSeries by default")
    tseries_type <- "timeSeries"
  }
  
  # Get related data of the specified stock from all stock trd_mnth data table by filtering by stock_cd
  ds_stock_trd_mnth <- na.omit(ds_trd_mnth.df[ds_trd_mnth.df$stkcd == as.numeric(stock_cd),])
  
  # Create the stock return timeseries for specified stock
  stkcd_string = sprintf("%06d", stock_cd)
  if (tseries_type == "timeSeries") {
    # timeSeries data series
    ds_name <- paste("ds_", stkcd_string, ".fts", sep = "")
    result_ts <- timeSeries(ds_stock_trd_mnth['mretwd'], as.Date(as.yearmon(ds_stock_trd_mnth$trdmnt)))
    colnames(result_ts) <- stkcd_string
    
  } else {
    # xts data series
    ds_name <- paste("ds_", stkcd_string, ".xts", sep = "")  
    result_ts <- xts(ds_stock_trd_mnth['mretwd'], order.by = as.Date(as.yearmon(ds_stock_trd_mnth$trdmnt)))
    colnames(result_ts) <- stkcd_string
  }
  
  # Return the result timeseries of stock 
  assign(ds_name, result_ts, pos = .GlobalEnv)
  return(get(ds_name))
}



# Get a timeseries of stock data for specified stock from peroidic datasets
get_stock_data <- function(ds_source.df, stock_cd, target_field, stkcd_field = "stkcd", date_field="trdmnt", tseries_type = c("timeSeries", "xts")) {
  
  # Validate parmas--------
  if (is.null(ds_source.df) || missing(stock_cd) || missing(target_field) || missing(date_field)) {
    stop("ds_source.df, stock_cd, target_field, date_field  mustn't be null")
  }
  
  # Check whether the datafields existed 
  field_list <- c(stkcd_field, target_field, date_field)
  if (!all(field_list %in% names(ds_source.df))) {
    error_fields <- NULL
    for (field_name in field_list) {
     if (!field_name %in% names(ds_source.df)) {
        error_fields <- ifelse(is.null(error_fields), field_name, paste(error_fields, field_name, sep = ","))
     }
    }
    
    error_msg <- sprintf("%s dosen't exist in dataset ", error_fields)
    stop(error_msg)
  }
  
  tseries_type <- match.arg(tseries_type)
  if (is.null(tseries_type)) {
    warning("teries_type should be timeSeries or xts, set as timeSeries by default")
    tseries_type <- "timeSeries"
  }
  
  # Get related data of the specified stock from all stock trd_mnth data table
  ds_stock_data.df <- na.omit(ds_source.df[ds_source.df$stkcd == as.numeric(stock_cd),])
  
  # Build result dataset for specified stock 
  stkcd_string = sprintf("%06d", stock_cd)
  if (tseries_type == "timeSeries") {
    # timeSeries data series
    ds_name <- sprintf("ds_%s_%s.fts", stkcd_string, target_field)
    result_ts <- timeSeries(ds_stock_data.df[target_field], as.Date(as.yearmon(ds_stock_data.df[[date_field]])))
    colnames(result_ts) <- stkcd_string
    
  } else {
    # xts data series
    ds_name <- sprintf("ds_%s_%s.xts", stkcd_string, target_field)
    result_ts <- xts(ds_stock_data.df['target_field'], order.by = as.Date(as.yearmon(ds_stock_data.df[[date_field]])))
    colnames(result_ts) <- stkcd_string
  }
  
  # Return the result timeseries of stock 
  assign(ds_name, result_ts, pos = .GlobalEnv)
  return(get(ds_name))
  
}

# Get several timeseries of stocks data for multiple stocks from peroidic dataset
#fetch_stocks_data <- function(ds_source.df, stock_cd_list, target_field, stkcd_field = "stkcd", date_field="trdmnt", tseries_type = c("timeSeries", "xts")) {
fetch_stocks_data <- function(ds_source.df, stock_cd_list, ...) {
  
  
  # Validate parmas--------
  # if (is.null(ds_source.df) || missing(stock_cd_list) || missing(target_field) || missing(date_field)) {
  if (is.null(ds_source.df) || missing(stock_cd_list) ) {
    stop("ds_source.df, stock_cd_list mustn't be null")
  }
  
  ds_result = NULL
  for (the_stock_cd in stock_cd_list) {
    
    # get stock data for specified stock
    # ds_stock_data <- get_stock_data(ds_source.df = ds_source.df, stock_cd = the_stock_cd, 
    #                                 target_field = target_field, stkcd_field = stkcd_field,
    #                                 date_field = date_field, tseries_type = tseries_type)
    ds_stock_data <- get_stock_data(ds_source.df = ds_source.df, stock_cd = the_stock_cd, ...)
     
    # Build the result dataset
    if (is.null(ds_result)) {
      # Set stock data as result dataset
      ds_result <- ds_stock_data
      } else {
      # Merge stock data into the result dataset
      ds_result <- merge(ds_result, ds_stock_data)   
    }
    
  }
  
  # Modifed the names of dataset by removing the prefix of "X" 
  #names(ds_result) <- gsub(pattern = "X",replacement = "", names(ds_result))
  
  return(ds_result)
}
