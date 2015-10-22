#' wrapper code which in turn calls other MAP functions and load the necessary packages
#' @description wrapper code that calls otehr MAP functions - to provide you a one line code execution for MAP
#' @return  a series of data frames, lists and visisual output
#' @param test_type_id platform id: 1 for CMSR, 2 for CRM, 3 for PPP, 4 for maximizer etc
#' @param test_id the test_id for your test
#' @param control_id the test_sub_id against which all other test_sub_id's will be compared
#' @param sql_connection the ODBC connection name (windows authenticated)
#' @param gm_low  Cutoff point for the negative GM orders - Most use cases with fall within 0.0001 and 0.002
#' @param gm_high  Cutoff point for high GM values per ordering visitors - Most use cases with fall within 0.95 and 1.0
#' @param bookings_high  Cutoff point for high booking values per ordering visitors  - Most use cases with fall within 0.95 and 1.0
#' @param rerun_sql_sproc 1 reruns order, credit and contact sproc:: 0 don't run the sprocs - useful if you have already run these sprocs
#' @param no_days_to_order Orders placed within this time frame from first touch point,is pulled in the order module
#' @param filter_out_reorders Do you want to filter out reorders from CARE credit rate analysis (1 means filter out, 0 means include reorder in Credit rate analysis)
#' @param contact_days_threshold How many days from the first visitor touch point, do you want to consider calls to CARE for contact rate analysis
#' @param cohort_dimensions dimensions for which you want to do cohort analysis
#' @param days_to_consider No of days given for the visitor to convert from the first touch point
#' @param remove_hoppers  0 means remove hopper, 1 means keep visitor level hopper in analysis
#' @param exposed_to_test  only consider visitor who were exposed to test in analysis (0 can be used for CRM test, everyone else should use 1)
#' @param x_axis_day_interval the date interval to be used for any plots by day
#' @param additional_constraint additional where clause used to filter your data
#' @param TCR  Tail criteria for CR metric (do you want to perform 1 or 2 tail analysis on each of the 5 metrics. 0 = 2tail, -1 = negative 1 tail, 1 = postive one tail analysis)
#' @param TBPB Tail criteria for Bookings per buyer metric
#' @param TGPB Tail level for gm per buyer metric
#' @param TBPV Tail level for bookings per visitor metric
#' @param TGPV Tail level for gm per visitor metric
#' @param demo_mode 1 means you are running MAP for demo. It prints all the graphs, but doesn't store the summary data in common directory
#' @examples
#' map_wrapper(test_id = 6352, test_type_id = 1, control_id = 25801)
#' map_wrapper(test_id = 1, test_type_id = 2, control_id = 0, rerun_sql_sproc = 0)
#' map_wrapper(test_id = 6352,control_id = 25801 , test_type_id = 1, rerun_sql_sproc = 0,sql_connection = "RSQL")
#' map_wrapper(test_id = 1, test_type_id = 2, control_id = 0, rerun_sql_sproc = 0,sql_connection = "RSQL",gm_low = 0.0001,gm_high = 0.99,bookings_high = 0.99,cohort_dimensions = c("is_new","region"))
#' @export
map_wrapper <- function(test_id
                        ,test_type_id
                        ,control_id
                        ,sql_connection = "RSQL"
                        ,gm_low = NA
                        ,gm_high = NA
                        ,bookings_high = NA
                        ,rerun_sql_sproc = 1
                        ,no_days_to_order = 7
                        ,filter_out_reorders = 0
                        ,contact_days_threshold = 7
                        ,cohort_dimensions = c("is_new","region","initial_device_type","channel_group")
                        ,days_to_consider = 7
                        ,remove_hoppers = 0
                        ,exposed_to_test = 1
                        ,x_axis_day_interval = '1 days'
                        ,additional_constraint = ''
                        ,TCR = 0
                        ,TBPB = 0
                        ,TGPB = 0
                        ,TBPV = 0
                        ,TGPV = 0
                        ,demo_mode = 0)
{

  time_start = Sys.time()

  require(RODBC)
  require(dplyr)
  require(ggplot2)
  require(scales)
  require(grid)
  require(gridExtra)
  require(proto)
  require(Rcpp)

  msr <- odbcConnect(sql_connection)



  ## based on above information, populate the table and file names in sql
  visitor_table_name <- paste('scratch..msr_vl_',test_type_id,'_',test_id , sep="")
  order_table_name <- paste('scratch..msr_ol_',test_type_id,'_',test_id , sep="")
  credit_table_name <- paste('scratch..msr_ol_',test_type_id,'_',test_id ,'_credit_level', sep="")
  contact_table_name <- paste('scratch..msr_vl_',test_type_id,'_',test_id ,'_contact_level', sep="")
  data_file_name <-   paste('MAP_1_',test_type_id,'_',test_id,'.Rdata',sep="")

  # Execute sql sproc

  ## Order MOdule SPROC
  if(rerun_sql_sproc == 1){
    sqlQuery(msr,paste("exec scratch..msr_module_order ",test_type_id,",",test_id,",",no_days_to_order,sep=""))

    print(paste("completed - Order module ",Sys.time()))

    }

  time_1 = Sys.time()

  # Pull the visitor and order level table (two different ways to pull this information)
  VL <<- msr_import_large_tables(msr, visitor_table_name, 'visitor_id', 4, additional_constraint)
  OL <<- sqlQuery(msr,paste("select * from ", order_table_name,sep =""))

  print(paste("completed - pulling tables into R ",Sys.time()))

  time_2 = Sys.time()
  # data distribution
  msr_data_distributions(msr,visitor_table_name, test_type_id, x_axis_day_interval)

  print(paste("completed - Data distribution graphs ",Sys.time()))

  time_3 = Sys.time()
  # Outlier module
  ## outliers module - data

  map_gm_outlier_data <<- msr_outlier_gm_data (order_level_table = OL
                                               , visitor_level_table = VL
                                               , days_to_consider = days_to_consider
                                               , hopper = remove_hoppers
                                               , exposedToTest = exposed_to_test
                                               , control_id = control_id)

  map_bkg_outlier_data <<- msr_outlier_booking_data  (order_level_table = OL
                                                      , visitor_level_table = VL
                                                      , days_to_consider = days_to_consider
                                                      , hopper = remove_hoppers
                                                      , exposedToTest = exposed_to_test
                                                      , control_id = control_id)
  ### graphs
  msr_outlier_graphs(map_bkg_outlier_data[[1]])
  msr_outlier_graphs(map_gm_outlier_data[[1]],map_gm_outlier_data[[2]])


  print(paste("completed - Outlier Module ",Sys.time()))

  time_4 = Sys.time()

  if(is.na(gm_low)|is.na(gm_high)|is.na(bookings_high)){
    gm_low  <- as.numeric(readline(prompt="Enter the lower cut off point for GM outlier (mostly between 0.0001 and 0.002) : "))
    gm_high <- as.numeric(readline(prompt="Enter the upper cut off point for GM outlier (mostly between 0.96 and 1  : "))
    bookings_high  <- as.numeric(readline(prompt="Enter the upper cut off point for bookings outlier (mostly between 0.96 and 1  : "))
  }


  time_5 = Sys.time()

  # summary module

  map_summary_data <<- msr_sm_data(order_level_table = OL
                                   ,visitor_level_table = VL
                                   ,days_to_consider = days_to_consider
                                   , hopper = remove_hoppers
                                   , exposedToTest = exposed_to_test
                                   , control_id = control_id
                                   , TCR = TCR
                                   , TGPB = TGPB
                                   , TBPB = TBPB
                                   , TBPV = TBPV
                                   , TGPV = TGPV
                                   , booking_outlier =  bookings_high
                                   , gm_upper_outlier = gm_high
                                   , gm_lower_outlier = gm_low)


  print(paste("completed - Summary data part ",Sys.time()))

  msr_sm_graph(map_summary_data)

  Sys.sleep(3)

  print(paste("completed - Summary graphs ",Sys.time()))

  time_6 = Sys.time()


  # cohort module
  ## "is_new","region","website_country","channel_group","initial_device_type"

  map_cohort_data <<- msr_cohm_data(order_level_table = OL
                                    ,visitor_level_table = VL
                                    , control_id = control_id
                                    ,cohort_list = cohort_dimensions
                                    ,days_to_consider = days_to_consider
                                    , hopper = remove_hoppers
                                    , exposedToTest = exposed_to_test
                                    , booking_outlier =  bookings_high
                                    , gm_upper_outlier = gm_high
                                    , gm_lower_outlier = gm_low)

  print(paste("completed - Cohort data ",Sys.time()))


  msr_cohm_graph(map_cohort_data[!is.na(map_cohort_data$pvalue_cr), ])

  Sys.sleep(3)

  print(paste("completed - Cohort graphs ",Sys.time()))


  time_7 = Sys.time()

  # CARE metrics
  ## Execute SQL SPROC
  ### Care Module - Credit rate sproc

  if(rerun_sql_sproc == 1){
    sqlQuery(msr,paste("exec scratch..msr_credit '",order_table_name,"',", filter_out_reorders ,",0",sep=""))

    ### Care Module - contact rate sproc
    sqlQuery(msr,paste("exec scratch..msr_contact '",visitor_table_name,"',", contact_days_threshold ,",0",sep=""))

    print(paste("completed - CARE sql sprocs ",Sys.time()))
  }

  Sys.sleep(1)
  ## Execute R funtions
  ### Credit metrics
  map_credit_data <<- msr_care_credit_data(con=msr,
                                           zvalue_bound=1.96,
                                           visitor_table=visitor_table_name,
                                           order_table=order_table_name,
                                           credit_order_table=credit_table_name,
                                           hopper = remove_hoppers,
                                           control_id =  control_id)


  print(paste("completed - CARE Credit data ",Sys.time()))

  Sys.sleep(2)
  ### Contact metrics
  map_contact_data <<- msr_care_contact_data(con=msr,
                                             zvalue_bound=1.96,
                                             visitor_table=visitor_table_name,
                                             contact_table=contact_table_name,
                                             hopper = remove_hoppers,
                                             control_id =  control_id)


  print(paste("completed - CARE Contact data ",Sys.time()))

  Sys.sleep(2)

  msr_care_graph(map_credit_data[[1]], map_contact_data[[1]])

  Sys.sleep(3)

  print(paste("completed - CARE graphs  ",Sys.time()))

  time_8 = Sys.time()
  # Daily CR
  ## Pull data
  map_daily_cr_data <<-msr_sanity_dailyCR_data(order_level_table = OL
                                               , visitor_level_table = VL
                                               ,days_to_consider = days_to_consider
                                               , hopper = remove_hoppers
                                               , exposedToTest = exposed_to_test
                                               , control_id = control_id)

  print(paste("completed - Daily metric data ",Sys.time()))

  ## draw daily CR graph if there is atleast 2 touch dates
  if(length(unique(map_daily_cr_data$touch_date)) > 1){
    msr_sanity_dailyCR_graph(map_daily_cr_data, x_axis_day_interval)
  }

  Sys.sleep(3)

  print(paste("completed - Daily metrics graph  ",Sys.time()))

  time_9 = Sys.time()

  # Cummulative  CR
  map_cumulative_cr_data <<- msr_sanity_cumulativeCR_data(order_level_table = OL
                                                          , visitor_level_table = VL
                                                          ,days_to_consider = days_to_consider
                                                          , hopper = remove_hoppers
                                                          , exposedToTest = exposed_to_test
                                                          , control_id = control_id)

  print(paste("completed - Cumulative metrics data  ",Sys.time()))


  if(length(unique(map_cumulative_cr_data$dateVariable)) > 1){
    msr_sanity_cumulativeCR_graph (map_cumulative_cr_data, x_axis_day_interval)
  }

  Sys.sleep(3)

  print(paste("completed - Cumulative metrics graph  ",Sys.time()))


  time_end = Sys.time()



  if(demo_mode == 0){
  # Save the data in a commeon work space
  setwd("//vistaprint.net/common/share/Marketing/Public/Marketing_Analysis/MAP/Test_data")

  map_input <- list(gm_low , gm_high ,bookings_high)
  names(map_input) <- c('gm_low','gm_high','bookings_high')

  save(map_summary_data,map_cohort_data,map_gm_outlier_data,map_bkg_outlier_data,map_daily_cr_data,map_cumulative_cr_data,map_credit_data, map_contact_data,map_input,  file = data_file_name)
  # save run time details in common space
  load("//vistaprint.net/common/share/Marketing/Public/Marketing_Analysis/MAP/DoNotTouch/MAP_run_history.Rdata")

  MAP_run_temp <- data.frame('ordersproc' = as.numeric(time_1) - as.numeric(time_start),
                             'datapull' = as.numeric(time_2) - as.numeric(time_1),
                             'datadist' = as.numeric(time_3) - as.numeric(time_2),
                             'outlier' = as.numeric(time_4) - as.numeric(time_3),
                             'userentry' = as.numeric(time_5) - as.numeric(time_4),
                             'summary' = as.numeric(time_6) - as.numeric(time_5),
                             'cohort' = as.numeric(time_7) - as.numeric(time_6),
                             'care' = as.numeric(time_8) - as.numeric(time_7),
                             'daily' = as.numeric(time_9) - as.numeric(time_8),
                             'cumulative' = as.numeric(time_end) - as.numeric(time_9),
                             'total_time' = as.numeric(time_end) - as.numeric(time_start),
                             'machine' = Sys.info()[[1]],
                             'user' = Sys.info()[[6]],
                             'MAP_version' = packageDescription("vistaprintMAP")$Version,
                             'test_id' = test_id,
                             'test_type_id' = test_type_id,
                             'rundate' = Sys.time())

  MAP_run_history <- rbind(MAP_run_history,MAP_run_temp)

  save(MAP_run_history,  file = "//vistaprint.net/common/share/Marketing/Public/Marketing_Analysis/MAP/DoNotTouch/MAP_run_history.Rdata")

  }

}

