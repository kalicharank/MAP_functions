#MSR Cohort Module Data Function
#Author: Christian Ulmer
#Last Update: 9/4/2015
#' MSR cohort module data function
#'
#' To produce cohort level summary data
#' @description Takes the order level and visitor level tables from the order and visitor modules and produces summary metrics for each specified cohort
#' @return  data frame with metrics for each cohort and test cell with significances and z scores
#' @param order_level_table Output from order sproc
#' @param visitor_level_table Output from visitor sproc
#' @param cohort_list list of cohorts (breakdowns) by which to group the data
#' @param days_to_consider No of days allowed from first touch point to an order for metric calculations
#' @param booking_outlier Ordering visitors whose bookings  fall above this limit is capped
#' @param gm_lower_outlier Ordering visitors whose GM fall above this limit is capped
#' @param gm_upper_outlier Ordering visitors whose GM  fall below this limit is capped
#' @param hopper 0 to exclude hoppers from analysis
#' @param exposed_to_test To exclude visitors who are assigned to test, but not exposed to them (default is 0 , to include all visitors in analysis)
#' @param control_id The test_sub_id  against which all other test_sub_id is evaluated against
#' @example output_table <- msr_cohm_data(order_level_table = OL,visitor_level_table = VL,cohort_list = c("is_new","region"),control_id = 25512)
#' @export


msr_cohm_data <- function(order_level_table,visitor_level_table,cohort_list = c("is_new","region","website_country","channel_group","initial_device_type"),days_to_consider = 7,booking_outlier = 0.99,gm_lower_outlier = 0.001,gm_upper_outlier = 0.99,hopper = 0,exposedToTest = 0,control_id = 0 )
{
  # Pull order level table , group at visitor level and summarise order_count, booking usd and gm_usd
  # obselete SubQ <- paste("select visitor_id,max(order_count) as order_count, sum(booking_usd) as booking_usd, sum(GM_usd) as GM_usd from", Order_table_name," where days_from_touch <= ",toString(days_to_consider), " group by visitor_id ")
  # Obselete OL2 <- sqlQuery(msr,SubQ)

  MSR_Cohm_OV <- filter(order_level_table,days_from_touch <= days_to_consider) %>% group_by(visitor_id) %>% summarise(order_count = max(order_count), booking_usd = sum(booking_usd),gm_usd = sum(gm_usd) )

  # Remove hoppers
  MSR_Cohm_VL <- filter(visitor_level_table,is_hopper <= hopper)

  # Remove if exposed to test constraint
  if(exposedToTest == 1)
  {MSR_Cohm_VL <- filter(MSR_Cohm_VL,exposed_to_test == 1)}

  # Merge Filtered Visitor table and order table (summarized at visitor level)  # performs a left join
  MSR_Cohm_CL <- left_join(x= MSR_Cohm_VL ,y = MSR_Cohm_OV, by = "visitor_id", all.x = TRUE) # matches with my number from SQL Ntile function
  # QA checking order count -  a <- MSR_Cohm_CL[!is.null(MSR_Cohm_CL$Order_count)] ; sum(a$Order_count[!is.na(a$Order_count)] > 0)

  # Find outliers for bookings & GM and cap them
  # na.rm = TRUE is to remove null values (from visitors who didnt order) before calculating outlier values
  Bookings_quantiles <- quantile( MSR_Cohm_CL$booking_usd, c(booking_outlier) , na.rm = TRUE)
  GM_quantiles <- quantile( MSR_Cohm_CL$gm_usd, c(gm_lower_outlier,gm_upper_outlier ) , na.rm = TRUE)

  # QA step sum(MSR_Cohm_CL$Booking_usd[!is.na(MSR_Cohm_CL$Booking_usd)]) --25860214
  MSR_Cohm_CL$booking_usd[MSR_Cohm_CL$booking_usd > Bookings_quantiles[1]] <- Bookings_quantiles[1]
  MSR_Cohm_CL$gm_usd[MSR_Cohm_CL$gm_usd > GM_quantiles[2]] <- GM_quantiles[2]
  MSR_Cohm_CL$gm_usd[MSR_Cohm_CL$gm_usd < GM_quantiles[1]] <- GM_quantiles[1]

  #construct loop to iterate in each element in the cohort list

  # Initialize an empty data frame to store all summary details
  MSR_Cohm_output_table <- {}

  control <- filter(MSR_Cohm_CL,  test_sub_id == control_id )

  control_metadata <- data.frame(exp_name = as.character(control$test_name[1]), c_name = as.character(control$test_sub_name[1]), c_id = as.character(control$test_sub_id[1]))

  for(cohort_x in cohort_list){

    #inner loop iterates on test cell, so we can run multiple t test comparisons for each test cell

    for(test_y in unique(MSR_Cohm_CL$test_sub_id[MSR_Cohm_CL$control_test != 'AC' & MSR_Cohm_CL$test_sub_id != control_id])){

      #create table that only includes control and the yth test cell
      test <- filter(MSR_Cohm_CL,  test_sub_id == test_y )

      OL2 <- bind_rows(control, test)

      test_metadata <- data.frame(t_name = as.character(unique(test$test_sub_name)), t_id = as.character(unique(test$test_sub_id)))

      #aggregate by cohort_x, use "dynamic SQL" equivalent
      call <- substitute(
        Temp <- OL2 %>%
          group_by(cohort_1) %>%
          summarise(c_count = sum(visitor_id > 0 & test_sub_id == control_id),
                    t_count = sum(visitor_id > 0 & test_sub_id == test_y),
                    c_ordering_visitor_count = sum(order_count[!is.na(order_count) & test_sub_id == control_id] > 0),
                    t_ordering_visitor_count = sum(order_count[!is.na(order_count) & test_sub_id == test_y] > 0),
                    c_gm = sum(gm_usd[test_sub_id == control_id], na.rm = TRUE),
                    t_gm = sum(gm_usd[test_sub_id == test_y], na.rm = TRUE),
                    c_bookings = sum(booking_usd[test_sub_id == control_id], na.rm = TRUE),
                    t_bookings = sum(booking_usd[test_sub_id == test_y], na.rm = TRUE),
                    #don't run t-tests if control or test visitor counts below 500, or control ot test order counts below 30
                    pvalue_gm_buyer = ifelse((sum(visitor_id > 0 & test_sub_id == control_id) <= 500 | sum(visitor_id > 0 & test_sub_id == test_y) <= 500 |
                                                sum(order_count[!is.na(order_count) & test_sub_id == control_id] > 0) <= 30 | sum(order_count[!is.na(order_count) & test_sub_id == control_id] > 0) <= 30),
                                             NaN,
                                             t.test(gm_usd[test_sub_id == control_id],gm_usd[test_sub_id == test_y],na.rm = TRUE, alternative="two.sided")$p.value),
                    pvalue_bookings_buyer = ifelse((sum(visitor_id > 0 & test_sub_id == control_id) <= 500 | sum(visitor_id > 0 & test_sub_id == test_y) <= 500 |
                                                      sum(order_count[!is.na(order_count) & test_sub_id == control_id] > 0) <= 30 | sum(order_count[!is.na(order_count) & test_sub_id == control_id] > 0) <= 30),
                                                   NaN,
                                                   t.test(booking_usd[test_sub_id == control_id],booking_usd[test_sub_id == test_y],na.rm = TRUE, alternative="two.sided")$p.value),
                    pvalue_cr = ifelse((sum(visitor_id > 0 & test_sub_id == control_id) <= 500 | sum(visitor_id > 0 & test_sub_id == test_y) <= 500 |
                                          sum(order_count[!is.na(order_count) & test_sub_id == control_id] > 0) <= 30 | sum(order_count[!is.na(order_count) & test_sub_id == control_id] > 0) <= 30),
                                       NaN,
                                       t.test(ifelse(is.na(order_count[test_sub_id == control_id]),0,1),ifelse(is.na(order_count[test_sub_id == test_y]),0,1),alternative="two.sided")$p.value),
                    z_score_gm_buyer = ifelse((sum(visitor_id > 0 & test_sub_id == control_id) <= 500 | sum(visitor_id > 0 & test_sub_id == test_y) <= 500 |
                                                 sum(order_count[!is.na(order_count) & test_sub_id == control_id] > 0) <= 30 | sum(order_count[!is.na(order_count) & test_sub_id == control_id] > 0) <= 30),
                                              NaN,
                                              t.test(gm_usd[test_sub_id == control_id],gm_usd[test_sub_id == test_y],na.rm = TRUE, alternative="two.sided")$statistic),
                    z_score_bookings_buyer = ifelse((sum(visitor_id > 0 & test_sub_id == control_id) <= 500 | sum(visitor_id > 0 & test_sub_id == test_y) <= 500 |
                                                       sum(order_count[!is.na(order_count) & test_sub_id == control_id] > 0) <= 30 | sum(order_count[!is.na(order_count) & test_sub_id == control_id] > 0) <= 30),
                                                    NaN,
                                                    t.test(booking_usd[test_sub_id == control_id],booking_usd[test_sub_id == test_y],na.rm = TRUE, alternative="two.sided")$statistic),
                    z_score_cr = ifelse((sum(visitor_id > 0 & test_sub_id == control_id) <= 500 | sum(visitor_id > 0 & test_sub_id == test_y) <= 500 |
                                           sum(order_count[!is.na(order_count) & test_sub_id == control_id] > 0) <= 30 | sum(order_count[!is.na(order_count) & test_sub_id == control_id] > 0) <= 30),
                                        NaN,
                                        t.test(ifelse(is.na(order_count[test_sub_id == control_id]),0,1),ifelse(is.na(order_count[test_sub_id == test_y]),0,1),alternative="two.sided")$statistic)
          ),list(cohort_1 = as.name(cohort_x)))

      #print(call)
      eval(call)

      #add columns for cohort name, control_id, test_sub_id, and values
      Temp$cohort <- cohort_x
      Temp$c_cr <- (Temp$c_ordering_visitor_count/Temp$c_count)
      Temp$t_cr <- (Temp$t_ordering_visitor_count/Temp$t_count)
      Temp$c_bookings_buyer <- (Temp$c_bookings/Temp$c_ordering_visitor_count)
      Temp$t_bookings_buyer <- (Temp$t_bookings/Temp$t_ordering_visitor_count)
      Temp$c_gm_buyer <- (Temp$c_gm/Temp$c_ordering_visitor_count)
      Temp$t_gm_buyer <- (Temp$t_gm/Temp$t_ordering_visitor_count)
      Temp$bookings_buyer_diff <- ifelse(Temp$c_bookings_buyer == 0, 0, (Temp$t_bookings_buyer-Temp$c_bookings_buyer)/Temp$c_bookings_buyer)
      Temp$gm_buyer_diff <- ifelse(Temp$c_gm_buyer == 0, 0, (Temp$t_gm_buyer-Temp$c_gm_buyer)/Temp$c_gm_buyer)
      Temp$cr_diff <- ifelse(Temp$c_cr == 0, 0, (Temp$t_cr-Temp$c_cr)/Temp$c_cr)
      Temp$exp_name <- control_metadata$exp_name
      Temp$c_name <- control_metadata$c_name
      Temp$c_id <- control_id
      Temp$t_name <- test_metadata$t_name
      Temp$t_id <- test_y

      #change name of cohort to dimension
      colnames(Temp)[1] = "dimension"
      Temp$dimension <- as.character(Temp$dimension)

      #insert into output table
      if(is.null(MSR_Cohm_output_table))
      { MSR_Cohm_output_table <- Temp
      }else {MSR_Cohm_output_table <- bind_rows(MSR_Cohm_output_table,Temp)   }
    }
  }

  #reorder columns to make it more readable
  MSR_Cohm_output_table <- MSR_Cohm_output_table[c(26:30,16,1,2:15,17:25)]

  return(MSR_Cohm_output_table)
}
