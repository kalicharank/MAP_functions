#' Cumulative CR over time data function
#' @description Pulls data for Cumulative CR over time
#' @return  a data frame with cumulative CR over time
#' @export

msr_sanity_cumulativeCR_data <- function(order_level_table, visitor_level_table, days_to_consider, hopper, exposedToTest, control_id)
{


  OL <- filter(order_level_table,days_from_touch <= days_to_consider)

  # Remove hoppers
  VL <- filter(visitor_level_table,is_hopper <= hopper)

  # Remove if exposed to test constraint
  if(exposedToTest == 1)
  {VL <- filter(VL,exposed_to_test == 1)}

  VL <- filter(VL, control_test != 'AC')

  #create a buyer level table from the order table
  BL <- OL %>%
    group_by(test_id, visitor_id, test_sub_id, control_test, test_name, test_sub_name, touch_date) %>%
    summarise(booking_usd = sum(booking_usd),gm_usd = sum(gm_usd), order_count = max(order_count), min_order_date = min(order_date))

  #create a visitor level table with order info
  VL2 <- left_join(VL, BL[,c("visitor_id","booking_usd","gm_usd", "order_count", "min_order_date")], by = "visitor_id")

  #update date
  VL2$touch_date <- as.Date(as.character(VL2$touch_date))

  ##recreate buyer level table after filtering for hoppers and exposed to test
  BL2 <-filter(VL2, order_count > 0)

  ##cumulative Visitor counts
  VLcum_pop <- {}

  for (grp in unique(VL2$test_sub_id)){
    subs <- filter(VL2, test_sub_id == grp)

    #summary information
    subs_info <- subs  %>%
      group_by(test_sub_name, test_sub_id, control_test, touch_date) %>%
      summarise(visitor_count = sum(visitor_id >0)) %>%
      arrange(touch_date) %>%
      mutate(cum_visitor_count = cumsum(visitor_count))

    # adding values to summ dataframe
    if(is.null(VLcum_pop))  {
      VLcum_pop <- subs_info
    }
    else {
      VLcum_pop <- rbind(VLcum_pop,subs_info)
    }
  }

  colnames(VLcum_pop)[colnames(VLcum_pop) == 'test_sub_id.x'] <- 'test_sub_id'

  ##cumulative Buyer counts (TAKES LONGER)
  VLcum_buyers<- {}
  BL2$touch_date <- as.Date(as.character(BL2$touch_date))
  BL2$min_order_date <- as.Date(as.character(BL2$min_order_date))


  ##counting cumulative buyers for each new day test was running
  for (day in unique(as.numeric(BL2$touch_date))) {

    #touch_date less than date  (try using filter instead)
    BL_sub <- subset(BL2, as.numeric(BL2$touch_date) <= day)
    BL_sub <- subset(BL_sub, as.numeric(BL_sub$min_order_date) <= day)

    #buyer level table in timeframe
    BL_sub1 <- BL_sub %>% group_by(visitor_id, test_sub_name, test_sub_id, control_test) %>% summarise(order_count = max(order_count))
    #summarising to get distinct buyer count
    BL_sub1 <- BL_sub1 %>% group_by(test_sub_name, test_sub_id, control_test) %>% summarise(cum_buyer_count = sum(visitor_id > 0))

    BL_sub1$dateVariable <- c(as.Date(day, origin="1970-01-01"))

    if(is.null(VLcum_buyers))  {
      VLcum_buyers <- BL_sub1
    }
    else {
      VLcum_buyers <- rbind(VLcum_buyers,BL_sub1)
    }
  }

  colnames(VLcum_pop)[colnames(VLcum_pop) == 'touch_date'] <- 'dateVariable'
  VL_cum_final <- inner_join(VLcum_pop, VLcum_buyers, by = c("test_sub_name","test_sub_id","control_test","dateVariable"))

  #add in CR
  VL_cum_final$CR <- (VL_cum_final$cum_buyer_count/VL_cum_final$cum_visitor_count)

  VL_cum_compare <- subset(VL_cum_final, test_sub_id == control_id)
  VL_cum_final <- subset(VL_cum_final, test_sub_id != control_id)

  #UPDATE THIS WITH COLUMN NAMES
  VL_cum_final <- left_join(VL_cum_final, VL_cum_compare[,c("test_sub_name","dateVariable", "cum_visitor_count", "cum_buyer_count", "CR")], by = "dateVariable")

  #rename the columns
  names(VL_cum_final) <- c("test_sub_name","test_sub_id", "control_test", "dateVariable" ,"visitor_test", "cum_visitor_test", "cum_buyers_test", "CR_test","sub_name_compared", "cum_visitor_ctrl", "cum_buyers_ctrl", "CR_ctrl")

  #add in percent lift
  VL_cum_final$perc_lift <- (VL_cum_final$CR_test - VL_cum_final$CR_ctrl)/VL_cum_final$CR_ctrl
  #add in the z score

  VL_cum_final$zscore <- (VL_cum_final$CR_test-VL_cum_final$CR_ctrl)/sqrt((VL_cum_final$CR_test*(1-VL_cum_final$CR_test)/VL_cum_final$cum_visitor_test)+(VL_cum_final$CR_ctrl*(1-VL_cum_final$CR_ctrl)/VL_cum_final$cum_visitor_ctrl))
  #flagging significance
  VL_cum_final$significant <- ifelse(VL_cum_final$zscore >1.96, 1, ifelse(VL_cum_final$zscore < -1.96, 1, 0))

  return(VL_cum_final)

}

