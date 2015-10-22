#' Cumulative CR over time data function
#' @description Pulls data for Cumulative CR over time
#' @return  a data frame with cumulative CR over time
#' @export

msr_sanity_cumulativeCR_data <- function(order_level_table, visitor_level_table, days_to_consider, hopper, exposedToTest, control_id)
{
  # debug order_level_table = OL; visitor_level_table = VL ; days_to_consider = 7; hopper = 0; exposedToTest =1; control_id = 25801

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

  BL$min_order_date <- as.Date(as.character(BL$min_order_date))

  #create a visitor level table with order info
  VL2 <- left_join(VL, BL[,c("visitor_id","booking_usd","gm_usd", "order_count", "min_order_date")], by = "visitor_id")

  VL2$ordered <- ifelse(!is.na(VL2$order_count),1,0)

  #update date
  VL2$touch_date <- as.Date(as.character(VL2$touch_date))


  # set the date threshold for your analysis
  min_date  <- as.Date(as.character(min(VL2$touch_date)))
  max_date  <- as.Date(as.character(max(BL$min_order_date)))
  date_seq <- data.frame(seq(min_date, max_date,1))

  # pulls the test_sub_id and thier name
  test_sub_id_seq <- (VL %>% group_by(test_sub_id, test_sub_name) %>% summarise(cnt = sum(1)))[ ,c(1,2)]

  # cartesian merge date and test_sub_id
  main_table <- merge(date_seq , test_sub_id_seq)
  names(main_table) <- c('date_x', 'test_sub_id', 'test_sub_name')

  ## Group visitor count by touch date
  visitor_count <- VL2 %>% group_by(test_sub_id, touch_date) %>% summarise(visitor_count = sum(visitor_id >0))
  names(visitor_count) <- c('test_sub_id', 'date_x','visitor_cnt' )
  visitor_count <- left_join(main_table, visitor_count, by = c('test_sub_id','date_x'))
  visitor_count[is.na(visitor_count$visitor_cnt),c('visitor_cnt')] <- 0

  # group ordering buyers by min_order_date
  buyer_count <- filter(VL2,ordered ==1)  %>% group_by(test_sub_id, min_order_date) %>% summarise(buyer_cnt = sum(visitor_id >0))
  names(buyer_count) <- c('test_sub_id', 'date_x','buyer_cnt' )
  buyer_count <- left_join(main_table[ , c('date_x', 'test_sub_id')], buyer_count, by = c('test_sub_id','date_x'))
  buyer_count[is.na(buyer_count$buyer_cnt),c('buyer_cnt')] <- 0

  # combine buyer and order count
  combined_count <- inner_join(visitor_count, buyer_count, by = c('date_x','test_sub_id'))

  # calculate the cumulate visitor and buyer count by date_x for each test_sub_id
  combined_count <- combined_count %>% group_by(test_sub_id) %>% arrange(date_x) %>% mutate(cum_visitor_cnt = cumsum(visitor_cnt), cum_buyer_cnt = cumsum(buyer_cnt))

  # filter if any rows has zero count, so you dont get divide by zero when calculating CR. This step is because i am paranoid of div by zero.
  combined_count <- filter(combined_count, cum_visitor_cnt > 0 )

  # Calculate CR
  combined_count$CR <- combined_count$cum_buyer_cnt/combined_count$cum_visitor_cnt

  # split control and test and combine them column wise
  cum_control <- filter(combined_count, test_sub_id == control_id)
  cum_test <- filter(combined_count, test_sub_id != control_id)
  cum_final <- inner_join(cum_control[, c('date_x',"cum_visitor_cnt",'cum_buyer_cnt','CR')], cum_test[, c('date_x','test_sub_name',"cum_visitor_cnt",'cum_buyer_cnt','CR')], by = 'date_x')
  names(cum_final) <- c('dateVariable','cum_visitor_ctrl','cum_buyers_ctrl','CR_ctrl','test_sub_name','cum_visitor_test','cum_buyers_test','CR_test')


  #add in percent lift
  cum_final$perc_lift <- (cum_final$CR_test - cum_final$CR_ctrl)/cum_final$CR_ctrl

  # add SD
  cum_final$sd <- sqrt((cum_final$CR_test*(1-cum_final$CR_test)/cum_final$cum_visitor_test)+(cum_final$CR_ctrl*(1-cum_final$CR_ctrl)/cum_final$cum_visitor_ctrl))

  #add in sensitivity
  cum_final$sensitivity <- 1.96*cum_final$sd/cum_final$CR_ctrl

  # Add z score
  cum_final$zscore <- (cum_final$CR_test-cum_final$CR_ctrl)/cum_final$sd

  #flagging significance
  cum_final$significant <- ifelse(cum_final$zscore >1.96, 1, ifelse(cum_final$zscore < -1.96, 1, 0))


  return(cum_final)

}

