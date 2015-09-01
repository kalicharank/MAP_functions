#' Pulls data for daily CR
#' @description Function to produce daily CR details
#' @return  data frame with details about daily CR
#' @export


msr_sanity_dailyCR_data <- function(order_level_table, visitor_level_table, days_to_consider, hopper, exposedToTest, control_id)
{



  OL <- filter(order_level_table,days_from_touch <= days_to_consider)

  # Remove hoppers
  VL <- filter(visitor_level_table,is_hopper <= hopper)

  # Remove if exposed to test constraint
  if(exposedToTest == 1)
  {VL <- subset(VL,exposed_to_test == 1)}

  VL <- filter(VL, control_test != 'AC')



  #-----------------------------------------------------#
  #   GOAL 1 - x-day CR by first touch date cohort
  #-----------------------------------------------------#

  #create a buyer level table from the order table
  BL <- OL %>%
    group_by(test_id, visitor_id, test_sub_id, control_test, test_name, test_sub_name) %>%
    summarise(booking_usd = sum(booking_usd),gm_usd = sum(gm_usd), order_count = max(order_count))

  #create a visitor level table with order info
  VL2 <- left_join(VL, BL[,c("visitor_id","booking_usd","gm_usd", "order_count")], by = "visitor_id")

  #update date
  VL2$touch_date <- as.Date(as.character(VL2$touch_date))

  # Add a new field called ordered , tells if a customer placed an order or not
  VL2$ordered <- ifelse(!is.na(VL2$order_count),1,0)
  ##recreate buyer level table after filtering for hoppers and exposed to test
  BL2 <-filter(VL2, ordered == 1)

  VLsum_final <- {}


  for(Touch_date_X in unique(as.numeric(VL2$touch_date)))
  {

    control <- subset(VL2,  test_sub_id == control_id & as.numeric(VL2$touch_date) == Touch_date_X)
    control_metadata <- data.frame(exp_name = as.character(control$test_name[1]), c_name = as.character(control$test_sub_name[1]), c_id = as.character(control$test_sub_id[1]))


    ##looping for all the different test cells to compare to the control
    for(TestNum_x in unique(VL2$test_sub_id[VL2$test_sub_id != control_id ])){

      test <- subset(VL2,  test_sub_id == TestNum_x & as.numeric(VL2$touch_date) == Touch_date_X)
      test_metadata <- data.frame(t_name = as.character(unique(test$test_sub_name)), t_id = as.character(unique(test$test_sub_id)))

      Daily_CR <- rbind(control, test)

      # Calcualte T test metric for CR and produce a summary row
      Ttest <- t.test(control$ordered, test$ordered, alternative="two.sided")

      CR <- Daily_CR %>% group_by("CR% by touch_date cohort") %>%
        summarise(c_denominator_count = sum(visitor_id > 0 & test_sub_id == control_id),
                  c_value = mean(ordered[test_sub_id == control_id]),
                  c_var =  var(ordered[test_sub_id == control_id]),
                  t_denominator_count = sum(visitor_id > 0 & test_sub_id == TestNum_x),
                  t_value = mean(ordered[test_sub_id == TestNum_x]),
                  t_var =  var(ordered[test_sub_id == TestNum_x]))

      # View(CR)
      CR1 <- cbind(control_metadata, test_metadata, CR[, 2:7], z_score = Ttest$statistic, pvalue = Ttest$p.value, touch_date = as.Date(Touch_date_X, origin="1970-01-01"))

      CR1$significant <- ifelse(CR1$z_score >1.96, 1, ifelse(CR1$z_score < -1.96, 1, 0))
      CR1$per_change <- 100*((CR1$t_value - CR1$c_value)/CR1$c_value)

      if(is.null(VLsum_final))
      { VLsum_final <- CR1
      }else {VLsum_final <- rbind(VLsum_final,CR1) }

    }
  }

  return(VLsum_final)

}


