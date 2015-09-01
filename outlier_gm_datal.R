#' Pulls data for gm outlier data
#' @description Pulls gmoutlier data for your test
#' @return  data to be used for outlier graphs
#' @export


msr_outlier_gm_data <- function(order_level_table, visitor_level_table, days_to_consider, hopper, exposedToTest, control_id)
{


  OL <- filter(order_level_table,days_from_touch <= days_to_consider)

  # Remove hoppers
  VL <- filter(visitor_level_table,is_hopper <= hopper)

  # Remove if exposed to test constraint
  if(exposedToTest == 1)
  {VL <- subset(VL,exposedToTest == 1)}

  VL <- filter(VL, control_test != 'AC')

  #create a buyer level table from the order table
  BL <- OL %>%
    group_by(test_id, visitor_id, test_sub_id, control_test, test_name, test_sub_name) %>%
    summarise(booking_usd = sum(booking_usd),gm_usd = sum(gm_usd), order_count = max(order_count))

  #create a visitor level table with order info
  VL2 <- left_join(VL, BL[,c("visitor_id","booking_usd","gm_usd", "order_count")], by = "visitor_id")

  #update date
  VL2$touch_date <- as.Date(VL2$touch_date)

  ##recreate buyer level table after filtering for hoppers and exposed to test
  BL2 <-filter(VL2, order_count > 0)

  #set cutoff points
  gm_cutoffs_max <- seq(from = 0.99, to = 1.00, by = 0.001)
  gm_cutoffs_max2 <- seq(from = 0.960, to = 0.9875, by = 0.0025)
  gm_cutoffs_max <- c(gm_cutoffs_max2, gm_cutoffs_max)

  gm_cutoffs_min <- -0.001 ##fix min cutoff to 0.1%

  gm_list <-data.frame(gm_cutoffs_min, gm_cutoffs_max)

  ##View(gm_list)

  num_row <- nrow(gm_list)

  # Pulls the cut off point for different values (for the first 1% by 0.01% and the last 5% by o.1%)
  msr_oulier_gm_quantile_low <- data.frame(amount = quantile(BL2$gm_usd, c(seq(0,0.002,0.0001)), na.rm = TRUE), quantile_point = c(seq(0,0.002,0.0001)))

  ##set final table
  # Initialize an empty data frame to store all summary details
  BL_loop_final <- {}


  ##looping through all gm_cutoff points
  for (gm_cutoff in 1:num_row) {

    #resetting temp table
    BL_play <- BL2

    gm_list_subset <- gm_list[gm_cutoff,1:2] ##pulling 1 row at a time

    #setting gm cutoff values
    gm_quantile_min <- quantile(BL_play$gm_usd, c(abs(gm_list_subset$gm_cutoffs_min)) , na.rm = TRUE)
    gm_quantile_max <- quantile(BL_play$gm_usd, c(gm_list_subset$gm_cutoffs_max) , na.rm = TRUE)

    #resetting values above and below cutoffs
    BL_play$gm_usd[BL_play$gm_usd > gm_quantile_max] <- gm_quantile_max
    BL_play$gm_usd[BL_play$gm_usd < gm_quantile_min] <- gm_quantile_min

    control <- subset(BL_play,  test_sub_id == control_id )
    control_metadata <- data.frame(exp_name = as.character(control$test_name[1]), c_name = as.character(control$test_sub_name[1]), c_id = as.character(control$test_sub_id[1]))

    ##looping for all the different test cells to compare to the control
    for(TestNum_x in unique(BL_play$test_sub_id[BL_play$test_sub_id != control_id ])){

      test <- subset(BL_play,  test_sub_id == TestNum_x)
      test_metadata <- data.frame(t_name = as.character(unique(test$test_sub_name)), t_id = as.character(unique(test$test_sub_id)))

      GMPB_v1 <- rbind(control, test)

      # Calcualte T test metric for GM per buyer  and produce a summary row
      Ttest <- t.test(control$gm_usd,test$gm_usd, alternative="two.sided")

      GMPB <- GMPB_v1 %>%  group_by("gm_per_buyer") %>%
        summarise(c_denominator_count = sum(test_sub_id == control_id ),
                  c_value = mean(gm_usd[test_sub_id == control_id]),
                  c_visitors_flattened = sum(test_sub_id == control_id & gm_usd == gm_quantile_max),
                  t_denominator_count = sum(test_sub_id == TestNum_x ),
                  t_value = mean(gm_usd[test_sub_id == TestNum_x]),
                  t_visitors_flattened = sum(test_sub_id == TestNum_x & gm_usd == gm_quantile_max))

      GMPB1 <- cbind(metric = "gm_per_buyer" ,control_metadata, test_metadata, GMPB[, 2:7], z_score = Ttest$statistic, pvalue = Ttest$p.value, cutoff_perc_max =gm_list[gm_cutoff,2], cutoff_value_max = gm_quantile_max)

      GMPB1$significant <- ifelse(GMPB1$z_score >1.96, 1, ifelse(GMPB1$z_score < -1.96, 1, 0))
      GMPB1$per_change <- ((GMPB1$t_value - GMPB1$c_value)/GMPB1$c_value)

      if(is.null(BL_loop_final))
      { BL_loop_final <- GMPB1
      }else {BL_loop_final <- rbind(BL_loop_final,GMPB1) }

    }
  }

  return(list(BL_loop_final,msr_oulier_gm_quantile_low))
}
