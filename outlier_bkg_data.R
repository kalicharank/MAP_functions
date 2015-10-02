#' Pulls data for bookings outlier data
#' @description Pulls booking outlier data for your test
#' @return  data to be used for outlier graphs
#' @export

msr_outlier_booking_data <- function(order_level_table, visitor_level_table, days_to_consider, hopper, exposedToTest, control_id)
{

  OL <- filter(order_level_table,days_from_touch <= days_to_consider)



  # Remove if exposed to test constraint
  if(exposedToTest == 1)
  {VL <- subset(VL,exposed_to_test == 1)}

  # Remove hoppers and alt control cell
  VL <- filter(VL, control_test != 'AC' & is_hopper <= hopper)

  #create a buyer level table from the order table
  BL <- OL %>%
    group_by(test_id, visitor_id, test_sub_id, control_test, test_name, test_sub_name) %>%
    summarise(booking_usd = sum(booking_usd),gm_usd = sum(gm_usd), order_count = max(order_count))

  #create a visitor level table with order info
  VL2 <- left_join(VL, BL[,c("visitor_id","booking_usd","gm_usd", "order_count")], by = "visitor_id")

  #update date
  VL2$touch_date <- as.Date(as.character(VL2$touch_date))

  ##recreate buyer level table after filtering for hoppers and exposed to test
  BL2 <-filter(VL2, order_count > 0)


  ## data to plot frequency plot by graph.
  bkg_dist <- BL2
  bucket_width <- 5 # the minimum resolution that's going to be used in this case

  bkg_dist$bucket <- floor(bkg_dist[ , c('booking_usd')]/bucket_width)
  bkg_dist <- bkg_dist %>%  group_by(bucket, test_sub_name) %>% summarise(no_of_ordering_visitors = sum(visitor_id > 0))



  ##defining list of cutoffs to look at for $/buyer
  bk_cutoffs_high <- seq(from = 0.99, to = 1.00, by = 0.001)
  bk_cutoffs_low <- seq(from = 0.960, to = 0.9875, by = 0.0025)

  bk_cutoffs <- c(bk_cutoffs_low, bk_cutoffs_high)
  bk_list <-data.frame(bk_cutoffs)

  ##set final table
  Bkg_loop_final <-{}

  ##looping through all bkg_cutoff points
  for (cutoff in unique(bk_list$bk_cutoffs)) {

    #resetting temp table
    BL_play <- BL2

    #setting cutoff point
    Bookings_quantiles <- quantile(BL_play$booking_usd, c(cutoff) , na.rm = TRUE)

    #updating table with cutoffs
    BL_play$booking_usd[BL_play$booking_usd > Bookings_quantiles[1]] <- Bookings_quantiles[1]

    control <- subset(BL_play,  test_sub_id == control_id)
    control_metadata <- data.frame(exp_name = as.character(control$test_name[1]), c_name = as.character(control$test_sub_name[1]), c_id = as.character(control$test_sub_id[1]))

    ##looping for all the different test cells to compare to the control
    for(TestNum_x in unique(BL_play$test_sub_id[BL_play$test_sub_id != control_id ])){

      test <- subset(BL_play,  test_sub_id == TestNum_x)
      test_metadata <- data.frame(t_name = as.character(unique(test$test_sub_name)), t_id = as.character(unique(test$test_sub_id)))

      BKPB_v1 <- rbind(control, test)

      # Calcualte T test metric for bkg per buyer  and produce a summary row
      Ttest <- t.test(control$booking_usd,test$booking_usd, alternative="two.sided")

      BKPB <- BKPB_v1 %>%  group_by("bkg_per_buyer") %>%
        summarise(c_denominator_count = sum(test_sub_id == control_id ),
                  c_value = mean(booking_usd[test_sub_id == control_id])  ,
                  c_visitors_flattened = sum(test_sub_id == control_id & booking_usd == Bookings_quantiles),
                  t_denominator_count = sum(test_sub_id == TestNum_x),
                  t_value = mean(booking_usd[test_sub_id == TestNum_x])  ,
                  t_visitors_flattened = sum(test_sub_id == TestNum_x & booking_usd == Bookings_quantiles))

      BKPB1 <- cbind(metric = "bkg_per_buyer" ,control_metadata, test_metadata, BKPB[, 2:7], z_score = Ttest$statistic, pvalue = Ttest$p.value, cutoff_perc_max  =cutoff, cutoff_value_max = Bookings_quantiles)

      BKPB1$significant <- ifelse(BKPB1$z_score >1.96, 1, ifelse(BKPB1$z_score < -1.96, 1, 0))
      BKPB1$per_change <- ((BKPB1$t_value - BKPB1$c_value)/BKPB1$c_value)

      if(is.null(Bkg_loop_final))
      { Bkg_loop_final <- BKPB1
      }else {Bkg_loop_final <- rbind(Bkg_loop_final,BKPB1) }

    }
  }

  return(list(Bkg_loop_final,bkg_dist))

}
