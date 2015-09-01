#' Produces summary metrics at overall level
#'
#' Summarizes the data at overall level and check if any of the primary metric is significantly impacted
#' @description Produces summary metrics at overall level
#' @return  a data frame of metrics along with significance,CI and etc
#' @param order_level_table Output from order sproc
#' @param visitor_level_table Output from visitor sproc
#' @param days_to_consider No of days allowed from first touch point to an order for metric calculations
#' @param booking_outlier Ordering visitors whose bookings  fall above this limit is capped
#' @param gm_lower_outlier Ordering visitors whose GM fall above this limit is capped
#' @param gm_upper_outlier Ordering visitors whose GM  fall below this limit is capped
#' @param hopper 0 to exclude hoppers from analysis
#' @param exposed_to_test To exclude visitors who are assigned to test, but not exposed to them (default is 0 , to include all visitors in analysis)
#' @param control_id The test_sub_id  against which all other test_sub_id is evaluated against
#' @param TCR Tail level for CR metric (-1 means is for one tail test that's expected to perform negatively, +1 means is for one tail test that's expected to perform postively and 0 is for two tail test)
#' @param TBPB Tail level for bookings per buyer metric
#' @param TGPB Tail level for gm per buyer metric
#' @param TBPV Tail level for bookings per visitor metric
#' @param TGPV Tail level for gm per visitor metric
#' @export
msr_sm_data <- function( order_level_table, visitor_level_table, days_to_consider = 7, booking_outlier = 0.99, gm_lower_outlier = 0.001, gm_upper_outlier = 0.99, hopper = 0, exposedToTest = 0, control_id = 0, TCR = 0 , TBPB = 0 , TGPB = 0, TBPV = 0, TGPV =0 )
{

  # Pull order level table , group at visitor level and summarise order_count, booking usd and gm_usd
  # obselete SubQ <- paste("select visitor_id,max(order_count) as order_count, sum(booking_usd) as booking_usd, sum(GM_usd) as GM_usd from", Order_table_name," where days_from_touch <= ",toString(days_to_consider), " group by visitor_id ")
  # Obselete OL2 <- sqlQuery(msr,SubQ)

  MSR_SM_OV <- subset(order_level_table,days_from_touch <= days_to_consider) %>% group_by(visitor_id) %>% summarise(order_count = max(order_count), booking_usd = sum(booking_usd),gm_usd = sum(gm_usd) )


  # Remove hoppers
  MSR_SM_VL <- subset(visitor_level_table,is_hopper <= hopper)

  # Remove if exposed to test constraint
  if(exposedToTest == 1)
  {MSR_SM_VL <- subset(MSR_SM_VL,exposed_to_test == 1)}

  # Remove alt control
  MSR_SM_VL <- subset(MSR_SM_VL,control_test != 'AC')

  # Merge Filtered Visitor table and order table (summarized at visitor level)  # performs a left join
  MSR_SM_CL <- merge(x= MSR_SM_VL ,y = MSR_SM_OV, by = "visitor_id", all.x = TRUE) # matches with my number from SQL Ntile function
  # QA checking order count -  a <- MSR_SM_CL[!is.null(MSR_SM_CL$Order_count)] ; sum(a$Order_count[!is.na(a$Order_count)] > 0)



  # Find outliers for bookings & GM and cap them
  # na.rm = TRUE is to remove null values (from visitors who didnt order) before calculating outlier values
  Bookings_quantiles <- quantile( MSR_SM_CL$booking_usd, c(booking_outlier ) , na.rm = TRUE)
  GM_quantiles <- quantile( MSR_SM_CL$gm_usd, c(gm_lower_outlier,gm_upper_outlier ) , na.rm = TRUE)

  # QA step sum(MSR_SM_CL$Booking_usd[!is.na(MSR_SM_CL$Booking_usd)]) --25860214
  MSR_SM_CL$booking_usd[MSR_SM_CL$booking_usd > Bookings_quantiles[1]] <- Bookings_quantiles[1]
  MSR_SM_CL$gm_usd[MSR_SM_CL$gm_usd > GM_quantiles[2]] <- GM_quantiles[2]
  MSR_SM_CL$gm_usd[MSR_SM_CL$gm_usd < GM_quantiles[1]] <- GM_quantiles[1]
  # QA step sum(MSR_SM_CL$Booking_usd[!is.na(MSR_SM_CL$Booking_usd)]) --24962982 -- both these numbers checks out with sql information

  # Since I want GM_vistor (and not just GM_buyer), updating the null values with 0 for GM_usd and booking_usd.
  # Since order_count is still "NA" for non buyers, i can use that to idenfity row of buyers vs. not
  MSR_SM_CL$booking_usd <- ifelse(!is.na(MSR_SM_CL$booking_usd),MSR_SM_CL$booking_usd,0.0)
  MSR_SM_CL$gm_usd <- ifelse(!is.na(MSR_SM_CL$gm_usd),MSR_SM_CL$gm_usd,0.0)

  # Add a new field called ordered , tells if a customer placed an order or not
  MSR_SM_CL$ordered <- ifelse(!is.na(MSR_SM_CL$order_count),1,0)

  # Initialize an empty data frame to store all summary details
  MSR_SM_SM2 <- {}

  control <- subset(MSR_SM_CL,  test_sub_id == control_id )

  control_metadata <- data.frame(exp_name = as.character(control$test_name[1]), c_name = as.character(control$test_sub_name[1]), c_id = as.character(control$test_sub_id[1]))

  # loop to iterate multiple test values
  for(TestNum_x in unique(MSR_SM_CL$test_sub_id[MSR_SM_CL$test_sub_id != control_id ])){

    # PUll a table with control and current test value under consideration

    test <- subset(MSR_SM_CL,  test_sub_id == TestNum_x )

    MSR_SM_SM1 <- rbind(control, test)

    test_metadata <- data.frame(t_name = as.character(unique(test$test_sub_name)), t_id = as.character(unique(test$test_sub_id)))

    # Calcualte T test metric for CR and produce a summary row
    Ttest <- t.test(control$ordered, test$ordered, alternative="two.sided")

    CR <- MSR_SM_SM1 %>% group_by("CR") %>%
      summarise(c_denominator_count = sum(visitor_id > 0 & test_sub_id == control_id),
                c_value = mean(ordered[test_sub_id == control_id]),
                c_var =  var(ordered[test_sub_id == control_id]),
                t_denominator_count = sum(visitor_id > 0 & test_sub_id == TestNum_x ),
                t_value = mean(ordered[test_sub_id == TestNum_x]),
                t_var =  var(ordered[test_sub_id == TestNum_x]))

    CR1 <- cbind(metric = "CR",control_metadata, test_metadata, CR[, 2:7], z_score = Ttest$statistic, pvalue = Ttest$p.value)

    # Calcualte T test metric for Bookings per buyer  and produce a summary row
    Ttest <- t.test(control$booking_usd[control$ordered == 1],test$booking_usd[test$ordered == 1], alternative="two.sided")

    BPB <- MSR_SM_SM1 %>% group_by("booking_per_buyer") %>%
      summarise(c_denominator_count = sum(ordered == 1 & test_sub_id == control_id ),
                c_value = mean(booking_usd[ordered == 1 & test_sub_id == control_id])  ,
                c_var = var(booking_usd[ordered == 1 & test_sub_id == control_id])  ,
                t_denominator_count = sum(ordered == 1 & test_sub_id == TestNum_x ),
                t_value = mean(booking_usd[ordered == 1 & test_sub_id == TestNum_x])  ,
                t_var = var(booking_usd[ordered == 1 & test_sub_id == TestNum_x]))

    BPB1 <- cbind(metric = "booking_per_buyer" ,control_metadata, test_metadata, BPB[, 2:7], z_score = Ttest$statistic, pvalue = Ttest$p.value )

    # Calcualte T test metric for GM per buyer  and produce a summary row
    Ttest <- t.test(control$gm_usd[control$ordered == 1],test$gm_usd[test$ordered == 1], alternative="two.sided")

    GMPB <- MSR_SM_SM1 %>%  group_by("gm_per_buyer") %>%
      summarise(c_denominator_count = sum(ordered == 1 & test_sub_id == control_id ),
                c_value = mean(gm_usd[ordered == 1 & test_sub_id == control_id])  ,
                c_var = var(gm_usd[ordered == 1 & test_sub_id == control_id])  ,
                t_denominator_count = sum(ordered == 1 & test_sub_id == TestNum_x ),
                t_value = mean(gm_usd[ordered == 1 & test_sub_id == TestNum_x])  ,
                t_var = var(gm_usd[ordered == 1 & test_sub_id == TestNum_x]))

    GMPB1 <- cbind(metric = "gm_per_buyer" ,control_metadata, test_metadata, GMPB[, 2:7], z_score = Ttest$statistic, pvalue = Ttest$p.value )


    # Calcualte T test metric for Bookings per visitor   and produce a summary row
    Ttest <- t.test(control$booking_usd,test$booking_usd, alternative="two.sided")

    BPV <- MSR_SM_SM1 %>%    group_by("booking_per_visitor") %>%
      summarise(c_denominator_count = sum(test_sub_id == control_id ),
                c_value = mean(booking_usd[test_sub_id == control_id])  ,
                c_var = var(booking_usd[test_sub_id == control_id])  ,
                t_denominator_count = sum( test_sub_id == TestNum_x ),
                t_value = mean(booking_usd[test_sub_id == TestNum_x])  ,
                t_var = var(booking_usd[test_sub_id == TestNum_x]))

    BPV1 <- cbind(metric = "booking_per_visitor" ,control_metadata, test_metadata, BPV[, 2:7], z_score = Ttest$statistic, pvalue = Ttest$p.value )



    # Calcualte T test metric for Bookings per visitor   and produce a summary row
    Ttest <- t.test(control$gm_usd,test$gm_usd, alternative="two.sided")

    GMPV <- MSR_SM_SM1 %>%   group_by("gm_per_visitor") %>%
      summarise(c_denominator_count = sum(test_sub_id == control_id ),
                c_value = mean(gm_usd[test_sub_id == control_id])  ,
                c_var = var(gm_usd[test_sub_id == control_id])  ,
                t_denominator_count = sum( test_sub_id == TestNum_x ),
                t_value = mean(gm_usd[test_sub_id == TestNum_x])  ,
                t_var = var(gm_usd[test_sub_id == TestNum_x]))

    GMPV1 <- cbind(metric = "gm_per_visitor" ,control_metadata, test_metadata, GMPV[, 2:7], z_score = Ttest$statistic, pvalue = Ttest$p.value )

    # combine the different metrics
    Temp = rbind(CR1, BPB1, GMPB1, BPV1, GMPV1)

    # test for bookings per visitor
    # t.test(subset(MSR_SM_CL,Test_sub_id == ControlNum_x)$Booking_usd,subset(MSR_SM_CL,Test_sub_id == TestNum_x)$Booking_usd,alternative="two.sided")

    if(is.null(MSR_SM_SM2))
    { MSR_SM_SM2 <- Temp
    }else {MSR_SM_SM2 <- rbind(MSR_SM_SM2,Temp) }



  }

  # Fill the 95, 90 and 80 confidence ranges in the summary and Fill if it's significant or not

  ## Step 1 - calculate combined variance and relative difference

  MSR_SM_SM2$sd <- sqrt((MSR_SM_SM2$t_var/MSR_SM_SM2$t_denominator_count) + (MSR_SM_SM2$c_var/MSR_SM_SM2$c_denominator_count))

  ## Step 2
  MSR_SM_SM2$low95 <- ((MSR_SM_SM2$t_value - (1.96*MSR_SM_SM2$sd))-MSR_SM_SM2$c_value)/MSR_SM_SM2$c_value
  MSR_SM_SM2$low90 <- ((MSR_SM_SM2$t_value - (1.65*MSR_SM_SM2$sd))-MSR_SM_SM2$c_value)/MSR_SM_SM2$c_value
  MSR_SM_SM2$low80 <- ((MSR_SM_SM2$t_value - (1.29*MSR_SM_SM2$sd))-MSR_SM_SM2$c_value)/MSR_SM_SM2$c_value
  MSR_SM_SM2$relative_diff = (MSR_SM_SM2$t_value - MSR_SM_SM2$c_value)/MSR_SM_SM2$c_value
  MSR_SM_SM2$high80 <- ((MSR_SM_SM2$t_value + (1.29*MSR_SM_SM2$sd))-MSR_SM_SM2$c_value)/MSR_SM_SM2$c_value
  MSR_SM_SM2$high90 <- ((MSR_SM_SM2$t_value + (1.65*MSR_SM_SM2$sd))-MSR_SM_SM2$c_value)/MSR_SM_SM2$c_value
  MSR_SM_SM2$high95 <- ((MSR_SM_SM2$t_value + (1.96*MSR_SM_SM2$sd))-MSR_SM_SM2$c_value)/MSR_SM_SM2$c_value

  # Say if we need one tail or two tail measurement
  MSR_SM_SM2$tail[MSR_SM_SM2$metric == "CR"] <- ifelse(TCR == 0, "2tail", ifelse(TCR == -1, "-1tail","+1tail"))
  MSR_SM_SM2$tail[MSR_SM_SM2$metric == "booking_per_buyer"] <- ifelse(TBPB == 0, "2tail", ifelse(TBPB == -1, "-1tail","+1tail"))
  MSR_SM_SM2$tail[MSR_SM_SM2$metric == "gm_per_buyer"] <- ifelse(TGPB == 0, "2tail", ifelse(TGPB == -1, "-1tail","+1tail"))
  MSR_SM_SM2$tail[MSR_SM_SM2$metric == "booking_per_visitor"] <- ifelse(TBPV == 0, "2tail", ifelse(TBPV == -1, "-1tail","+1tail"))
  MSR_SM_SM2$tail[MSR_SM_SM2$metric == "gm_per_visitor"] <- ifelse(TGPV == 0, "2tail", ifelse(TGPV == -1, "-1tail","+1tail"))

  # Calculate if the significance
  MSR_SM_SM2$significance[MSR_SM_SM2$tail == "2tail"] <- ifelse(MSR_SM_SM2$z_score[MSR_SM_SM2$tail == "2tail"]  <= -1.96,"Significant Increase",ifelse(MSR_SM_SM2$z_score[MSR_SM_SM2$tail == "2tail"]  >= 1.96,"Significant Decrease","Not significant"))
  MSR_SM_SM2$significance[MSR_SM_SM2$tail == "-1tail"] <- ifelse(MSR_SM_SM2$z_score[MSR_SM_SM2$tail == "-1tail"] >= 1.6449,"Significant Decrease","Not significant")
  MSR_SM_SM2$significance[MSR_SM_SM2$tail == "+1tail"] <- ifelse(MSR_SM_SM2$z_score[MSR_SM_SM2$tail == "+1tail"] <= -1.6449,"Significant Increase","Not significant")
  MSR_SM_SM2$chart_value[MSR_SM_SM2$tail == "2tail"] <- ifelse(MSR_SM_SM2$z_score[MSR_SM_SM2$tail == "2tail"]  <= -1.96,1,ifelse(MSR_SM_SM2$z_score[MSR_SM_SM2$tail == "2tail"]  >= 1.96,5,3))
  MSR_SM_SM2$chart_value[MSR_SM_SM2$tail == "-1tail"] <- ifelse(MSR_SM_SM2$z_score[MSR_SM_SM2$tail == "-1tail"] >= 1.6449,5,3)
  MSR_SM_SM2$chart_value[MSR_SM_SM2$tail == "+1tail"] <- ifelse(MSR_SM_SM2$z_score[MSR_SM_SM2$tail == "+1tail"] <= -1.6449,1,3)


  # For graphing purposes , Multiple CR by 100
  MSR_SM_SM2$t_value[MSR_SM_SM2$metric =="CR"] <- MSR_SM_SM2$t_value[MSR_SM_SM2$metric =="CR"]*100
  MSR_SM_SM2$c_value[MSR_SM_SM2$metric =="CR"] <- MSR_SM_SM2$c_value[MSR_SM_SM2$metric =="CR"]*100

  return(MSR_SM_SM2)
}
