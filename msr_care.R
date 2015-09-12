#########################################################################################################
# Care  metrics (Credit and contact % ) propotions test
# Author: Lin Y
# Modifier: Jialin, Charan
# Maintainer: Jialin/Lin Y
#########################################################################################################
# how to run the function
#aa <- msr_care_credit_data(con=msr, zvalue_bound=1.96,visitor_table='msr_analytics..MSR_VL_1_6372',order_table='msr_analytics..MSR_OL_1_6372', credit_order_table='msr_analytics..MSR_OL_1_6372_credit_level', hopper = 0, control_id = 25895)
#' CARE credit rate data module
#'
#' Produces a summary table with % shoppers having credit and significance from control
#' @description Produces a summary table with % shoppers having credit and significance from control
#' @return  a list that contains a data frame of control and test care credit metrics and stats information
#' @param con connection name to sql
#' @param zvalue_bound the Z threshold at which we will call the difference significanct
#' @param visitor_table the visitor table name
#' @param order_table the order table name
#' @param credit_order_table the shopper credit table name - this is the output from CARE credit sproc
#' @param hopper flag to include hoppers in analysis, 0 means exclude hoppers and 1 means include hoppers in the analysis
#' @param control_id the test_sub_id that should be used as the control. All other test_sub_id will be combpared against this
#' @export

msr_care_credit_data <- function(con, zvalue_bound=1.96,visitor_table,
                            order_table, credit_order_table, hopper, control_id){

  # uncomment for debugging inside the function
  # con=msr; zvalue_bound=1.96;visitor_table=VISITOR_TABLE;  order_table=ORDER_TABLE; credit_order_table=CREDIT_TABLE; hopper = 0 ; control_id = 0



  # get the credit combined data from another function
  msr_sql <- msr_sql_credit(visitor_table,order_table,credit_order_table, hopper = 1)
  df <- sqlQuery(channel=con,query=msr_sql)

  # calculation for credit rate
  df$credit_rate <- df$shopper_ct_credited/ df$shopper_ct

  # seprate out the control and test rows into two different tables
  control_row <- subset(df, test_sub_id == control_id)
  test_rows <- subset(df, test_sub_id != control_id)

  # enter the control values into the test_table
  test_rows$control_shopper_ct <- control_row$shopper_ct
  test_rows$control_shopper_ct_credited <- control_row$shopper_ct_credited
  test_rows$control_credit_rate <- control_row$credit_rate
  test_rows$control_name <- control_row$test_sub_name

  # calculate the z metrics and if the test is significant or not
      test_rows$combined_var <- (test_rows$control_shopper_ct_credited + test_rows$shopper_ct_credited) / (test_rows$control_shopper_ct + test_rows$shopper_ct )
      test_rows$SE <- sqrt(  (test_rows$combined_var*(1- test_rows$combined_var) / test_rows$shopper_ct )   +    (test_rows$combined_var*(1- test_rows$combined_var) / test_rows$control_shopper_ct )  )
      test_rows$zvalue <-   (test_rows$control_credit_rate  - test_rows$credit_rate)/ test_rows$SE
      # indicate if the change is significant or not. If significantly up then 1 , if significantly down then -1, if not significant then 0
      test_rows$significance <- ifelse(test_rows$zvalue >= zvalue_bound,'significant decrease',   ifelse(test_rows$zvalue <= (-1*zvalue_bound),'significant increase','No significant change'))
      test_rows$relative_diff <- (test_rows$credit_rate - test_rows$control_credit_rate)/test_rows$control_credit_rate


  return(list(output=test_rows))
}



# how to run the function
#aa <- msr_care_contact_data(con=msr, zvalue_bound=1.96,visitor_table='msr_analytics..MSR_VL_1_6372', contact_table ='msr_analytics..MSR_OL_1_6372_contact_level', hopper = 0, control_id = 25895)
#' CARE Contact rate data module
#'
#' Produces a summary table with % shoppers contacting CARE and significance from control
#' @description Produces a summary table with % shoppers contacting CARE and significance from control
#' @return  a list that contains a data frame of control and test care contact metrics and stats information
#' @param con connection name to sql
#' @param zvalue_bound the Z threshold at which we will call the difference significanct
#' @param visitor_table the visitor table name
#' @param contact_table the shopper contact table name - this is the output from CARE contact sproc
#' @param hopper flag to include hoppers in analysis, 0 means exclude hoppers and 1 means include hoppers in the analysis
#' @param control_id the test_sub_id that should be used as the control. All other test_sub_id will be combpared against this
#' @export

msr_care_contact_data <- function(con, zvalue_bound=1.96,visitor_table,
                                 contact_table, hopper, control_id){

  # uncomment for debugging inside the function
  # con=msr; zvalue_bound=1.96;visitor_table='msr_analytics..MSR_VL_1_6372';  contact_table ='msr_analytics..MSR_VL_1_6372_contact_level'; hopper = 0 ; control_id = 25895



  # get the credit combined data from another function
  msr_sql <- msr_sql_contact(visitor_table,contact_table, hopper = 0)
  df <- sqlQuery(channel=con,query=msr_sql)

  # calculation for credit rate
  df$contact_rate <- df$shopper_ct_contacted/ df$shopper_ct

  # seprate out the control and test rows into two different tables
  control_row <- subset(df, test_sub_id == control_id)
  test_rows <- subset(df, test_sub_id != control_id)

  # enter the control values into the test_table
  test_rows$control_shopper_ct <- control_row$shopper_ct
  test_rows$control_shopper_ct_contacted <- control_row$shopper_ct_contacted
  test_rows$control_contact_rate <- control_row$contact_rate
  test_rows$control_name <- control_row$test_sub_name

  # calculate the z metrics and if the test is significant or not
  test_rows$combined_var <- (test_rows$control_shopper_ct_contacted + test_rows$shopper_ct_contacted) / (test_rows$control_shopper_ct + test_rows$shopper_ct )
  test_rows$SE <- sqrt(  (test_rows$combined_var*(1- test_rows$combined_var) / test_rows$shopper_ct )   +    (test_rows$combined_var*(1- test_rows$combined_var) / test_rows$control_shopper_ct )  )
  test_rows$zvalue <-   (test_rows$control_contact_rate  - test_rows$contact_rate)/ test_rows$SE
  # indicate if the change is significant or not. If significantly up then 1 , if significantly down then -1, if not significant then 0
  test_rows$significance <- ifelse(test_rows$zvalue >= zvalue_bound,'significant decrease',   ifelse(test_rows$zvalue <= (-1*zvalue_bound),'significant increase','No significant change'))
  test_rows$relative_diff <- (test_rows$contact_rate - test_rows$control_contact_rate)/test_rows$control_contact_rate

  return(list(output=test_rows))
}

#' CARE graph module
#' @description Produces a graph based on credit or contact rate summary data from data functions
#' @return  prints the graph directly
#' @param credit_data the credit data - the output from credit data functions
#' @param contact_data the contact data - the output from msr_care_contact_data data functions
#' @export
msr_care_graph <- function(credit_data, contact_data){

  # useful for debugging
  # credit_data = credit_data[[1]]; contact_data <- contact_data[[1]]

  # manupilate the data to include control in the graph - by creating a row for control
  credit_control <- data.frame(credit_data[1, c('control_name', "control_credit_rate")], "control")
  credit_test <- credit_data[ , c("test_sub_name",'credit_rate','significance')]
  names(credit_control) <- names(credit_test)
  credit_data <- rbind(credit_control,credit_test)


  contact_control <- data.frame(contact_data[1, c('control_name', "control_contact_rate")], "control")
  contact_test <- contact_data[ , c("test_sub_name",'contact_rate','significance')]
  names(contact_control) <- names(contact_test)
  contact_data <- rbind(contact_control,contact_test)


 # graph part for credit data
 credit_graph <-  ggplot(data = credit_data, aes(x = test_sub_name, y = credit_rate,fill = factor(significance))) +
    geom_bar(position = "dodge", stat = "identity") + theme_bw() + coord_flip() +
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major.y = element_blank()
          ,panel.grid.minor.y = element_blank(),panel.grid.major.x = element_blank() ,panel.grid.minor.x = element_blank()) +
    scale_fill_manual(values = c('significant increase'= "#84A384" ,'No significant change' = "#4C4C4C",'significant decrease' = "#9E5E5E" , 'control' = "#66CCFF")) +
    geom_text(aes(label=percent(round(credit_rate,4)), y = max(credit_rate/2))) + scale_y_continuous(expand = c(0, 0)) +
  ggtitle('% ordering shoppers receiving  CARE credit')

 # graph part for contact data
 contact_graph <-  ggplot(data = contact_data, aes(x = test_sub_name, y = contact_rate,fill = factor(significance))) +
   geom_bar(position = "dodge", stat = "identity") + theme_bw() + coord_flip() +
   theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major.y = element_blank()
         ,panel.grid.minor.y = element_blank(),panel.grid.major.x = element_blank() ,panel.grid.minor.x = element_blank()) +
   scale_fill_manual(values = c('significant increase'= "#84A384" ,'No significant change' = "#4C4C4C",'significant decrease' = "#9E5E5E", 'control' = "#66CCFF" )) +
   geom_text(aes(label=percent(round(contact_rate,4)), y = max(contact_rate/2))) + scale_y_continuous(expand = c(0, 0)) +
   ggtitle('% shoppers contacting CARE')

 multiplot(credit_graph, contact_graph, cols = 2)

}

#SQL aggregate for credit and contact

msr_sql_credit <- function(visitor_table, order_table, credit_table, hopper = 0){
  sql_credit <- paste("
                      select
                      ol.test_sub_name,ol.test_sub_id,
                      count(distinct ol.visitor_id) as shopper_ct,
                      count(distinct cr.visitor_id) as shopper_ct_credited
                      from ", order_table," ol
                      join ", visitor_table," vl on ol.visitor_id = vl.visitor_id
                      left join ", credit_table," cr on cr.visitor_id = ol.visitor_id
                      where vl.control_test <> 'AC' and vl.is_hopper <= ",hopper,"
                      group by ol.test_sub_name, ol.test_sub_id",sep="")

  return(sql_credit)

}

msr_sql_contact <- function(visitor_table, contact_table, hopper = 0){
  sql_contact <- paste("
                       select
                       vl.test_sub_id, vl.test_sub_name,
                       count(distinct vl.shopper_key) as shopper_ct,
                       count(distinct co.shopper_key) as shopper_ct_contacted,
                       count(distinct co.contact_id)  as contact_ct
                       from ", visitor_table," vl
                       left join ", contact_table," co on vl.visitor_id = co.visitor_id
                       where vl.shopper_key is not null
                       and vl.control_test <> 'AC' and vl.is_hopper <= ",hopper,"
                       group by vl.test_sub_id, vl.test_sub_name ",sep="")

  return(sql_contact)

}
