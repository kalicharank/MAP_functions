#' Cumulative CR over time graph
#' @description Prints Cumulative CR over time
#' @return  nothing
#' @export

msr_sanity_cumulativeCR_graph <- function(data_table, x_axis_date_breaks)
{
  #helpful during debugging
  # data_table = ccr;  x_axis_date_breaks = x_axis_day_interval


  ##comparing daily CR test cells vs. ctrl in blue
  c <- ggplot(data = data_table, aes(x=dateVariable, y = CR_test, colour = factor(test_sub_name))) + geom_line()+ xlab("touch date") + ylab("Cumulative CR")

  ##adding in the control
  c <- c + geom_line(aes(y = CR_ctrl), col = "blue", size = 1)

  ##scaling and adjusting date format
  c <- c + scale_x_date(breaks = date_breaks(x_axis_date_breaks), labels = date_format("%m/%d")) + scale_y_continuous(labels = percent) + theme_bw() +
       theme(axis.text.x = element_text(size = 10, angle = 90))

  c <- c + ggtitle("Cumulative CR")

  print(c)

  ##looking at % change by test cell as compared to control

  c1 <- ggplot(data = data_table, aes(x=dateVariable, y = perc_lift, colour = factor(test_sub_name))) + geom_line() + xlab("touch date") + ylab("% lift in cumulative CR")
  ##plot points with sig change

  ## only running this step if there is anything sigificant
  if(1 %in% data_table$significant) {

    c_sub <- subset(data_table, significant > 0)
    c1 <- c1 + geom_point(data = c_sub, aes(x=dateVariable, y = perc_lift, colour = factor(test_sub_name)),size = 3)
  }

  ##scaling and adjusting date format
  c1 <- c1 + scale_x_date(breaks = date_breaks(x_axis_date_breaks), labels = date_format("%m/%d")) + theme_bw() + scale_y_continuous(labels = percent)

  c1 <- c1 + ggtitle("% lift in cumulative CR") +  theme(axis.text.x = element_text(size = 10, angle = 90))

  print(c1)

}

