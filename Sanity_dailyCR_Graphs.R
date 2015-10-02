#' Prints charts for daily CR
#' @description Function to prints graph based on daily CR data frame
#' @return  nothing
#' @export


msr_sanity_dailyCR_graph <- function(data_table, x_axis_date_breaks)
{

  # for debugging: data_table = dcr; x_axis_date_breaks = '3 day'

  ##comparing daily CR test cells vs. ctrl in blue
  g <- ggplot(data = data_table, aes(x=touch_date, y = t_value, colour = factor(t_name))) + geom_line() + xlab("touch date cohort") + ylab("x-day CR") + theme_bw()


  ##adding in the control
  g <- g + geom_line(aes(y = c_value), col = "blue", size = 1)

  ##scaling and adjusting date format
  g <- g + scale_x_date(breaks = date_breaks(x_axis_date_breaks), labels = date_format("%m/%d")) + scale_y_continuous(labels = percent)

  g<- g + ggtitle("X-day CR by touch-date cohorts") +  theme(axis.text.x = element_text(size = 10, angle = 90))

  print(g)

  ##looking at % change by test cell as compared to control
  ##subset of significant values
  g1 <- ggplot(data = data_table, aes(x=touch_date, y = per_change, colour = factor(t_name))) + geom_line() + xlab("touch date cohort") + ylab("% lift in x-day CR")

  ## only running this step if there is anything sigificant
  if(1 %in% data_table$significant) {

    g1_sub <- subset(data_table, significant > 0)
    g1 <- g1 + geom_point(data = g1_sub, aes(x=touch_date, y = per_change, colour = factor(t_name)),size = 3)
  }

  ##scaling and adjusting date format
  g1 <- g1 + scale_x_date(breaks = date_breaks(x_axis_date_breaks), labels = date_format("%m/%d"))

  g1 <- g1 + ggtitle("% lift in x-day CR by touch-date cohorts") + theme_bw()  +  theme(axis.text.x = element_text(size = 10, angle = 90))


  print(g1)

}






