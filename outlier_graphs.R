#' Produces outlier graph based on output from outlier data functions
#' @description Prints graph to help pick the outlier threshold
#' @return  nothing
#' @export

msr_outlier_graphs <- function(data_table, gm_table )
{
  # useful for debugging data_table = bkg[[1]]; frequency_table = bkg[[2]]; gm_table = gm[[3]]

  # define values for the chart
  seq <- c(seq(0.96,1,0.0025)) # X axis labels
  metric <- unique(data_table$metric)

  if(metric == "gm_per_buyer"){
    ## Show where to select the lower cutoff point for GM threshold
    d <- ggplot(data = gm_table, aes(x = quantile_point, y = amount)) + geom_line() + theme_bw()
    d <- d +  xlab("cutoff %") + ylab(paste("Min ",metric)) + scale_x_continuous(labels = percent,breaks=c(seq(0,0.002,0.0001))) + ggtitle("Min gm_per_buyer @ different cutoff points")
    print(d)
  }


  ## showing the max gm/bookings per buyer @ differnt cutoff points
  d1 <- ggplot(data = data_table, aes(x=cutoff_perc_max, y = cutoff_value_max)) + geom_line() + xlab("% cutoff") + ylab(paste('max ',metric))
  d1 <- d1 + ggtitle(paste('max', metric, " @ different cutoff points"))  + theme_bw() +  scale_x_continuous(labels = percent,breaks=seq)
  print(d1)


  ## $/buyer  @ differnt cutoff points (by test cells)
  d2 <- ggplot(data = data_table, aes(x=cutoff_perc_max, y = t_value, colour = factor(t_name))) + geom_line()+ xlab("cutoff %") + ylab(metric)
  ##adding in the control
  d2 <- d2 + geom_line(aes(y = c_value), col = "blue", size = 1, linetype = 'F1')
  d2 <- d2 + ggtitle(paste(metric, "  @ different cuffoff points"))  + theme_bw() +  scale_x_continuous(labels = percent,breaks=seq)
  print(d2)

  ## No of ordering visitors by different $ bucket - line plot
  # ggplot(data = frequency_table, aes(x=bucket, y = log(no_of_ordering_visitors), colour = factor(test_sub_name))) + geom_line()+ xlab("$ bucket") + ylab('no of ordering visitors') + theme_bw()


  ##looking at % change by test cell as compared to control

  ##subset of significant values

  d3 <- ggplot(data = data_table, aes(x=cutoff_perc_max, y = per_change, colour = factor(t_name))) + geom_line() + xlab("cutoff %") + ylab(paste("% change in ", metric))

  ## only running this step if there is anything sigificant
  if(1 %in% data_table$significant) {

    d3_sub <- subset(data_table, significant > 0)
    d3 <- d3 + geom_point(data = d3_sub,size = 3)
  }
  d3 <- d3 + ggtitle(paste("% change in ", metric, " (Test vs. Control) @ different cufoff points")) + theme_bw() +  scale_x_continuous(labels = percent,breaks=seq)+  scale_y_continuous(labels = percent)
  print(d3)

  ## how many visitors were flattned at different cutoff points
  d4 <- ggplot(data = data_table , aes(x=cutoff_perc_max)) + geom_line(aes(y = t_visitors_flattened, colour = factor(t_name))) +
    geom_line(aes(y = c_visitors_flattened, colour = factor(c_name)), linetype = 'dashed') + theme_bw() +  scale_x_continuous(labels = percent,breaks=seq) +
    ggtitle("# of visitor capped @ different cutoff % by test cell")+ xlab("cutoff %") + ylab("# of visitor's capped")
  print(d4)
  # return(list(d,d1,d2,d3,d4))


}
