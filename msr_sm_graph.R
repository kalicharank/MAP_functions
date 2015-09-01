#' Produces summary graph based on output from summary module
#'
#' Takes the data output from summary module and prints a graph
#' @description Produces sumary graph
#' @return  a graph
#' @param summary_data_output
#' @export


msr_sm_graph <- function(graph_data , only_for_test_sub_id = 0)
{



  if(only_for_test_sub_id != 0)
  {graph_data <- subset(graph_data,t_id == only_for_test_sub_id)}

  # Form a loop, to print one graph for each test value
  for(TestNum_x in unique(graph_data$t_id)){

    graph_data_subset <- subset(graph_data, t_id == TestNum_x)



    # Tries to define the boundry,axis tick mark and other formatting details for the chart
    gmin <- min(graph_data_subset$low95)
    gmax <- max(graph_data_subset$high95)
    grange <- abs(gmax - gmin)
    # gbreak <- round(grange/7, digits = 3)
    # axis_seq <- c(rev(seq(0,gmin,-gbreak)),seq(gbreak,gmax,gbreak) ) # not used right now
    Limits <- c((gmin - grange*0.3),(gmax + grange*0.15) )
    graph_data_subset$axis_min <- (gmin - grange*0.3)
    graph_data_subset$axis_max <- (gmax + grange*0.15)
    # Size of the text within the chart
    txt_size <- 4

    # The actual chart
    # Bar and the boxes + flip the coordinates
    chart <- ggplot(graph_data_subset, aes(x=metric,ymin = low95, ymax = high95, lower = low95, upper = high95, middle = relative_diff)) +
      geom_boxplot(stat = 'identity', aes(fill = factor(chart_value))) + coord_flip() +
      geom_crossbar(aes(x= metric, y = low80)) +
      geom_crossbar(aes(x= metric, y = high80)) +
      # color of the boxes based on significance
      scale_fill_manual(values = c("1"= "#84A384","2" = "#84A384" ,"3" = "#4C4C4C","4" = "#9E5E5E" ,"5" = "#9E5E5E" )) +
      # Labels
      xlab('Metric') +
      ylab(paste(' % Change from control ',toString(unique(graph_data_subset$c_id)), ' Vs. Test ',toString(unique(graph_data_subset$t_id)),' ', unique(graph_data_subset$t_name),sep = "")) +
      # white backbround
      theme_bw() +
      # grid liles and text size
      theme(panel.grid.major.y = element_blank()
            ,panel.grid.minor.y = element_blank()
            , panel.grid.major.x = element_line(color = "#4C4C4C", linetype = "dashed")
            , axis.text.x = element_text(size = 15)
            , axis.text.y = element_text(size = 15, angle = 0)
            ,text = element_text(size=10)) +
      # no legeng
      theme(legend.position="none") +
      # Print control and test mean values
      geom_text(aes(x=metric,y=relative_diff+0.0005,label = round(t_value, digits = 2)), vjust = 0.9, angle = 90, color = "White", size = txt_size ) +
      geom_text(aes(x=metric,y= axis_min,label = round(c_value, digits = 2)),vjust = 0.5,hjust = -0.1, color = "Blue",  size = txt_size) +
      # print 95% and 90% boundries
      geom_text(aes(x=metric,y= low95,label = paste(round(low95*100, digits = 2),'%',sep = '')),  vjust = -0.6, angle = 90,  size = txt_size) +
      geom_text(aes(x=metric,y= high95,label = paste(round(high95*100, digits = 2),'%',sep = '')), vjust = 1.2, angle = 90,  size = txt_size) +
      geom_text(aes(x=metric,y= low80,label = paste(round(low80*100, digits = 2),'%',sep = '')),  vjust = -0.6, angle = 90, color = "White" , size = txt_size ) +
      geom_text(aes(x=metric,y= high80,label = paste(round(high80*100, digits = 2),'%',sep = '')),  vjust = 1.2, angle = 90, color = "White" ,  size = txt_size ) +

      # Relative diff
      geom_text(aes(x=metric,y= relative_diff,label = paste(round( relative_diff*100, digits = 2),'%',sep = '')),  vjust = -0.6, angle = 90, color = "White" ,  size = txt_size ) +
      # is this one tail or two tail measurement
      geom_text(aes(x=metric,y= axis_max,label = tail),  vjust = 1, angle = 90,  size = txt_size) +
      # axis limits and show xaxis as %
      scale_y_continuous(labels = percent, limits = Limits)

    print(chart)


  }

}
