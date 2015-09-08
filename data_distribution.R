#' To produce general data distirbution for a test
#' @description produces graph to illustrate general data distribution
#' @return  nothing
#' @param connection_name sql connection
#' @param table_name visitor table name
#' @param days_interval x axis scale for date axis
#' @export

msr_data_distributions <- function(connection_name, table_name, days_interval = "2 days"){
  #connection_name <- msr ; table_name <- 'msr_analytics..MSR_VL_1_6350'



  # Pull the no of visitors by test_sub_id
  qry <- paste("select test_sub_name ,  count(*) as no_visitors from ",table_name, " group by test_sub_name", sep = "")
  count_by_test_sub_id <- sqlQuery(connection_name,qry)


  p1 <- ggplot(data = count_by_test_sub_id, aes(x = test_sub_name, y = no_visitors)) +
    geom_bar(position = "dodge", stat = "identity", fill = "#66CCFF") + theme_bw() + theme(axis.text.x=element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),axis.ticks.x=element_blank(),panel.grid.major.y = element_blank()
                                                                                           ,panel.grid.minor.y = element_blank(),panel.grid.major.x = element_blank()
                                                                                           ,panel.grid.minor.x = element_blank()) +
    geom_text(aes(label=prettyNum(no_visitors,big.mark=",",scientific=FALSE), y = max(no_visitors/2))) + coord_flip() + ggtitle("# visitor by test name")

  # Pull the no of visitors by is_hopper field
  qry2 <- paste("select is_hopper , case when is_hopper = 1 then 'visitor hopper'
                 when is_hopper = 2 then 'session hopper'
                 when is_hopper = 3 then 'subsession hopper' else 'Not a hopper' end as hopper_class,
                 count(*) as no_visitors from ",table_name, " group by is_hopper , case when is_hopper = 1 then 'visitor hopper'
                 when is_hopper = 2 then 'session hopper'
                when is_hopper = 3 then 'subsession hopper' else 'Not a hopper' end", sep = "")
  hopper_data <- sqlQuery(connection_name,qry2)

  hopper_data$rawcent <- hopper_data$no_visitors/sum(hopper_data$no_visitors)

  hopper_data <- subset(hopper_data, is_hopper > 0)

  p2 <- ggplot(data = hopper_data, aes(x = hopper_class, y = rawcent)) +
    geom_bar(position = "dodge", stat = "identity", fill = "#66CCFF") + theme_bw() + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major.y = element_blank()
                                                                                           ,panel.grid.minor.y = element_blank(),panel.grid.major.x = element_blank()
                                                                                           ,panel.grid.minor.x = element_blank()) +
    geom_text(aes(label=percent(rawcent), y = max(rawcent/2))) + coord_flip() + ggtitle("% visitors flagged as hoppers")



  print(multiplot(p1,p2, cols = 2))


  # visitor count by date
  qry3 <- paste("select test_sub_name, CONVERT(date, touch_date) as touch_date , count(*) as no_visitors from ",table_name, " group by CONVERT(date, touch_date), test_sub_name", sep = "")
  count_by_date <- sqlQuery(connection_name,qry3)

  ## change touch date format and sort the table by touch date
  count_by_date$touch_date <- as.Date(count_by_date$touch_date)
  count_by_date <- count_by_date[order(count_by_date$test_sub_name, count_by_date$touch_date), ]


  ## plot the graph
  ggplot(data = count_by_date, aes(x=touch_date, y = no_visitors, colour = factor(test_sub_name))) + geom_line() + theme_bw() +
    theme(axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),axis.text.x = element_text(size = 10, angle = 90)) + scale_y_continuous(expand = c(0, 0)) +
    scale_x_date(breaks = date_breaks(days_interval), labels = date_format("%m/%d"))+ ggtitle("# visitors by touch date (and test name)")


}

#' multiplot
#'
#' To product subplots
#' @description Produces subplot
#' @return  combined graph

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

