#' To produce general data distirbution for a test
#' @description produces graph to illustrate general data distribution
#' @return  nothing
#' @param connection_name sql connection
#' @param table_name visitor table name
#' @export

msr_data_distributions <- function(connection_name, table_name){
  #connection_name <- msr ; visitor_table_name <- 'msr_analytics..MSR_VL_1_6309'



  # Pull the no of visitors by test_sub_id
  qry <- paste("select test_sub_id , test_sub_name ,  count(*) as no_visitors from ",table_name, " group by test_sub_id , test_sub_name", sep = "")
  count_by_test_sub_id <- sqlQuery(connection_name,qry)


  p1 <- ggplot(data = count_by_test_sub_id, aes(x = test_sub_name, y = no_visitors)) +
    geom_bar(position = "dodge", stat = "identity", fill = "#66CCFF") + theme_bw() + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
    geom_text(aes(label=prettyNum(no_visitors,big.mark=",",scientific=FALSE), y = max(no_visitors/2))) + coord_flip()

  print(p1)

  # Pull the no of visitors by is_hopper field
  qry2 <- paste("select is_hopper ,  count(*) as no_visitors from ",table_name, " group by is_hopper", sep = "")
  hopper_data <- sqlQuery(connection_name,qry2)

  pos = max(hopper_data$no_visitors)/2

  p2 <- ggplot(data = hopper_data, aes(x = is_hopper, y = no_visitors)) +
    geom_bar(position = "dodge", stat = "identity", fill = "#66CCFF") + theme_bw() + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    geom_text(aes(label=prettyNum(no_visitors,big.mark=",",scientific=FALSE), y = max(no_visitors/2)), angle = 90)


  # Pull the no of visitors by is_hopper field
  qry3 <- paste("select exposed_to_test ,  count(*) as no_visitors from ",table_name, " group by  exposed_to_test", sep = "")
  exposed_data <- sqlQuery(connection_name,qry3)

  pos = max(exposed_data$no_visitors)/2

  p3 <-  ggplot(data = exposed_data, aes(x =  factor(exposed_to_test), y = no_visitors)) + xlab("exposed to test") +
    geom_bar(position = "dodge", stat = "identity", fill = "#66CCFF") + theme_bw() + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    geom_text(aes(label=prettyNum(no_visitors,big.mark=",",scientific=FALSE), y = max(no_visitors/2)), angle = 90)


  print(multiplot(p2,p3, cols = 2))


}

#' multiplot
#'
#' To product subplots
#' @description Produces subplot
#' @return  nothing


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

