#' To produce general data distirbution for a test
#' @description produces graph to illustrate general data distribution
#' @return  nothing
#' @param connection_name sql connection
#' @param table_name visitor table name
#' @param days_interval x axis scale for date axis
#' @export
msr_data_distributions <- function(connection_name, table_name, test_type_id = 1, days_interval = "1 days"){
  #connection_name <- msr ; table_name <- 'msr_analytics..MSR_VL_2_1' ; test_type_id = 2; days_interval = "1 days"



  # Pull the no of visitors by test_sub_id
  qry <- paste("select test_sub_name , control_test, count(*) as no_visitors from ",table_name, " where is_hopper = 0  group by test_sub_name, control_test", sep = "")
  count_by_test_sub_id <- sqlQuery(connection_name,qry)


  p1 <- ggplot(data = count_by_test_sub_id, aes(x = test_sub_name, y = no_visitors)) +
    geom_bar(position = "dodge", stat = "identity", fill = "#66CCFF") + theme_bw() + theme(axis.text.x=element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),axis.ticks.x=element_blank(),panel.grid.major.y = element_blank()
                                                                                           ,panel.grid.minor.y = element_blank(),panel.grid.major.x = element_blank()
                                                                                           ,panel.grid.minor.x = element_blank()) +
    geom_text(aes(label=prettyNum(no_visitors,big.mark=",",scientific=FALSE), y = max(no_visitors/2))) + coord_flip() + ggtitle("# visitor by test name")

  if(test_type_id == 1 ){
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

  }

  if(test_type_id == 2 ){
    # Pull the no of visitors by is_hopper field
    qry2 <- paste("select case when exposed_to_test = 1 then 'exposed to test' else 'not exposed to test' end as exposed_class,
                count(*) as no_visitors from ",table_name, " group by case when exposed_to_test = 1 then 'exposed to test' else 'not exposed to test' end", sep = "")
    exposed_to_test <- sqlQuery(connection_name,qry2)

    exposed_to_test$rawcent <- exposed_to_test$no_visitors/sum(exposed_to_test$no_visitors)


    p2 <- ggplot(data = exposed_to_test, aes(x = exposed_class, y = rawcent)) +
      geom_bar(position = "dodge", stat = "identity", fill = "#66CCFF") +
      theme_bw() + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major.y = element_blank()
      ,panel.grid.minor.y = element_blank(),panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
      geom_text(aes(label=percent(rawcent), y = max(rawcent/2))) + coord_flip() + ggtitle("% visitors flagged as exposed_to_test")



    print(multiplot(p1,p2, cols = 2))

  }


  # visitor count by date
  qry3 <- paste("select test_sub_name, CONVERT(date, touch_date) as touch_date , count(*) as no_visitors from ",table_name, " group by CONVERT(date, touch_date), test_sub_name", sep = "")
  count_by_date <- sqlQuery(connection_name,qry3)

  ## change touch date format and sort the table by touch date
  count_by_date$touch_date <- as.Date(count_by_date$touch_date)
  count_by_date <- count_by_date[order(count_by_date$test_sub_name, count_by_date$touch_date), ]
  max_count_by_date <- max(count_by_date$no_visitors)

  ## no of dates
  if (length(unique(count_by_date$touch_date)) > 1){

  ## plot the graph
  p3 <- ggplot(data = count_by_date, aes(x=touch_date, y = no_visitors, colour = factor(test_sub_name))) + geom_line() + theme_bw() +
    theme(axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),axis.text.x = element_text(size = 10, angle = 90)) + ylim(0, max_count_by_date) +
    scale_x_date(breaks = date_breaks(days_interval), labels = date_format("%m/%d"))+ ggtitle("# visitors by touch date (and test name)")

  print(p3)
  }

  # population by different dimensions

  ## get the data from sql
  qry4 <- paste("
                select test_sub_name, 'is_new' as dimension, cast(is_new as varchar(30)) as value, count(*) as no_visitor
                from ", table_name , "
                where control_test <> 'AC' and is_hopper = 0
                group by test_sub_name,  cast(is_new as varchar(30))
                union all
                select test_sub_name, 'channel_group' as dimension, cast(channel_group as varchar(30)) , count(*)
                from ", table_name , "
                where control_test <> 'AC' and is_hopper = 0
                group by test_sub_name,  cast(channel_group as varchar(30))
                union all
                select test_sub_name, 'region' as dimension, cast(region as varchar(30)) , count(*)
                from ", table_name , "
                where control_test <> 'AC' and is_hopper = 0
                group by test_sub_name,  cast(region as varchar(30))
                union all
                select test_sub_name, 'initial_device_type' as dimension, cast(initial_device_type as varchar(30)) , count(*)
                from ", table_name , "
                where control_test <> 'AC' and is_hopper = 0
                group by test_sub_name,  cast(initial_device_type as varchar(30)) ",sep="")



  ## add more fields
  count_by_dim_test <- sqlQuery(connection_name,qry4)

  count_by_dim_test$combined_name <- paste(count_by_dim_test$dimension,' : ',count_by_dim_test$value,sep="")

  ## summarize by dimension
  count_by_dim <- count_by_dim_test %>% group_by(combined_name, value, dimension) %>% summarise(no_visitors = sum(no_visitor))
  total_count <- sum(subset(count_by_test_sub_id,control_test != 'AC')$no_visitors)
  count_by_dim$cent <- round(count_by_dim$no_visitors/total_count,3)


  ## Sort the table and add an index number
  count_by_dim <- count_by_dim[order(count_by_dim$dimension,  - count_by_dim$cent), ]
  count_by_dim$index <- as.numeric(rownames(count_by_dim))


  ## plot graph

  p4 <- ggplot(data = count_by_dim, aes(x = reorder(combined_name,index), y = cent,fill = dimension)) +
    geom_bar(position = "dodge", stat = "identity") + theme_bw() +
    theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major.y = element_blank()
          ,panel.grid.minor.y = element_blank(),panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(), axis.text.x = element_text(size = 10, angle = 90)) +
    ggtitle("% visitor distribution by dimensions") + geom_text(aes(label=percent(cent), y = 0.13), angle = 90)

  print(p4)

  # is there bias in the data distribution between each test_sub_name

  ## we only care about values which has atleast 1% distribution within the dimensions
  count_by_dim <- subset(count_by_dim, cent > 0.01)

  ##  no_visitor is count by test name and dimension_value, no_visitors is count by dimension_value
  even_distribution <- merge(x= count_by_dim_test ,y = count_by_dim[, c('value', 'dimension','no_visitors','cent')], by = c('value','dimension'))
  even_distribution$centeven <- round(even_distribution$no_visitor/even_distribution$no_visitors,4)

  ## Sort by dimension and test_sub_name
  even_distribution <- even_distribution[order(even_distribution$dimension, - even_distribution$cent,  even_distribution$test_sub_name), ]

  ## Breaks in the Y axis
  seq <- c(seq(0,1,1/length(unique(even_distribution$test_sub_name))))

  p5 <- ggplot(data = even_distribution, aes(x = combined_name, y = centeven, fill = test_sub_name)) +
    geom_bar(stat = "identity") + theme_bw() +
    theme(axis.title.x = element_blank(),axis.title.y = element_blank()
          ,panel.grid.minor.y = element_blank(),panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(), axis.text.x = element_text(size = 10, angle = 90)) +
    ggtitle("% visitor distribution by test and dimension value ") + scale_y_continuous(labels = percent, breaks = seq )

  print(p5)
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

