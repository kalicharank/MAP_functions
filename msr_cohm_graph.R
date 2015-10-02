#' Produces summary graphs at cohort level
#'
#' Produces graph by different dimensions
#' @description Takes output data frame from the msr_cohm_data function and produces summary charts for CR, bookings and GM per buyer for each test cell by dimension
#' @param cohort_data_output the output from cohort data module
#' @export

msr_cohm_graph <- function(msr_cohm_data_output_table)
{

  MSR_Cohm_output_table <- msr_cohm_data_output_table

  #bucket the z-scores, to be used for color coding in charts
  MSR_Cohm_output_table$bookings_buyer_chart_value <- ifelse(MSR_Cohm_output_table$z_score_bookings_buyer <= -1.96,1,ifelse(MSR_Cohm_output_table$z_score_bookings_buyer <= -1.65,2,ifelse(MSR_Cohm_output_table$z_score_bookings_buyer >= 1.96,5,ifelse(MSR_Cohm_output_table$z_score_bookings_buyer >= 1.64,4,3))))
  MSR_Cohm_output_table$gm_buyer_chart_value <- ifelse(MSR_Cohm_output_table$z_score_gm_buyer <= -1.96,1,ifelse(MSR_Cohm_output_table$z_score_gm_buyer <= -1.65,2,ifelse(MSR_Cohm_output_table$z_score_gm_buyer >= 1.96,5,ifelse(MSR_Cohm_output_table$z_score_gm_buyer >= 1.64,4,3))))
  MSR_Cohm_output_table$cr_chart_value <- ifelse(MSR_Cohm_output_table$z_score_cr <= -1.96,1,ifelse(MSR_Cohm_output_table$z_score_cr <= -1.65,2,ifelse(MSR_Cohm_output_table$z_score_cr >= 1.96,5,ifelse(MSR_Cohm_output_table$z_score_cr >= 1.64,4,3))))

  #We need to define 2 functions in order to produce the output

  library(proto)
  library(grid)
  library(scales)

  #Define horizontal bar chart geom, because we can't use coordflip() with facet_grid(scale="free"). Without creating this geom we cannot exclude factors with no data (i.e. is_new = "UK" will show up on the chart with no bar)

  #copied exactly from web
  geom_bar_horz <- function (mapping = NULL, data = NULL, stat = "bin", position = "stack", ...) {
    GeomBar_horz$new(mapping = mapping, data = data, stat = stat, position = position, ...)
  }

  GeomBar_horz <- proto(ggplot2:::Geom, {
    objname <- "bar_horz"

    default_stat <- function(.) StatBin
    default_pos <- function(.) PositionStack
    default_aes <- function(.) aes(colour=NA, fill="grey20", size=0.5, linetype=1, weight = 1, alpha = NA)

    required_aes <- c("y")

    reparameterise <- function(., df, params) {
      df$width <- df$width %||%
        params$width %||% (resolution(df$x, FALSE) * 0.9)
      OUT <- transform(df,
                       xmin = pmin(x, 0), xmax = pmax(x, 0),
                       ymin = y - .45, ymax = y + .45, width = NULL
      )
      return(OUT)
    }

    draw_groups <- function(., data, scales, coordinates, ...) {
      GeomRect$draw_groups(data, scales, coordinates, ...)
    }
    guide_geom <- function(.) "polygon"
  })

  #Define multiplot function, so that we can display all of our charts in one plot

  #create a set of charts for bookings per buyer, gm per buyer and cr, for each test cell

  for(test_y in unique(MSR_Cohm_output_table$t_name)){

    #get the id for the test name
    test_id_y <- unique(MSR_Cohm_output_table$t_id[MSR_Cohm_output_table$t_name == test_y])

    #create fitlered table of only this test cell and control, and where p values and z scores exist
    Temp1 <- subset(MSR_Cohm_output_table,t_name == test_y)

    #bookings per buyer chart
    temp_bb_chart <- ggplot(data=Temp1,aes(y=dimension,x=bookings_buyer_diff)) +
      geom_bar_horz(position="identity",stat="identity",aes(fill=factor(bookings_buyer_chart_value))) + #creates the horizontal bars, color based on chart_value
      geom_text(aes(y=dimension,x=0,label=paste("$",round(c_bookings_buyer,digits=2)),size=5)) + #adds the values for control
      scale_fill_manual(values = c("1"="#84A384","2" = "#8E8E8E","3" = "#8E8E8E","4" = "#8E8E8E" ,"5" = "#9E5E5E" )) + #dark red, gray, gray, gray, dark green based on chart_value 1,2,3,4,5
      theme_bw() + #remove gray background
      theme(legend.position = "none",axis.title.y = element_blank(),strip.text.y = element_text(size=10)) + #take away legend, y-axis title, resize cohorts
      xlab("% Change from control") + #label x-axis
      facet_grid(cohort~.,scale="free_y") + #break out chart by cohort, remove values that have no data, such as "is_new = UK"
      ggtitle(paste(test_y,"vs.Control\n Bookings per Buyer\n")) + #add title dynamically based on test and control name
      scale_x_continuous(labels = percent) #make labels %, requires library(scales)

    #gm per buyer chart
    temp_gmb_chart <- ggplot(data=Temp1,aes(y=dimension,x=gm_buyer_diff)) +
      geom_bar_horz(position="identity",stat="identity",aes(fill=factor(gm_buyer_chart_value))) +
      geom_text(aes(y=dimension,x=0,label=paste("$",round(c_gm_buyer,digits=2)),size=5)) +
      scale_fill_manual(values = c("1"="#84A384","2" = "#8E8E8E","3" = "#8E8E8E","4" = "#8E8E8E" ,"5" = "#9E5E5E"  )) +
      theme_bw() +
      theme(legend.position = "none",axis.title.y = element_blank(),axis.text.y=element_blank()) +
      xlab("% Change from control") +
      facet_grid(cohort~.,scale="free_y") +
      ggtitle(paste(test_y,"vs.Control\n GM per Buyer\n")) +
      scale_x_continuous(labels = percent)

    #cr chart
    temp_cr_chart <- ggplot(data=Temp1,aes(y=dimension,x=cr_diff)) +
      geom_bar_horz(position="identity",stat="identity",aes(fill=factor(cr_chart_value))) +
      geom_text(aes(y=dimension,x=0,label=paste(round(c_cr,digits=3)*100,'%'), size = 5)) +
      scale_fill_manual(values = c("1"="#84A384","2" = "#8E8E8E","3" = "#8E8E8E","4" = "#8E8E8E" ,"5" = "#9E5E5E"  )) +
      theme_bw() +
      theme(legend.position = "none",axis.title.y = element_blank(),axis.text.y=element_blank()) +
      xlab("% Change from control") +
      facet_grid(cohort~.,scale="free_y") +
      ggtitle(paste(test_y,"vs.Control\n CR\n")) +
      scale_x_continuous(labels = percent)

    #dynamic SQL equivalent to create object that is the multiplot of the 3 graphs above
    multiplot(temp_bb_chart,temp_gmb_chart,temp_cr_chart, cols = 3)

  }

}









