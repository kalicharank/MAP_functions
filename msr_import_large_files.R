#' MOdule to import large data files into R
#' @description fxn to import large data files into R
#' @return  sql table as a data.frame
#' @export


msr_import_large_tables <- function(connection_name, table_name, primary_column_name, no_of_loops){
#connection_name <- msr ; table_name <- 'msr_analytics..MSR_VL_1_6309' ; primary_column_name <- "visitor_id" ; no_of_loops = 10; i = 1

 dd <- {}

 for(i in  0:(no_of_loops-1)){
    qry <- paste("select * from ",table_name, " where ",primary_column_name,"%",no_of_loops, " = ",i, sep = "")
    Temp <- sqlQuery(connection_name,qry)

    if(is.null(dd))
      { dd <- Temp
      }else {dd <- rbind(dd,Temp) }
  }
 return(dd)
}
