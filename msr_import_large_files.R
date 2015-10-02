#' MOdule to import large data files into R
#' @description fxn to import large data files into R
#' @param connection_name name of the connection to sql
#' @param table_name the table name that needes to be pulled from sql into R
#' @param primary_column_name The primary key for the table. This field should not have any null values
#' @param no_of_loops HOw many chunks should the sql table be broken into.Recommend dividing it into 0.5M blocks.
#' @param add_where_cont Do you want to add additional where clauses when pulling this table
#' @return  sql table as a data.frame
#' @export


msr_import_large_tables <- function(connection_name, table_name, primary_column_name, no_of_loops = 1,add_where_cont = ' '){
#connection_name <- msr ; table_name <- 'msr_analytics..MSR_VL_1_6309' ; primary_column_name <- "visitor_id" ; no_of_loops = 10; i = 1

 dd <- {}

 for(i in  0:(no_of_loops-1)){
    qry <- paste("select * from ",table_name, " where ",primary_column_name,"%",no_of_loops, " = ",i," " , add_where_cont, sep = "")
    Temp <- sqlQuery(connection_name,qry)

    if(is.null(dd))
      { dd <- Temp
      }else {dd <- rbind(dd,Temp) }
  }
 return(dd)
}
