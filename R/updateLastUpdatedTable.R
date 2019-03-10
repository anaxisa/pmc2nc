#' Inserts Target and current date into MySQL database
#'
#' `updateLastUpdatedTable` inserts targetIDs and the current date into the specified MySQL table

#' @details
#' The table will be created if it does not exist, with columns 'Target' and 'lastUpdated'
#' Targets form 'targetIDs' that are in the table will be removed. 
#' 
#' @param conMysql a MySQL connection
#' @param targetIDs the target IDs to add to the table
#' @param tableName string with the name of the table used to store update dates, defaulting to 'EdgeList_date'#' 
#' @return a scalar numeric that specifies the number of rows
#'         affected by the statement. An error is raised when issuing a statement over a closed
#'         or invalid connection, if the syntax of the statement is invalid.
#' @seealso \code{\link{generateEdgeList}} for obtaining edgeList citation results
#'
#' @export
updateLastUpdatedTable <- function(conMysql, targetIDs, tableName = "EdgeList_date"){
  
  # create table if it does not exist
  create_date_table(conMysql, tableName, targetName)
  
  targetIDs <- unique(targetIDs)
  
  # delete old dates so there are no duplicates
  x <- paste0(targetIDs, collapse = ",")
  delete_paste <- paste0("DELETE FROM ",tableName," where Target in (",x,");")
  dbExecute(conMysql, delete_paste)
  
  # concatenate the two columns of edgeList so I can insert the whole thing in one insert statement
  date_paste <- paste0("(" , targetIDs , ",\'" , Sys.Date(), "\')", collapse = ",")

  # Insert statement for MySQL into tableName
  qry <- paste0("INSERT INTO ",tableName," Values ",date_paste,";")

  # execute the insert statement
  dbExecute(conMysql, qry)
}

