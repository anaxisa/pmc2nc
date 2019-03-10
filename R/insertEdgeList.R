#' Insert edge lists to mysql database.
#'
#' `insertEdgeList` insert the edge list from the results of `generateEdgeList`

#' @details
#' `edgeList` must come from the result of generateEdgeList. The edge list will be stored in the
#' specified table, which must include column names 'Source' and 'Target'. The table will be
#' created if it does not exist. Edges in the table with matching targets are  first 
#' deleted. The assumption is that the 'edgeList' is a complete up-to-date edge list, and
#' previous records are removed.
#'
#' @param conMysql a MySQL connection
#' @param edgeList an edge list, as obtained from `generateEdgeList`
#' @param tableName string with the name of the table used to store the edge list, defaulting to 'EdgeList'
#' @param dateTableName string with the name of the table used to store the dates for pmids, defaulting to 'EdgeList_date'
#' @return a scalar numeric that specifies the number of rows
#'         affected by the statement. An error is raised when issuing a statement over a closed
#'         or invalid connection, if the syntax of the statement is invalid.
#' @seealso \code{\link{generateEdgeList}} for obtaining edgeList citation results
#'
#' @examples
#'
#' # generate an edge list for a single article and then insert the data to the table called "edgelist"
#' res1 <- get_pmc_cited_in(21876761)
#' e2 <- generateEdgeList(res1)
#' insertEdgeList(conMysql, e2)


#' @export

insertEdgeList <-
  function(conMysql, edgeList, tableName = "EdgeList", dateTableName = "EdgeList_date"){
    if (length(edgeList) != 0){
      print("insertEdgeList: Inserting edge list into database now.")
      
      # To avoid hitting any memory limits, only process 1 million rows at a time
      rowcount <-nrow(edgeList)
      if( rowcount > 1000000){
        n <- ceiling(rowcount / 1000000)
        edgeList <- split(edgeList, 1:n)
        
        # this will insert in batches
        for(x in edgeList){
          insertEdgeList1(conMysql, x, tableName)
        }
      }else{
        # this will insert in all in one go
        insertEdgeList1(conMysql, edgeList, tableName)
      }
    }else{
      print("insertEdgeList: Input is empty. Nothing to insert to database.")
      res <- NULL
    }
    
}


insertEdgeList1 <- function(conMysql, edgeList, tableName){
  # delete old edge list so there is no duplicates
  target_delete <- unique(edgeList$Target)
  x <- paste0(target_delete, collapse = ",")
  delete_paste <- paste0("DELETE FROM ",tableName," where Target in (",x,");")
  dbExecute(conMysql, delete_paste)
  
  # concatenate the two columns of edgeList so I can insert the whole thing in one insert statement
  edge_list_paste <- paste0("(" , edgeList$Source , "," , edgeList$Target, ")", collapse = ",")

  # Insert statement for MySQL into tableName
  qry <- paste0("INSERT INTO ",tableName," Values ",edge_list_paste,";")

  # execute the insert statement
  dbExecute(conMysql, qry)
}


