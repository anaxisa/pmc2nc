#' Retrieve edge list from database
#'
#' retrieves an edge list from the database with the provided list of target PMIDs.

#' @details
#' `get_edge_list_db` will retrieve an edge list from database with the provided list of target PMIDs.
#' An error is generated if the table does not exist in the database.
#'
#' @param conMysql a MySQL connection
#' @param pmids list of target pmids used to search database.
#' @param tableName string with the name of the table used to store the edge list, defaulting to 'EdgeList'
#' @return returns the edge list as a data.frame
#' 
#' @seealso \code{\link{create_edge_list_table}} for edge list table structure
#' 
#' @examples
#' # This will look for PMIDs in 'x' from the DB and get the Source and Target
#' x <- c(21876761, 311, 29463753, 21876726)
#' res <- get_edge_list_db(conMysql, x)


get_edge_list_db <- 
  function(conMysql, pmids, tableName = "EdgeList"){
    # This will search if tableName exist in database
    qry <- paste0("show tables like '",tableName,"';")
    res <- dbGetQuery(conMysql, qry)
    
    # If the tableexists, check if pmids argument actually have values.
    if (length(res[[1]]) == 1){
      
      if (length(pmids) != 0){
      
        print("get_edge_list_db: Querying edge list from database.")
        
        # format list of pmids for mysql query
        pmids <- paste0(pmids, collapse = ",")
        
        # SELECT * FROM edgelist WHERE Target in (21876761, 311, 29463753, 21876726)
        qry <- paste0("SELECT * FROM ",tableName," WHERE Target in (",pmids,");")
        res <- dbGetQuery(conMysql, qry)
      }else{
        stop("PMID input is empty. Cannot search database.")
      }
    }else{
      stop("get_edge_list_db: Table does not exist in database.")
    }
}
