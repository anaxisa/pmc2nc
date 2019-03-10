#' Retrieve an edge list from the provided PMIDs
#'
#' `retrieveEdgeList` Retrieves a citation network edge list for a set of PMIDs

#' @details
#' This function works as follows:
#' 
#' 1. If a MySQL connection is not specified, retreive citation information from NCBI.
#' 
#' 2. If a MySQL connection is specified and 'lastUpdate' is not NULL, then get the edge 
#'    list from the database, for targets that have been updated after 'lastUpdate'. 
#'    If 'lastUpdate' is NULL, then all target PMIDS in the database are used. For
#'    other target PMIDs, get citation information from NCBI.
#'    
#' If a MySQL connection is specified, the updated edge list and target PMIDs are stored
#' in the database. Note that if a target PMID is updated, previous records with that
#' target PMID are deleted from the database.
#'    

#' @param pmids a vector of PMIDs look-up.
#' @param batchSize the batch size to use for NCBI look-ups.
#' @param conMysql a MySQL connection 
#' @param lastUpdate date string, in YYYY-MM-DD format, to use a threshold for updating target PMIDs in the database
#' @return An edge list (data.frame) with one column for Target PMIDS and one column for Source PMIDS.
#' 
#' 
#' @seealso \code{\link{get_pmc_cited_in}} and \code{\link{generateEdgeList}} for 
#' obtaining edge_list citation results from NCBI
#'
#' @examples
#' pmid <- 21876761
#' pmids1 <- c(21876761, 311, 29463753, 21876726)
#' 
#' # This will create tables and insert edge list from one pmid "21876761"
#' # This gets everything from NCBI if there is no database
#' res1 <- retrieveEdgeList(pmid, conMysql = con_mysql)
#' 
#' # This will not create new tables or insert edge list. This will just take everything from DB.
#' # res2 == res1
#' res2 <- retrieveEdgeList(pmid, conMysql = con_mysql)
#'
#' @export

retrieveEdgeList <- function(pmids, batchSize = 200, conMysql = NULL, lastUpdate = NULL){

    e1 <- NULL
    e2 <- NULL
    
    if(!is.null(conMysql)){
    
      # create date table if it does not exist 
      create_date_table(conMysql)
      create_edge_list_table(conMysql)
    
      # determine which PMIDs to get from database and which to get from NCBI
      pmids_db <- get_pmids_by_date(conMysql, pmids, lastUpdate)$Target
      
      if (length(pmids_db) > 0) {
        e2 <- get_edge_list_db(conMysql, pmids_db)
      }
      
      pmids <- setdiff(pmids, pmids_db)
      
  }
  
  if (length(pmids) > 0) {
    res1 <- get_pmc_cited_in(pmids, batchSize)
    e1 <- generateEdgeList(res1)
  }
  
  if (!is.null(conMysql) && length(pmids) > 0) {
    # update the database
    insertEdgeList(conMysql, e1)
    updateLastUpdatedTable(conMysql, pmids)
  }
  combineEdgeList(e1, e2)
}
