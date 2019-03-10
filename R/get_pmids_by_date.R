#' Query an pmids date from DB
#'
#' `get_pmids_by_date` query edgelist_date table for PMIDs that match.

#' @details
#' `get_pmids_by_date` queries the edgelist_date table for target PMIDs, that optionally have 
#' been updated since 'lastUpdate'. The user can specify the 'tableName', but this table must have
#' column names 'Target' and 'lastUpdated'. An error will be generated if 'tableName' does not exist.
#'
#' @param conMysql a MySQL connection
#' @param pmids a vector of pmids to look up
#' @param lastUpdate string of the date used for comparison, in YYYY-MM-DD format
#' @param tableName string with the name of the table that stores the target pmids and dates.
#' @param mysqlOperator string for comparison operators '= , >, >=, <, <=', defaulting to ">" 
#' @return a data.frame with one column for Target and one column for lastUpdated
#' 
#' @examples
#' 
#' @export
get_pmids_by_date <- function(conMysql, pmids, lastUpdate, tableName = "Edgelist_date"){
  
  # format PMIDS
  x <- paste0(pmids, collapse = ",")
  
  # create qury for PMIDS only
  qry <- paste0("SELECT * FROM ",tableName," WHERE Target IN (", x, ")")
                
  # add date criteria if specified
  if (!is.null(lastUpdate)) {
    lastUpdate <- isDate(lastUpdate)
    qry <-paste0(qry," AND lastUpdated >= '",lastUpdate,"';")
  }
  print("get_pmids_by_date: Querying now.")
  dbGetQuery(conMysql, qry)
}
