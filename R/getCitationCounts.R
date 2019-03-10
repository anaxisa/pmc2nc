#' Counts the number of citations for each target ID.
#'
#' `getCitationCounts` counts the number of citations for each target ID in an edge list

#' @details
#' The edge list `e` must have a `Target` column. The number of citations for each
#' `Target` are counted. All citations per `Target` are assumed to be unique.
#' 
#' @param e an edge list, as obtained from `generateEdgeList` 
#' @return A tibble with one column for target PMIDS and one column, 'n', containing the corresponding citation frequencies
#' @seealso \code{\link{generateEdgeList}} for generating an edge list
#' 
#' @examples
#' 
#' # generates an edge list for multiple articles
#' res <- get_pmc_cited_in(c(21876761, 311,29463753))
#' e <- generateEdgeList(res)
#' counts <- getCitationCounts(e)

#' @export
getCitationCounts <- function(e) {
  require(dplyr)
  group_by(e, Target) %>% count()
}
