#' Finds "cited in" articles from PMC.
#'
#' `get_pmc_cited_in` takes a vector of article PMIDs and returns elink results containing
#' the PMIDs of articles that cite each of the given articles.
#' 
#' @details
#' Finds articles that cite the provided set of articles. This
#' function is a wrapper for the `entrez_link` function from
#' `rentrez`. If more than 1 PMID is specified, requests are made in batches, ensuring that
#' no more than 200 pmids are queried at a time, and that no
#' more than 3 requests are made per second. If an Entrez Key
#' is set, this function allows for 10 requests per second.
#' Entrez keys can be set using `set_entrez_key(key)`.
#' 
#' @param pmids a vector of PMIDs look-up 
#' @param batchSize the batch size to use
#' @return For looking up one PMID or one batch, an elink object; for multiple batches, a list of 
#'         of elink list objects are returned, i.e., the \eqn{i^{th}} element of the list is an elink list 
#'         object for the \eqn{i^{th}} batch.
#'
#' @examples
#' res <- get_pmc_cited_in(c(21876726,21876761))

#' @export
get_pmc_cited_in <-
function(pmids, batchSize = 200) {
  
  if (batchSize <= 0 || batchSize > 200) {
    stop("Batch size must be betwen 1 and 200")
  } 
  
  n <- length(pmids)

  # if 1 batch, return the results
  if (n <= batchSize) {
    by_id = TRUE
    if (n == 1) {
        by_id <- FALSE
    }
    res <- entrez_link(dbfrom = "pubmed",  id = pmids, linkname = "pubmed_pubmed_citedin", by_id = by_id)
    
    if (n == 1) {
      res <- list(res)
    }
  
    names(res) <- pmids
    
    return(res)
  } 

  # otherwise, process in batches

  wait <- 0.34 # no more than 3 requests per second
  if (Sys.getenv("ENTREZ_KEY") != "") {
    wait <- 0.11 # no more than 10 requests per second
  }
  num_batches <- ceiling(n/batchSize)
  res <- vector("list", num_batches)
  
  beg <- 1
  end <- min(batchSize, n)
  
  pb <- progress_bar$new(total = num_batches)
  
  withProgress(message = 'Getting Citation Information.', detail = 'This may take a while...', value = 0, {
  for (i in 1:num_batches) {
    currIDs <- pmids[beg:end]
    pb$tick()

    incProgress(1 / num_batches)
    
    by_id <- TRUE
    if (length(currIDs) == 1) {
        by_id <- FALSE
    }

    res[[i]] <- entrez_link(dbfrom = "pubmed",  id = currIDs, linkname = "pubmed_pubmed_citedin", by_id = by_id)
    
    if (!by_id) {
        res[[i]] <- list(res[[i]])
    }
    names(res[[i]]) <- currIDs
    beg = end +1
    end = min(end + batchSize, n)
    
    Sys.sleep(wait)
  }
  })
  res
}
