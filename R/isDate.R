#' isDate will format date input to work with MySQL
#'
#' `isDate` will try to format an input date into a format recognizable by MySQL, i.e., YYYY-MM-DD. 

#' @details
#' `isDate` will format the input as YYYY-MM-DD and will generate an error if the date cannot be formatted.
#'  The year should be specified as YYYY and must be >= 1900.
#' @param mydate string to be formated.
#' @return isDate() returns the date in the format of YYYY-MM-DD. 
#'
#' @examples
#' 
#' # This will print 2001-01-09"
#' date <- "2001-1-9"
#' isDate(2001-1-9)
#'
#' # This will give an error
#' date <- "209-01-9999"
#' isDate("209-01-9999")
#' 

#' @export
isDate <- function(mydate) {
  
  err <- "date format must be YYYY-MM-DD, with year >= 1900"
  
  if (is.null(mydate)) {
    stop(err)
  }
  
  # This is the date format that matches MySQL. Y = year with 4 digits, m = month with 2 digits, d = day with 2 digits
  date.format = "%Y-%m-%d"
    
  # This will convert mydate into the corresponding date format and will return NA if it fails.
  res <- as.Date(mydate, date.format)
  if (is.na(res)) {
    stop(err)    
  }
    
  # confirm that mydate is in correct format
  s <- strsplit(mydate, "[/-]")
  if (as.integer(s[[1]][1])<1900) {
      stop(err)
  }
  
  res  
}