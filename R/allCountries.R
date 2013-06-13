#' Helper function to return a data.table of all ISO codes
#' 
#' This function provides a helpful merge template with all country codes.
#' 
#' @import data.table
#' @return A \code{data.table} of country ISO alpha 2 codes
#' @export
#' 
allCountries <- function() {
  all.countries <- data.table(source_country = getISOs(2))
  setkey(all.countries, "source_country")
  return(all.countries)
}
