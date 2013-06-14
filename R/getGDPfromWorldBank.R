#' Retrieve GDP data from World Bank
#' 
#' This function gets the GDP data for a given country indicated in present-day US dollars.
#' @export
#' @import RJSONIO data.table plyr
#' @param iso.code ISO alpha 2 code for the country
#' @param start.year Optional start year (default is 2006)
#' @param end.year Optional end year (default is 2012)
#' @return A data table
#' 
getGDPfromWorldBank <- function(iso.code, start.year = 2006, end.year = 2012) {
  iso.code <- iso2(iso.code)
  # Define a list of expected columns
  cols <- c("indicator", "country", "decimal", "date", "value")
  
  # Fetch JSON from Financial Tracking Service website
  json_file <- sprintf("http://api.worldbank.org/countries/%s/indicators/NY.GDP.MKTP.CD?date=%i:%i&format=json", iso.code, start.year, end.year)
  json_data <- try(fromJSON(paste(readLines(json_file, warn = F), collapse="")), silent = T)
  if (class(json_data) == "try-error") {
    results <- data.table(country = iso.code, year = seq(start.year,end.year), GDP = NA)
    return(results)
  }
  
  # Convert list with sporadic NULL values to a well-behaving data.frame
  # Solution from http://stackoverflow.com/questions/15793759/convert-r-list-to-dataframe-with-missing-null-elements
  results <- rbind.fill(lapply(json_data[[2]], function(f) {
    as.data.frame(Filter(Negate(is.null), f))
  }))
  results <- data.table(subset(results, country == iso.code))
  
  # Fill in missing columns based on list of expected column names
  # Note: Factors will get converted to strings
  #results <- insertMissingColumns(results, cols)
  
  results <- results[, list(country, year = date, GDP = value)]
  # Return a regular data.frame
  return(results)
}
