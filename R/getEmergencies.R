#' Information about emergencies in a country
#' 
#' This function queries Financial Tracking Service for emergency information.
#' @param country Three-letter ISO-alpha3 code of the country
#' @return Data frame of emergencies
#' @import RJSONIO, plyr
#' @export
#' @examples
#' \dontrun{
#' getEmergencies("HTI")}

getEmergencies <- function(country) {
  # Define a list of expected columns
  cols <- c("id", "title", "year", "glideid", "type", "country", "funding", "pledges")
  
  # Fetch JSON from Financial Tracking Service website
  json_file <- sprintf("http://fts.unocha.org/api/v1/Emergency/country/%s.json", country)
  json_data <- fromJSON(paste(readLines(json_file, warn = F), collapse=""))

  # Convert list with sporadic NULL values to a well-behaving data.frame
  # Solution from http://stackoverflow.com/questions/15793759/convert-r-list-to-dataframe-with-missing-null-elements
  results <- rbind.fill(lapply(json_data, function(f) {
    as.data.frame(Filter(Negate(is.null), f))
  }))
  
  if (is.null(results)) {
    return(NULL)  # Country had no entries
  }
  
  # Fill in missing columns based on list of expected column names
  # Note: Factors will get converted to strings
  results <- insertMissingColumns(results, cols)
  
  # Return a regular data.frame
  return(results)
  
}
