#' Information about donors to specific emergency
#' 
#' This function queries Financial Tracking Service for donor information.
#' @param emergency.id FTS emergency id (e.g., 15797 for Haiti 2011 earthquake)
#' @return Data frame of donors and amounts
#' @import RJSONIO plyr data.table
#' @export
#' @examples
#' \dontrun{
#' getDonors(15797)
#' }

getDonors <- function(emergency.id) {
  # Define a list of expected columns
  cols <- c("type", "amount")
  
  # Fetch JSON from Financial Tracking Service website
  json_file <- sprintf("http://fts.unocha.org/api/v1/funding.json?emergency=%s&groupby=donor", as.character(emergency.id))
  json_data <- fromJSON(paste(readLines(json_file, warn = F), collapse=""))

  # Convert list with sporadic NULL values to a well-behaving data.frame
  # Solution from http://stackoverflow.com/questions/15793759/convert-r-list-to-dataframe-with-missing-null-elements
  results <- rbind.fill(lapply(json_data$grouping, function(f) {
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
