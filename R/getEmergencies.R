#' Information about emergencies in a country
#' 
#' This function queries Financial Tracking Service for emergency information.
#' @param country Three-letter ISO-alpha3 code of the country
#' @return Data frame of emergencies
#' @import RJSONIO plyr data.table
#' @export
#' @examples
#' \dontrun{
#' getEmergencies("HTI")
#' }

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

getEmergencyByID <- function(ids) {
  id <- as.numeric(ids)
  if (any(is.na(id))) {
    stop("Please provide numeric input.")
  } else {
    id <- paste(id, collapse = ",")
    sql <- sprintf("SELECT * FROM emergencies WHERE id IN (%s)", id)
    return(magicSQL(sql, "cpw_Crises"))
  }
}

#' Emergencies in a country during a certain year/month
#' 
#' Retrieves emergency information from \code{cpw_meta}.
#' @param country Two- or three-letter ISO-alpha code of the country or a country name.
#' @return Data frame of emergencies (usually just one; not checked)
#' @import magic
#' @export
#' @examples
#' \dontrun{
#' getEmergency("HTI", 2010, 1)
#' }

getEmergency <- function(country, year, month, where.statement = NULL) {
  iso.code <- iso2(country)
  and.where <- addWHERE(where.statement)
  
  sql <- sprintf("SELECT id FROM emergencies WHERE ISOalpha2 = '%s' AND year = %i AND month = %i%s", 
                 iso.code, year, month, and.where)
  
  ids <- magicSQL(sql, "cpw_Crises")$id
  return(getEmergencyByID(ids))
}
