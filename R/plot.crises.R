#' Plot country's Facebook friendships with markers indicating natural disasters
#' 
#' This plots the month-to-month changes in FB friendships both internationally (incoming and outgoing relations) and domestically.
#' Additionally, the graph shows major natural disasters that occurred during that time.
#' @param country Two- or three-letter ISO code of the country
#' @return A \code{ggplot2} graph
#' @import magic ggplot2
#' @export
#' @examples
#' \dontrun{
#' plot.crises("HTI")}

plot.crises <- function(country) {
  country <- toupper(as.character(country))
  # Get country codes
  iso2s <- as.character(getISOs(2))
  iso3s <- as.character(getISOs(3))
  
  if (country %in% c(iso2s, iso3s)) {
    # Get both 2- and 3-letter ISO codes
    if (nchar(country) == 2) {iso2 <- country; iso3 <- getISO3(country)}
    if (nchar(country) == 3) {iso2 <- getISO2(country); iso3 <- country}
    
    events <- magicSQL(sprintf("SELECT year, month, title as name FROM emergencies WHERE ISOalpha3 = '%s' AND month IS NOT NULL AND year > 2005", iso3), "cpw_Crises")
    events$date <- paste(events$year, events$month, 15)
    events$year <- NULL
    events$month <- NULL
    
    plot.facebook(iso2, events)
    
  } else {
    stop("Please provide valid 2- or 3-letter ISO country code.")
  }
}