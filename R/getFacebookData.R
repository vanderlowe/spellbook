#' Get FB data on new international and domestic friendships by country
#' 
#' Accesses \code{cpw_meta} database to retrieve data on new friendships relating to the target country.
#' @param country Two-letter ISO code of the country
#' @return A \code{data.frame}
#' @import magic lubridate
#' @export
#' @examples
#' \dontrun{
#' getFacebookData("FI")
#' }

getFacebookData <- function(country) {
  iso.code <- iso2(country)
  all.dyads <- magicSQL(sprintf("SELECT * FROM Friendships_Directional_Final WHERE Friender_Country = '%s' OR Friended_Country = '%s'", iso.code, iso.code), "cpw_meta")
  all.dyads$date <- lubridate::ymd(paste(all.dyads$Year, all.dyads$Month, 15), quiet = T)
  return(all.dyads)
}
