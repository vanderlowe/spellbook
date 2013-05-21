#' Get FB data on new international and domestic friendships by country
#' 
#' Accesses \code{cpw_meta} database to retrieve data on new friendships relating to the target country.
#' @param country Two- or three letter ISO code of the country (or a valid country name)
#' @param where.statement A vector of WHERE conditions amended to the query
#' @return A \code{data.frame}
#' @import magic lubridate
#' @export
#' @examples
#' \dontrun{
#' getFacebookData("FI")
#' getFacebookData("FIN")
#' getFacebookData("Finland")
#' getFacebookData("Finland", "year = 2006")
#' getFacebookData("Finland", c("year = 2006", "Count > 100"))
#' }

getFacebookData <- function(country, where.statement = NULL) {
  iso.code <- iso2(country)
  and.where <- addWHERE(where.statement)
  sql <- sprintf("SELECT * FROM Friendships_Directional_Final WHERE (Friender_Country = '%s' OR Friended_Country = '%s')%s", iso.code, iso.code, and.where)
  all.dyads <- magicSQL(sql, "cpw_meta")
  if (nrow(all.dyads) == 0) {
    stop(sprintf("Cannot locate data for country code %s", iso.code))
  }
  all.dyads$date <- lubridate::ymd(paste(all.dyads$Year, all.dyads$Month, 15), quiet = T)
  return(all.dyads)
}

addWHERE <- function(where.statement) {
  if (is.null(where.statement)) {
    and.where <- ""
  } else {
    and.where <- generateANDWHERE(where.statement, "Friendships_Directional_Final")
  }
  return(and.where)
}

generateANDWHERE <- function(x, table.name) {
  if (is.null(x)) {return("")}
  if (length(x) == 0) {return("")}
  where <- "AND "
  for (i in 1:length(x)) {
    value <- x[i]
    thisWhere <- paste(table.name, value, sep =".")
    if (i > 1) {
      where <- paste(where, thisWhere, sep = " AND ")  
    } else {
      where <- paste(where, thisWhere, sep = "")
    }
  }
  return(where)
}
