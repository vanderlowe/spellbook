#' Fetch data for a date range
#' 
#' Queries \code{cpw_meta} for Facebook friendship data for a country between two dates.
#' @param country A valid two- or three-letter ISO 3166-1 alpha code.
#' @param start.date A character string in the format YYYY-MM-DD.
#' @param end.date A character string in the format YYYY-MM-DD.
#' @return A data frame.
#' @export
#' @import lubridate
#' @note Because the underlying data are indexed by year and month, the DD-portion (day) of start and end dates will be ignored and data are returned at month-level resolution.
#' 
getDateRange <- function(country, start.date, end.date) {
  # Convert input to usable formats
  iso2.code <- iso2(country)
  start <- ymd(start.date, quiet = T)
  end <- ymd(end.date, quiet = T)
  
  # The data resolution is one month, so day portion of the date is irrelevant
  start.txt <- paste(year(start), sprintf("%02d", month(start)), sep = "")
  end.txt <- paste(year(end), sprintf("%02d", month(end)), sep = "")

  results <- getFacebookData(country = iso2.code, 
    where.statement = sprintf("YearMonth BETWEEN '%s' AND '%s'",
                              start.txt, end.txt)
  )
    
  return(results)
}
