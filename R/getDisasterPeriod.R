#' Fetch before—after data for a natural disaster
#' 
#' Queries \code{cpw_meta} for Facebook friendship data three months before and after a natural disaster.
#' @param country Two- or three-letter ISO alpha code or country name
#' @param date A character string in the format of YYYY-MM-DD.
#' @param offset Integer to indicate how many months of data before/after disaster to include
#' @return A data frame.
#' @export
#' @import lubridate
#' 

getDisasterPeriod <- function(country, date, offset, dummy = T) {
  event <- ymd(date, quiet = T)
  event.YearMonth <- paste(year(event), sprintf("%02d", month(event)), sep = "")
  start.date <- as.character(event - months(offset))
  end.date <- as.character(event + months(offset))
  results <- getDateRange(country, start.date, end.date)
  
  if (dummy) {
    # Code dummy variable for before crisis (0) and after crisis (1)
    results$crisis <- 0
    results[results$YearMonth >= event.YearMonth, "crisis"] <- 1
  }
  return(results)
}

