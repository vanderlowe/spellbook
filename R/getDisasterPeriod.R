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

getDisasterPeriod <- function(country, date, offset, dummy = T, countdown = T) {
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
  
  if (relative.time) {
    # Indicate 'countdown' to disaster
    results$Countdown <- NA
    results[results$YearMonth == event.YearMonth, "Countdown"] <- 0  # Set midpoint to disaster month
    
    time.points <- setdiff(unique(results$YearMonth), event.YearMonth)
    before <- time.points[time.points < event.YearMonth]
    relative.start <- length(before) * -1
    
    while (length(before) > 0) {
      current.month <- min(before)  # Take the lowest month value
      results[results$YearMonth == current.month, "Countdown"] <- relative.start  # Set relative month value
      relative.start <- relative.start + 1  # Update relative month value
      before <- setdiff(before, current.month)  # Remove current month from the pool
    }
    
    after <- time.points[time.points > event.YearMonth]
    relative.end <- length(after)
    
    while (length(after) > 0) {
      current.month <- max(after)  # Take the highest month value
      results[results$YearMonth == current.month, "Countdown"] <- relative.end  # Set relative month value
      relative.end <- relative.end - 1  # Update relative month value
      after <- setdiff(after, current.month)  # Remove current month from the pool
    }
    
  }
  
  return(results)
}

a <- getDisasterPeriod(country="FJ", date="2010-07-15", offset = 1, dummy = T, countdown = T)