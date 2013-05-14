#' Plot country's Facebook friendships over time with optional event markers
#' 
#' This plots the month-to-month changes in FB friendships both internationally (incoming and outgoing relations) and domestically.
#' @param country Two-letter ISO-alpha2 code of the country
#' @param events An optional \code{data.frame} object with columns \code{date} (in YYYY-MM-DD format) and \code{name}, both as character strings.
#' @return A \code{ggplot2} graph
#' @import magic ggplot2 scales lubridate
#' @export
#' @examples
#' \dontrun{
#' plot.facebook("JP")
#' }

plot.facebook <- function(country, events) {
  country.name <- magicSQL(sprintf("SELECT name FROM countries WHERE ISOalpha2 = '%s'", country), "cpw_meta")[,1]
  plot.data <- do.call(rbind, list(indegree(country), outdegree(country), domestic(country)))
  plot.data <- plot.data[date < ymd("2012-11-01", quiet = T)]  # Remove last, incomplete data points
  
  p <- ggplot(plot.data, aes(x = date, y = Friendships)) + 
    theme_bw() +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = -90, vjust = 0.5)) +
    scale_y_continuous(name = "New friendships per month", labels = comma) +
    scale_x_datetime(breaks = date_breaks("1 months"), labels=date_format("%Y %b")) +
    geom_line(aes(color = type)) + 
    ggtitle(country.name)
  
  if (!missing(events)) {
    
    events$date <- ymd(events$date, quiet = T)
    return(p + geom_vline(data = events, aes(xintercept = as.numeric(date))) +
             geom_text(data = events, aes(x = date, y = 0, label = name), size = 3, angle = -90, vjust = -.4)
    )
  } else {
    return(p)  
  }
  
}
