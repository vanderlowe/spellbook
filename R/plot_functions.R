iso2  <- function(country) {
  if (nchar(country) ==  2) {return(country)}
  if (nchar(country) ==  3) {return(getISO2(country))}
  stop("You must provide a valid 2- or 3-letter ISO country code.")
}

getFacebookData <- function(country) {
  iso.code <- iso2(country)
  all.dyads <- magicSQL(sprintf("SELECT * FROM Friendships_Directional_Final WHERE Friender_Country = '%s' OR Friended_Country = '%s'", iso.code, iso.code), "cpw_meta")
  all.dyads$date <- lubridate::ymd(paste(all.dyads$Year, all.dyads$Month, 15), quiet = T)
  return(all.dyads)
}

outdegree <- function(country) {
  dyads.friendship <- data.table(getFacebookData(country))
  results <- dyads.friendship[Friender_Country == country & !Friended_Country == country, list(Friendships = sum(Count, na.rm = T)), by = date]
  results$type <- "Out"
  return(results)
}

indegree <- function(country) {
  dyads.friendship <- data.table(getFacebookData(country))
  results <- dyads.friendship[!Friender_Country == country & Friended_Country == country, list(Friendships = sum(Count, na.rm = T)), by = date]
  results$type <- "In"
  return(results)
}

domestic <- function(country) {
  dyads.friendship <- data.table(getFacebookData(country))
  results <- dyads.friendship[Friender_Country == country & Friended_Country == country, list(Friendships = sum(Count, na.rm = T)), by = date]
  results$type <- "Within"
  return(results)
}

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

plot.crises <- function(country) {
  country <- toupper(country)
  # Get country codes
  iso2s <- getISOs(2)
  iso3s <- getISOs(3)
  
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

