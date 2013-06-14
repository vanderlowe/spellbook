#' Get incoming Facebook relations from all countries to target country on the month of disaster
#' 
#' Based on an emergency id, this function returns incoming friendships from each world country to the disaster country.
#' If no data is available for a country, a zero will be reported.
#' 
#' @param id Emergency id that identifies a natural disaster in FTS data.
#' @return This function returns a \code{data.table} with 251 rows, one for each country in the world.
#' @import magic
#' @export

getGlobalIncomingFacebookRelationsByDisaster <- function(id) {
  id <- as.numeric(id)
  
  if (is.na(id)) {stop("Emergency id must be numeric.")}
  
  thisCrisis <- magicSQL(sprintf("SELECT * FROM emergencies WHERE id = %i", id), "cpw_Crises")
  if (nrow(thisCrisis) == 0) {stop("Cannot locate an emergency with that id.")}
  
  iso.code <-   thisCrisis$ISOalpha2  # Get disaster country code
  year <- thisCrisis$year  # Get year of disaster
  month <- thisCrisis$month  # Get month of disaster

  cat(iso.code, year, month,"\n")
  # Construct a WHERE statement to feed into getFacebookData function
  # i.e., must match emergency year and month, exclude outgoing and domestic relations
  where <- sprintf("Year = %i AND Month = %i AND Friender_Country != '%s'",
                   year, month, iso.code)
  
  # Query Facebook relations table
  results <- try(getFacebookData(iso.code, where), silent = T)
  if (class(results) == "try-error") {
    # Query returned no data, return 0 for all countries
    results <- data.table(target_country = iso.code, source_country = allCountries()$source_country, friendships = 0)
    setkeyv(results, c("target_country", "source_country"))
  } else {
    results <- data.table(results)
    results <- results[, list(target_country = Friended_Country, source_country = Friender_Country, friendships = Count)]
    setkeyv(results, c("target_country", "source_country"))
    
    # Merge results with all world countries to ensure 251 rows
    world <- data.table(target_country = iso.code, source_country = allCountries()$source_country)
    setkeyv(world, c("target_country", "source_country"))
    
    results <- merge(world, results, all.x = T, by = c("target_country", "source_country"))
    results[is.na(friendships), friendships := 0]  # Turn missing data to zeroes
  }
  
  # Add relevant data
  key.data <- data.table(emergency_id = id, year = year, month = month, target_country = iso.code)
  setkey(key.data, "target_country")
  
  results <- merge(key.data, results, by = "target_country")
  
  if (nrow(results) != 251) {stop("Results do not have 251 rows. This was an unexpected error: you have some debugging to do.")}
  
  return(as.data.frame(results))
}
