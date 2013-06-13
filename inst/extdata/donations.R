getDonors <- function(id) {
  return(donor.countries[emergency_id == id, ]$iso2)
}

donations <- data.table(from = character(0), to = character(0), year = numeric(0))

recipients <- unique(crises$iso3)

# Get donor network by year
for (thisCountry in recipients) {
  thisCountry <- iso2(thisCountry)
  cat("Processing", thisCountry, "\n")
  theseDisasters <- crises[iso2 == thisCountry, emergency_id]
  
  for (thisDisaster in theseDisasters) {
    cat("Emergency id: ", thisDisaster, "\n")
    thisYear <- crises[emergency_id == thisDisaster, year]
    theseDonors <- getDonors(thisDisaster)
    if (identical(theseDonors, character(0))) {next}
    theseResults <- data.frame(from = theseDonors, to = thisCountry, year = thisYear)
    donations <- rbind(donations, theseResults)
  }  
}
donations <- donations[!from == as.character(to),] # Drop self-donation
