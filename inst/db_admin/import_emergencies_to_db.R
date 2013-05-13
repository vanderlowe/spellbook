require(magic)
require(data.table)
require(ggplot2)
require(scales)
require(lubridate)
require(testthat)
require(RJSONIO)
require(plyr)

source("Method/Measures/Humanitarian aid/Financial Tracking Service/R/utilities.R")
source("Method/Measures/Humanitarian aid/Financial Tracking Service/R/getEmergencies.R")

# Get a vector of 3-letter country codes
countries <- magicSQL("SELECT ISOalpha3 FROM countries", "cpw_meta")[, 1]

all.emergencies <- lapply(countries, getEmergencies)  # Takes few minutes to run

# Erase existing data
magicSQL("TRUNCATE TABLE emergencies", "cpw_Crises")

# Enter data for each country
for (i in 1:length(countries)) {
  # Skip countries with no data
  if (is.null(all.emergencies[[i]])) {next}
  
  iso3 <- countries[i]
  cat(iso3, "\n")
  national.emergencies <- all.emergencies[[i]]
  
  for (j in 1:nrow(national.emergencies)) {
    e <- national.emergencies[j, ]
    
    # Note: Some funding amounts get into big int realm
    sql <- sprintf("INSERT INTO `emergencies` VALUES (%i, '%s', '%s', \"%s\", %i, %i, %i, '%s', '%s', \"%s\", '%s', '%s')",
                   e$id, iso3, "", gsub("'", "''", e$title), e$year, 0, e$glideid, e$type, e$country, as.character(e$funding), as.character(e$pledges))
    # print(sql)    
    magicSQL(sql, "cpw_Crises")
  }
}

# Populate months
# NOTE: Does not detect appreviations (e.g., Feb)... Needs manual cleanup
ids <- magicSQL("SELECT id, title FROM emergencies WHERE YEAR IN (2006,2007,2008,2009,2010,2011,2012)", "cpw_Crises")

for (i in 1:nrow(ids)) {
    month <- parseMonth(ids[i, "title"])
    if (!is.null(month)) {
      magicSQL(sprintf("UPDATE emergencies SET month = %i WHERE id = %i", month, ids[i, "id"]), "cpw_Crises")
    }
}


# Populate ISO2s
isos <- magicSQL("SELECT DISTINCT(ISOalpha3) FROM emergencies", "cpw_Crises")[, 1]

for (iso3 in isos) {
    iso2 <- getISO2(iso3)
    magicSQL(sprintf("UPDATE emergencies SET ISOalpha2 = '%s' WHERE ISOalpha3 = '%s'", iso2, iso3), "cpw_Crises")
}
