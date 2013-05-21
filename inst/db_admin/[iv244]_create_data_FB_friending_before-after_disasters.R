# TODO: Automatic parsing of require/library calls for necessary citations
require(magic)
require(spellbook)
require(lubridate)

options(scipen=999)  #' Suppress scientific notation for big numbers

# In order to compare Facebook friendships before and after natural disasters, 
# we first need a list of all natural disasters, which was obtained from Financial Tracking Service (FTS).
disasters <- magicSQL("SELECT * FROM emergencies WHERE year IS NOT NULL AND month IS NOT NULL", "cpw_Crises")

# Since FTS only records the month and year information for each emergency, but not the specific date,
# all natural disasters were set to occur on the 15th of each month (YYYY-MM-15) in order to maintain
# compatible date format with the rest of the data.
disasters$date <- ymd(paste(disasters$year, disasters$month, 15, sep = "-"))

# Likewise, since the data of international Facebook friendships may not be complete 
# for the first and last months of data collection,
# only disasters that occurred between March 2006 and October 2012 
# (i.e., a period during which friendship data are complete) were included in the analyses.
disasters <- subset(disasters, disasters$date > ymd("2006-02-15") & disasters$date < ymd("2012-11-15"))

# This yields ```nrow(disasters)``` natural disasters as tracked by FTS.
nrow(disasters)

# The data consisted of the number of relationships each country sent to other countries during a period when
# a natural disaster occurred in either country.

fb.data <- data.frame(Disaster_id = numeric(0), 
                      Disaster_Country = character(0),
                      Disaster_ISOalpha3 = character(0),
                      Friender_Country = character(0),
                      Friended_Country = character(0),
                      Count = numeric(0),
                      YearMonth = numeric(0),
                      Countdown = numeric(0),
                      Crisis = numeric(0),
                      stringsAsFactors = F)

for (i in 1:nrow(disasters)) {
  disaster <- disasters[i, ]
  
  cat("Collecting data for", as.character(disaster$country), "in", as.character(disaster$date), "\n")
    
  disaster.timeline <- getDisasterPeriod(
    country = disaster$ISOalpha2,
    date = as.character(disaster$date),
    offset = 6
  )
  
  names(disaster.timeline) <- gsub("^crisis$", "Crisis", names(disaster.timeline))  # Fix different variable spelling
  disaster.timeline$Disaster_id <- disaster$id
  disaster.timeline$Disaster_Country <- disaster$country
  disaster.timeline$Disaster_ISOalpha3 <- disaster$ISOalpha3
  
  results <- disaster.timeline[, names(fb.data)]
  
  fb.data <- rbind(fb.data, results)
}

write.csv(fb.data, "~/Downloads/fb_disasters.csv", row.names = F, na = "NULL")

require(RMySQL)
conn <- dbConnect(drv=MySQL(), user = Sys.getenv("magic_user"), password = Sys.getenv("magic_password"), host = "alex.e-psychometrics.com", dbname = "cpw_iv244")
dbWriteTable(conn=conn, name="fb_disasters", value=fb.data, overwrite = T, row.names = F)