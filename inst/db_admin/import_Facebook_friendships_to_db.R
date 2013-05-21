require(RMySQL)
fb <- read.table("~/Downloads/friends_country.csv", na.strings = "NULL", header = F)
names(fb) <- c("Year", "Month", "Friender_Country", "Friended_Country", "Count")

# Clean up data
fb.clean <- subset(fb, Year >= 2006 & !is.na(Friender_Country) & !is.na(Friended_Country))

# Establish database connection
conn <- dbConnect(drv = MySQL(), user = Sys.getenv("magic_user"), password = Sys.getenv("magic_password"), host = "alex.e-psychometrics.com", dbname = "cpw_meta")

dbWriteTable(conn, name = "Friendships_Directional_Final", value = fb.clean, row.names = F, overwrite = F, append = T)