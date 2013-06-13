require(RJSONIO)
require(plyr)
require(magic)
require(data.table)
require(testthat)

# Get all donors from FTS
# Define a list of expected columns
cols <- c("id", "name", "abbreviation", "type")

# Fetch JSON from Financial Tracking Service website
json_file <- "http://fts.unocha.org/api/v1/organization.json"
json_data <- fromJSON(paste(readLines(json_file, warn = F), collapse=""))

# Convert list with sporadic NULL values to a well-behaving data.frame
# Solution from http://stackoverflow.com/questions/15793759/convert-r-list-to-dataframe-with-missing-null-elements
donors <- rbind.fill(lapply(json_data, function(f) {
  as.data.frame(Filter(Negate(is.null), f))
}))

magicDB <- function(db = NULL) {
  if (is.null(db)) {stop("You must specify a database")}
  killConnections()
  connection <- dbConnect(MySQL(), 
                          host = Sys.getenv("magic_host"), 
                          user = Sys.getenv("magic_user"), 
                          password = Sys.getenv("magic_password"),
                          dbname = db
  )
  return(connection)
}

killConnections <- function() {
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) {
    dbDisconnect(con)
  }
  return(TRUE)
}

# Save data to db
# NOTE: "Arts for Peace" has wrong type
dbWriteTable(conn, name = "donors", value = donors, overwrite = T, row.names = F)
conn <- magicDB("cpw_Crises")

# Add country ISO codes
magicSQL("ALTER TABLE donors ADD ISOalpha2 VARCHAR(2)", "cpw_Crises")

# Get country information
countries <- magicSQL("SELECT ISOalpha2, name FROM countries", "cpw_meta")

# Store country names for quick matching
country.names <- countries$name

for (i in 1:nrow(donors)) {
  donor <- donors[i, ]
  if (donor$name %in% country.names) {
    iso2 <- countries[countries$name == donor$name, "ISOalpha2"]
    cat(donor$name, iso2, "\n")
    sql <- sprintf("UPDATE donors SET ISOalpha2 = '%s' WHERE id = '%i'",
                   iso2, donor$id
    )
    magicSQL(sql, "cpw_Crises")
  }
}

# NOTE: The script misses at least the fol lowing country codes due to spelling differences:
# MD, BO, BA, VG, CI, CD, CG, VA, LA, MK, FM, PS, RE, RS, ME, SA, SR, TW, TZ, TL, GB, US, HK

# Sanity check
test_that("All donor codes are valid ISO alpha 2 codes", {
  donor.codes <- magicSQL("SELECT ISOalpha2 FROM donors WHERE ISOalpha2 IS NOT NULL", "cpw_Crises")[, 1]
  expect_true(
    all(donor.codes %in% countries$ISOalpha2)
    )
})