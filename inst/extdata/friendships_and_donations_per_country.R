require(magic)
require(spellbook)
Sys.setlocale(locale="C")

# Step 1: Get basic crisis data
loadData("crises")
crisis.ids <- unique(crises$emergency_id)

# Step 2: Get Facebook friendship data for the month of the crisis
incoming.friendships <- lapply(crisis.ids, getGlobalIncomingFacebookRelationsByDisaster)
incoming.friendships <- do.call(rbind, incoming.friendships)
incoming.friendships <- data.table(incoming.friendships)
setkeyv(incoming.friendships, c("emergency_id", "target_country", "source_country", "year", "month"))

test_that("The incoming friendship data is of expected size", {
  emergency.count <- length(crisis.ids)
  country.count <- nrow(allCountries())
  expect_equal(nrow(incoming.friendships), country.count * emergency.count)
})

# Calculate total number of friendships per country per month
incoming.friendships[, total_friendships := sum(friendships), by = list(year, month, target_country)]

# Step 3: Get donations by donor countries
getDonations <- function(id) {
  cat(id, "\n")
  sql <- sprintf("SELECT emergency_id, donor, amount, ISOalpha2 as donorISO FROM donations LEFT JOIN donors ON donations.donor = donors.name WHERE emergency_id = %i AND ISOalpha2 IS NOT NULL", id)
  return(magicSQL(sql, "cpw_Crises"))
}

donations <- lapply(crisis.ids, getDonations)
donations <- do.call(rbind, donations)
donations <- data.table(donations)
donations <- donations[, list(emergency_id, source_country = donorISO, amount)]
setkeyv(donations, c("emergency_id", "source_country"))

# National donor statistics
donations.by.country <- donations[, list(total_amount = sum(amount)), by = source_country]

# Step 4: Merge data
# Merge friendships with donations
friendships.with.donations <- merge(incoming.friendships, donations, by = c("emergency_id", "source_country"), all.x = T, all.y = T)
setkeyv(friendships.with.donations, c("emergency_id", "target_country", "year", "month", "source_country"))
friendships.with.donations[is.na(amount), amount := 0]  # Change NAs to zeros for no donation
test_that("The data is of expected size", {
  expect_equal(nrow(incoming.friendships), nrow(friendships.with.donations))
})

# Merge crises with friendships and donations data
crises <- merge(crises, friendships.with.donations, by = c("emergency_id", "target_country", "year", "month"), all.x = T, all.y = T)
setkeyv(crises, c("emergency_id", "target_country", "year", "month", "source_country"))

# Merge national level data
#gdp.data <- lapply(allCountries()$source_country, getGDPfromWorldBank)
#gdp.data <- do.call(rbind, gdp.data)
  
country.data.timeseries <- data.table(magicSQL("SELECT Code AS country, Year as year, InternetUsers, GDP, Population FROM countries_by_year", "cpw_meta"))
# First, merge target_country data
target.countries <- copy(country.data.timeseries)
setnames(target.countries, names(country.data.timeseries), gsub("^", "target_", names(country.data.timeseries)))
setnames(target.countries, names(target.countries), gsub("^target_year", "year", names(target.countries)))
setkeyv(target.countries, c("target_country", "year"))
crises <- merge(crises, target.countries, by = c("target_country", "year"), all.x = T)

# Then, the same for source countries
source.countries <- copy(country.data.timeseries)
setnames(source.countries, names(country.data.timeseries), gsub("^", "source_", names(country.data.timeseries)))
setnames(source.countries, names(source.countries), gsub("^source_year", "year", names(source.countries)))
setkeyv(source.countries, c("source_country", "year"))
crises <- merge(crises, source.countries, by = c("source_country", "year"), all.x = T)

# Step 5: Calculate percentages
test_that("All emergencies have received some donations (so that division by zero does not occur)", {
  expect_true(min(crises$total_funding) > 0)
})
crises[, fund_percent := amount/total_funding, by = c("emergency_id", "target_country", "year", "month", "source_country")]

test_that("All target countries have received some friendships (so that division by zero does not occur)", {
  expect_true(min(crises$total_friendships) > 0)
})
crises[, friend_percent := friendships/total_friendships, by = c("emergency_id", "target_country", "year", "month", "source_country")]

# Step 6: Dummy code for donation
crises$dummyDonation <- 0
crises[amount > 0, dummyDonation := 1]

# Step 7: Filter out countries that did not donate anything
crises <- crises[amount > 0, ]

# (OLD) Step 7: Filter out donations from micronations
# exclude <- magicSQL("SELECT DISTINCT(Code) FROM countries_by_year WHERE population < 1000000", "cpw_meta")[,1]
# crises <- crises[!source_country %in% exclude,]

# At this stage, the crises data.table is ready.
