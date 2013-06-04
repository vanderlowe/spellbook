# Load emergecy-level data
crises <- data.table(magicSQL("SELECT id AS emergency_id, ISOalpha2 AS iso2, ISOalpha3 AS iso3, year, month, title, funding, pledges FROM emergencies WHERE month IS NOT NULL AND type = 'Natural Disaster'", "cpw_Crises"))
setkey(crises, "emergency_id")

# Merge donor count information
donor.count <- data.table(magicSQL("SELECT emergency_id, COUNT(donor) as Donors FROM donations GROUP BY emergency_id", "cpw_Crises"))
setkey(donor.count, "emergency_id")
crises <- merge(crises, donor.count)
rm(donor.count)

test_that("Emergency-level data passes basic assumptions", {
  expect_true( # All ISO2 codes are valid
    all(crises$iso2 %in% getISOs(2)))
  
  expect_true( # All ISO3 codes are valid
    all(crises$iso3 %in% getISOs(3)))
  
  expect_true( # No missing ISO2 codes
    all(!is.na(crises$iso2)))
  
  expect_true( # No missing ISO3 codes
    all(!is.na(crises$iso3)))
  
  expect_true( # No future years
    max(crises$year) <= 2013)
  
  expect_true( # No months greater than December
    max(crises$month) == 12)
  
  expect_true( # No months less than January
    min(crises$month) == 1
  )
  
  expect_true( # No negative funding
    min(crises$funding) >= 0)
  
  expect_true( # There are more disasters than countries
    length(unique(crises$emergency_id)) >= length(unique(crises$iso2))) 
  
  expect_true( # No missing emergency ids 
    all(!is.na(crises$emergency_id)))
})
