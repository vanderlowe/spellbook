# Load country-level data
world.data <- data.table(magic::countries())
setkey(world.data, "name")
test_that("World data passes basic assumptions", {
  
  expect_equal(# "NA" is appropriately coded as Namibia, not as missing value
    world.data[ISOalpha2 == "NA"]$name,
    "Namibia"
  )
  
  expect_true( # World has 251 countries
    nrow(world.data) == 251  # (As of Jun 4, 2013)
  )
  
  expect_true(
    all( # Country and capital names are strings
      class(world.data$name) == "character",
      class(world.data$capital) == "character"
    )
  )
  
  expect_true(
    all( # No missing ISO alpha codes
      !is.na(world.data$ISOalpha2),
      !is.na(world.data$ISOalpha3)
    )
  )
  
  expect_true(
    all(
      length(unique(world.data$ISOalpha2)) == length(unique(world.data$ISOalpha3)), # There are as many ISO alpha 2 codes as ISO alpha 3 codes
      length(unique(world.data$ISOalpha2)) == length(unique(world.data$id)) # There are as many ISO alpha 2 codes as there are countries
    )
  )
  
  expect_true(
    # No missing location data
    !any(
      is.na(world.data$capital_latitude),
      is.na(world.data$capital_longitude)
    )
  )
  
  expect_true( # Only 8 regions
    length(unique(world.data$region)) == 8
  )
  
  expect_true(
    all( # Latitude between -90 and 90
      max(world.data$capital_latitude) <= 78.23, # Svalbard and Jan Mayen
      min(world.data$capital_latitude) >= -90 # Antarctica
    )
  )
  
  expect_true(
    all( # Longitude between -180 and 180
      max(world.data$capital_longitude) <= 180, # Funafuti, Tuvalu
      min(world.data$capital_longitude) >= -180 # Mata-Utu, Wallis and Futuna
    )
  )
  
})
