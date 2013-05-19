test_that("ISO2 conversion returns sensible responses", {
  expect_equal(class(iso2("US")), "character")
  expect_equal(
    c(nchar(iso2("US")), nchar(iso2("USA")), nchar(iso2("United States"))), 
    c(2,2,2)
  )
  expect_equal(iso2("NA"), "NA")  # Returns itself, it two chars long
  expect_equal(iso2("NAM"), "NA")
  expect_equal(iso2("Namibia"), "NA")
  expect_error(iso2(NA))
  expect_error(iso2())
  
})

test_that("iso2 validation works", {
  expect_equal(verifyISO2("XX"), NULL)
  expect_error(verifyISO2())
  expect_equal(verifyISO2("US"), "US")
})