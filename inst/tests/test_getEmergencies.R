test_that("getEmergencies returns a regular data frame", {
  aruba <- getEmergencies("ABW")
  expect_true(class(aruba) == "data.frame")
  expect_true(class(aruba[, "id"]) == "numeric")
  expect_equal(setdiff(names(aruba), c("id", "title", "year", "glideid", "type", "country", "funding", "pledges")), character(0))
})