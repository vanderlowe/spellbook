test_that("getGlobalIncomingFacebookRelationsByDisaster returns sensible data", {
  # 14859 = Aruba 2003
  aruba <- getGlobalIncomingFacebookRelationsByDisaster(14859)
  # 16004 = Australia 2010
  australia <- getGlobalIncomingFacebookRelationsByDisaster(16004)
  
  country.count = 251
  expect_equal(
    # Although no FB data exists for 2003, data should still have 251 rows
    nrow(aruba), 
    country.count
  )
  
  expect_equal(sum(aruba$friendships), 0)  # Since there should be no data and NAs are converted to 0s
  
  expect_equal(
    # Ditto for Australia in 2010
    nrow(australia), 
    country.count
  )
})
