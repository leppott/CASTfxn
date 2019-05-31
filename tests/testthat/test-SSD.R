context("test-SSD")

test_that("Plot created", {
  # getSSD, Example 1
  myDF   <- data_SSD
  myRT   <- "ResponseType"
  myTaxa <- "Taxa"
  myExp  <- "Exposure_mgperL"
  # Run function
  p1 <- getSSDplot(myDF, myRT, myTaxa, myExp)
  #
  # TEST
  #expect_equal(sum(is.na(p1)), 0)
  #expect_equal(is.object(p1), TRUE)
  expect_equal(exists("p1"), TRUE)
})
