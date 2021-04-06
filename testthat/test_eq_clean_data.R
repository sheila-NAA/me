context("test-eq_clean_data")

tidy_noaa <- eq_clean_data(noaa)


test_that("output is a data.frame", {
  expect_is(tidy_noaa, "data.frame")
})


test_that("lowercase columns", {
  noaa_names <- names(tidy_noaa)

  expect_equal(noaa_names, tolower(noaa_names))
})


test_that("correct column date", {
  data_column <- tidy_noaa[['date']]

  expect_is(data_column, "Date")
})


test_that("latitude and longitude columns converted to numeric class", {
  latitude_column  <- tidy_noaa[['latitude']]
  longitude_column <- tidy_noaa[['longitude']]

  expect_is(latitude_column, "numeric")
  expect_is(longitude_column, "numeric")
})


test_that("location_names are cleaned", {
  location_column <- tidy_noaa[['location_name']]

  expect_equal(
    location_column,
    eq_location_clean(noaa$LOCATION_NAME)
  )
})
