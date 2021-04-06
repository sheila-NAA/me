context("test-geom_timeline")

gg_output <- noaa %>%
  eq_clean_data() %>%
  dplyr::filter(country == "GREECE", lubridate::year(date) >= 2000) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = date,
    y = country,
    size   = eq_primary,
    colour = log(total_deaths)
  )) +
  geom_timeline()


test_that("correct object in output", {
  expect_is(gg_output, "gg")
})
