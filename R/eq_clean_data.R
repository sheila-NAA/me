#'Package: buildingpackage
#'Title: Cleaning raw data
#'Date: 06/04/2021
#'Version: 0.0.0.9000
#'Authors@R:
#'  person(given = "Sheila",
#'         family = "Amoafo",email = "amoafosheila@outloo.com",
#'         role = c("aut", "cre"))
#'Description: This package cleans raw a dataframe into understandable
#'  and neat dataframe.
#'License: `use_mit_license()`
#'Encoding: UTF-8
#'LazyData: true
#'Roxygen: list(markdown = TRUE)
#'RoxygenNote: 7.1.1
#'
#'
#' @details
#' To asses the request we have to produce a [data frame][data.frame]
#' which has:
#'
#' - A date column created by uniting the year, month, day and
#'   converting it to the Date class
#' - latitude and longitude columns converted to numeric class
#' - using \code{\link{eq_location_clean}} cleans the location_name
#'   column by stripping out the country name (including the colon) and
#'   converts names to title case (as opposed to all caps).
#'
#' @note For simplicity of coding and consistency, we reduced all the
#'       column names to lowercase too.
#'
#' @param data A raw data frame of NOAA data
#'
#' @return a [tibble][tibble::tibble-package] (which is also a
#'         [data frame][data.frame]).
#' @export
#'
#' @examples
#' data(noaa)
#' eq_clean_data(noaa)
eq_clean_data <- function(data) {
 names(data) <- tolower(names(data))


    data %>%
    dplyr::mutate(
          location_name = eq_location_clean(location_name)
              ) %>%
    dplyr::filter(!is.na(year)) %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::mutate_at(
        dplyr::vars(month:second),
        dplyr::funs(
          purrr::map_chr(.,
             ~ dplyr::if_else(is.na(.x),
                 "01",
                   stringr::str_pad(.x, width = 2, pad = "0")
               )
            )
        )
    ) %>%
    tidyr::unite(col = "date", year, month, day,
       sep    = "-",
       remove = FALSE
    )%>%
    dplyr::mutate(
      date = good_date(year, month, day)
    )
}

use_vignette("eq_clean_data")
