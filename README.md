## earthquakr R package

<!-- badges: start -->
<!-- badges: end -->

The goal of earthquakr is to show
information about history of earthquake around the world.

To do that, it provide a raw version of the
[NOAA Significant Earthquake DatabaseD](https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1).

## Installation

You can install the released version of earthquakr from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("earthquakr")'=

requireNamespace("DT", quietly = TRUE)
requireNamespace("lubridate", quietly = TRUE)
requireNamespace("ggplot2", quietly = TRUE)
library(devrcap)
    data("noaa")
DT::datatable(noaa)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(earthquakr)
## basic example code
```
The first function implemented would be used to clean this database and
prepare it for further analyses and visualizations.

More in details, the function `eq_clean_data()` produce a dataset with
the following characteristics:

 - A date column created by uniting the year, month, day and converting
   it to the Date class

 - latitude and longitude columns converted to numeric class

 - use the funciton `eq_location_clean()` (from the **earthquakr** package
   too) to clean the _location_name_ column by stripping out the country
   name (including the colon) and converting names to title case (as
   opposed to all caps).

 - set all the column names to lowercase

```{r}
data_cleaned <- devrcap::eq_clean_data(noaa) 
DT::datatable(data_cleaned)
```

Using the ggplot2 geom `geom_timeline()` provided by **earthquakr**,
we can use the cleaned data to plot time lines of earthquakes
both overall and by group (e.g., country).

Moreover, using `geom_timeline_label()` we can add information about the
earthquakes, e.g. the location. Due to the possible huge ammount of 
information that could be displayed, an option _n_max_ can be used to
limit the ammount of information to show. If the _size_ option is also
provided to ggplot, the _n_max_ option will show the _n_max_ largest
earthquakes (otherwise it will be simply sample _n_max_ earthquakes at
random to show the information about the location)


```{r, fig.cap = "Time lines of earthquakes in Greece, Italy and Portugal, from 1900 to date. For each earthquake, the point size is proportional with intensity, colour gradient is proportional with total number of death."}
noaa %>%
    eq_clean_data() %>%
    dplyr::filter(
        country %in% c("ITALY", "GREECE", "PORTUGAL"),
        lubridate::year(date) >= 1900
    ) %>%
    ggplot2::ggplot(ggplot2::aes(
        x = date,
        y = country,
        size   = eq_primary,
        colour = log(total_deaths),
        label  = location_name
    )) +
    geom_timeline() +
    geom_timeline_label(n_max = 3) +
    ggplot2::theme(legend.position = "bottom")
```

Using the funciton `eq_map()` we can also show an interactive map of earthquakes, and with the functionality added by the function `eq_create_label()` we can also show (interactively) information (when
present) about location, magnitude and total death of each earthquakes.

```{r}
noaa %>%
    eq_clean_data %>%
    dplyr::filter(
        country == "MEXICO",
        lubridate::year(date) >= 2000
    ) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map("popup_text")
```
