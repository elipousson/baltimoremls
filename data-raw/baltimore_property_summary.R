## code to prepare `baltimore_real_property_summary` dataset goes here

library(tidyverse)

baltimore_property <- getdata::get_location_data(
  data = "baltimore_property",
  pkg = "mapbaltimore"
)

baltimore_property_prepped <- baltimore_property %>%
  naniar::replace_with_na(
    replace = list(
      currland = 0,
      currimpr = 0
    )
  ) |>
  sfext::rename_sf_col()

baltimore_property_summary <- vctrs::vec_rbind(
  summarise_baltimore_property(
    property_data = baltimore_property_prepped,
    input_sf = mapbaltimore::neighborhoods,
    geography = "neighborhood",
    id_col = "name",
    .by = c("type", "name")
  ),
  summarise_baltimore_property(
    property_data = baltimore_property_prepped,
    input_sf = mapbaltimore::council_districts,
    geography = "council district"
  ),
  summarise_baltimore_property(
    property_data = baltimore_property_prepped,
    input_sf = mapbaltimore::planning_districts,
    geography = "planning district"
  ),
  summarise_baltimore_property(
    property_data = baltimore_property_prepped,
    name = "Baltimore city",
    geography = "county"
  )
) |>
  dplyr::mutate(
    county = "Baltimore city",
    state = "MD",
    .before = all_of("geography")
  ) |>
  dplyr::relocate(type, .after = all_of("name"))

usethis::use_data(baltimore_property_summary, overwrite = TRUE)
