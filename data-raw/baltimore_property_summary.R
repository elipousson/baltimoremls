## code to prepare `baltimore_real_property_summary` dataset goes here

library(tidyverse)

baltimore_property <- getdata::get_location_data(
  data = "baltimore_property",
  pkg = "mapbaltimore"
)

baltimore_property_prepped <- baltimore_property %>%
  dplyr::select(
    c(vacant_lot, vacant_bldg, usegroup, currland, currimpr)
  ) |>
  naniar::replace_with_na(
    replace = list(
      currland = 0,
      currimpr = 0
    )
  )

baltimore_property_nhood_summary <- baltimore_property_prepped |>
  sf::st_point_on_surface() |>
  sf::st_join(mapbaltimore::neighborhoods) |>
  sf::st_drop_geometry() %>%
  filter(!is.na(name)) %>%
  group_by(type, name) %>%
  reframe(
    num_properties = n(),
    num_residential = sum(usegroup %in% c("R")),
    perc_residential = round(num_residential / num_properties, digits = 2),
    median_land_value = median(currland, na.rm = TRUE),
    median_imprv_value = median(currimpr, na.rm = TRUE)
  ) |>
  mutate(
    geography = "neighborhood"
  )

baltimore_property_district_summary <- baltimore_property_prepped |>
  sf::st_point_on_surface() |>
  sf::st_join(mapbaltimore::council_districts) |>
  sf::st_drop_geometry() %>%
  filter(!is.na(name)) %>%
  group_by(name) %>%
  reframe(
    num_properties = n(),
    num_residential = sum(usegroup %in% c("R")),
    perc_residential = round(num_residential / num_properties, digits = 2),
    median_land_value = median(currland, na.rm = TRUE),
    median_imprv_value = median(currimpr, na.rm = TRUE)
  ) |>
  mutate(
    geograpy = "council district"
  )


baltimore_property_citywide_summary <- baltimore_property_prepped |>
  sf::st_drop_geometry() %>%
  reframe(
    num_properties = n(),
    num_residential = sum(usegroup %in% c("R")),
    perc_residential = round(num_residential / num_properties, digits = 2),
    median_land_value = median(currland, na.rm = TRUE),
    median_imprv_value = median(currimpr, na.rm = TRUE)
  ) |>
  mutate(
    name = "Baltimore city",
    type = NA_character_,
    .before = everything()
  ) |>
  mutate(
    geography = "county"
  )

baltimore_property_summary <- vctrs::vec_rbind(
  baltimore_property_citywide_summary,
  baltimore_property_nhood_summary,
  baltimore_property_district_summary
) |>
  dplyr::mutate(
    county = "Baltimore city",
    state = "MD",
    .before = all_of("geography")
  )

usethis::use_data(baltimore_property_summary, overwrite = TRUE)
