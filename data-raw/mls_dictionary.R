## code to prepare `mls_dictionary` dataset goes here

library(tidyverse)

data_dict <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1O7H7W7JJPI954K9R0kICw9-TJd38pXGta7Pm-JrMf7o/edit#gid=1913138299",
  sheet = "dictionary"
)

mls_dictionary <- filter(data_dict, source == "MLS (Home Sales Data)") |>
  rename(name = variable) |>
  select(-c(source, levels, value_labels, na_values, na_range)) |>
  janitor::clean_names()

mls_dictionary <- mls_dictionary |>
  mutate(
    nm = janitor::make_clean_names(label)
  )

usethis::use_data(mls_dictionary, overwrite = TRUE)
