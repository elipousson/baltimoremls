#' Summarise MLS sales data by area
#'
#' @noRd
summarise_mls <- function(data = NULL,
                          location = NULL,
                          input_sf = NULL,
                          ...,
                          .by = id_col,
                          id_col = "name",
                          .fn = reframe_mls,
                          suffix = c("_mls", ""),
                          placement = "surface",
                          name = NULL,
                          keep_geometry = FALSE,
                          geography = NULL,
                          county = "Baltimore city",
                          state = "MD",
                          join = sf::st_intersects,
                          left = TRUE,
                          largest = FALSE,
                          call = caller_env()) {
  if (is_string(data)) {
    cli::cli_progress_step("Reading {.arg data} from file")
    data <- get_mls_data(data = data, location = location, ...)
  }

  summarise_by_sf(
    data = data,
    location = location,
    input_sf = input_sf,
    ...,
    .by = .by,
    id_col = id_col,
    .fn = .fn,
    suffix = suffix,
    placement = placement,
    name = name,
    keep_geometry = keep_geometry,
    geography = geography,
    county = county,
    state = state,
    join = join,
    left = left,
    largest = largest,
    call = call
  )
}

#' @noRd
reframe_mls <- function(data, ..., .by = NULL, .cols = NULL, .error_call = caller_env()) {
  reframe_ext(
    data,
    num_sales = n(),
    mean_close_price = mean(close_price),
    median_close_price = stats::median(close_price),
    ...,
    .by = .by,
    .cols = "close_price",
    .error_call = .error_call
  )
}
