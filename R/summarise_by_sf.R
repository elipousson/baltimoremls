#' Summarise by sf
#'
#' @noRd
summarise_by_sf <- function(data = NULL,
                            location = NULL,
                            input_sf = NULL,
                            ...,
                            .by = id_col,
                            id_col = "name",
                            .fn = reframe_baltimore_property,
                            suffix = c("_prop", ""),
                            placement = "surface",
                            name = NULL,
                            keep_geometry = FALSE,
                            geography = NULL,
                            county = "Baltimore city",
                            state = "MD",
                            join = sf::st_intersects,
                            left = TRUE,
                            largest = FALSE,
                            data_arg = caller_arg(data),
                            call = caller_env()) {
  if (!is.null(input_sf)) {
    input_sf <- .st_transform_ext(
      input_sf,
      crs = data,
      allow_null = FALSE
    )

    stopifnot(is_string(id_col))

    is_point_input <- sf::st_is(input_sf, "POINT")

    if (!is_point_input && identical(placement, "centroid")) {
      cli::cli_progress_step("Getting centroids for {.arg {data_arg}}")
      data <- suppressWarnings(sf::st_centroid(data))
    } else if (!is_point_input && identical(placement, "surface")) {
      cli::cli_progress_step("Getting point on surface geometry for {.arg {data_arg}}")
      data <- suppressWarnings(sf::st_point_on_surface(data))
    }

    cli::cli_progress_step("Joining {.arg {data_arg}} to {.arg input_sf}")

    data <- sf::st_join(
      x = data,
      y = dplyr::select(input_sf, dplyr::all_of(c(id_col, .by))),
      suffix = suffix,
      ...,
      join = join,
      left = left,
      largest = largest
    )
  }

  if (!keep_geometry) {
    data <- sf::st_drop_geometry(data)
  }

  if (!has_name(data, id_col)) {
    if (!is.null(name)) {
      data <- dplyr::mutate(
        data,
        "{id_col}" := name
      )
    } else {
      cli_abort(
        "{.arg name} is required if {.arg input_sf} and {.arg {data_arg}}
        are both missing a column matching {.arg id_col}: {.val {id_col}}",
        call = call
      )
    }
  }

  data_summary <- exec(.fn, data = data, .by = .by)

  vctrs::vec_cbind(
    data_summary,
    as.data.frame(
      vctrs::list_drop_empty(
        list(
          geography = geography,
          county = county,
          state = state
        )
      )
    )
  )
}

#' @noRd
reframe_ext <- function(.data,
                        ...,
                        .by = NULL,
                        .cols = NULL,
                        .data_arg = caller_arg(data),
                        .error_call = caller_env()) {
  if (!all(has_name(.data, .cols))) {
    cli_abort(
      "One or more required column names {.val {.cols}} can't be found in {.arg {.data_arg}}",
      call = .error_call
    )
  }

  dplyr::reframe(
    .data = .data,
    ...,
    .by = .by
  )
}
