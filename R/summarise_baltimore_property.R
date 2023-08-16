#' Summarize Baltimore property data by intersection with another sf object
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' [summarise_baltimore_property()] enables the easy creation of a summary data
#' frame based on citywide or area property data. If an additional sf object is
#' supplied to input_sf, this object is joined to the property data to support
#' the creation of a grouped summary by planning area or other administrative
#' geography. This function is used to prepare the `baltimore_property_summary`
#' data included with this package. It is designed for use with the
#' [reframe_baltimore_property()] summary function but an alternate function can
#' be supplied.
#'
#' @param property_data Property data to summarize. If `NULL`, get property data
#'   for location with [get_baltimore_property()]
#' @param location Optional location to use for property data. Ignored if
#'   property_data is supplied. Defaults to `NULL`
#' @param input_sf Input sf object to join to property data. Must have a column
#'   name matching id_col unless name is supplied. If input_sf is not supplied,
#'   property data can be summarized based on existing variables or without any
#'   grouping. Default: `NULL`
#' @param ... Additional parameters passed to [get_baltimore_property()] if
#'   property_data is `NULL`
#' @param .by Grouping variables, Default: id_col
#' @param id_col Required string. Identifier column name, Default: 'name'
#' @param suffix Suffix passed to [sf::st_join()] when joining property_data to
#'   input_sf, Default: c("_prop", "")
#' @param placement Placement for property data, Default: "surface" which uses
#'   [sf::st_point_on_surface()], "centroid" is also supported for
#'   [sf::st_centroid()]
#' @param name Name to use for data if .id column is not present in input_sf or
#'   property_data, Default: `NULL`
#' @param .fn Summary function to use, Default: `reframe_baltimore_property`
#' @param keep_geometry If `TRUE`, retain geometry for property data when
#'   summarizing data by input_sf. Default: `FALSE`
#' @param geography Geography name, Default: `NULL`
#' @param county County name, Default: 'Baltimore city'
#' @param state State name, Default: 'MD'
#' @inheritParams sf::st_join
#' @inheritParams rlang::args_error_context
#' @returns A data frame or sf object (if keep_geometry is `TRUE`)
#' @seealso
#'   [sf::st_join()]
#' @rdname summarise_baltimore_property
#' @export
#' @importFrom sf st_intersects st_centroid st_point_on_surface st_join st_drop_geometry
#' @importFrom cli cli_progress_step
#' @importFrom dplyr select all_of mutate
#' @importFrom vctrs vec_cbind list_drop_empty
summarise_baltimore_property <- function(property_data = NULL,
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
                                         call = caller_env()) {
  if (is.null(property_data)) {
    cli::cli_progress_step("Reading property data from cache")
    property_data <- get_baltimore_property(location = location, ...)
  }

  if (!is.null(input_sf)) {
    input_sf <- .st_transform_ext(
      input_sf,
      crs = property_data,
      allow_null = FALSE
    )

    stopifnot(is_string(id_col))

    if (placement == "centroid") {
      cli::cli_progress_step("Getting centroids for property data")
      property_data <- suppressWarnings(sf::st_centroid(property_data))
    } else if (placement == "surface") {
      cli::cli_progress_step("Getting point on surface geometry for property data")
      property_data <- suppressWarnings(sf::st_point_on_surface(property_data))
    }

    cli::cli_progress_step("Joining property data to {.arg input_sf}")

    property_data <- sf::st_join(
      x = property_data,
      y = dplyr::select(input_sf, dplyr::all_of(c(id_col, .by))),
      suffix = suffix,
      join = join,
      left = left,
      largest = largest
    )
  }

  if (!keep_geometry) {
    property_data <- sf::st_drop_geometry(property_data)
  }

  if (!has_name(property_data, id_col)) {
    if (!is.null(name)) {
      property_data <- dplyr::mutate(
        property_data,
        "{id_col}" := name
      )
    } else {
      cli_abort(
        "{.arg name} is required if {.arg input_sf} and {.arg property_data}
        are both missing a column matching {.arg id_col}: {.val {id_col}}",
        call = call
      )
    }
  }

  property_summary <- exec(.fn, property_data = property_data, .by = .by)

  vctrs::vec_cbind(
    property_summary,
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

#' @name get_baltimore_property
#' @rdname summarise_baltimore_property
#' @inheritParams filenamr::list_pkg_cachedata
#' @inheritParams mapbaltimore::cache_baltimore_property
#' @param cache_data If `TRUE` and cached data is not already available, call
#'   [cache_baltimore_property()]. Defaults to `TRUE`.
#' @export
#' @importFrom getdata get_location_data
#' @importFrom filenamr list_pkg_cachedata
get_baltimore_property <- function(location = NULL,
                                   data = "baltimore_property",
                                   filename = "baltimore_property.gpkg",
                                   pkg = "mapbaltimore",
                                   cache_data = TRUE,
                                   crs = 2804,
                                   overwrite = FALSE) {
  has_cache_data <- TRUE

  if (is_string(data)) {
    has_cache_data <- data %in% filenamr::list_pkg_cachedata(pkg)[["item"]]
  }

  if (cache_data && !has_cache_data) {
    check_installed("mapbaltimore")

    mapbaltimore::cache_baltimore_property(
      location = location,
      filename = filename,
      crs = crs,
      overwrite = overwrite
    )
  }

  getdata::get_location_data(
    data = data,
    pkg = pkg,
    location = location,
    crs = crs
  )
}

#' @name reframe_baltimore_property
#' @rdname summarise_baltimore_property
#' @param .by Passed to [dplyr::reframe()]
#' @param .cols Required column names.
#' @export
#' @importFrom dplyr reframe n
#' @importFrom stats median
reframe_baltimore_property <- function(property_data,
                                       .by = NULL,
                                       .cols = c(
                                         "usegroup", "currland", "currimpr",
                                         "vacant_lot", "vacant_bldg", "grndrent"
                                       ),
                                       call = caller_env()) {
  if (!all(has_name(property_data, .cols))) {
    cli_abort(
      "One or more required column names {.val {.cols}} can't be found in {.arg property_data}",
      call = call
    )
  }

  dplyr::reframe(
    property_data,
    # counts
    num_properties = dplyr::n(),
    num_vacant_lot = sum(vacant_lot),
    num_vacant_bldg = sum(vacant_bldg),
    num_buildings = num_properties - num_vacant_lot,
    num_residential_bldg = sum((usegroup %in% "R") & is_false(vacant_lot)),
    num_ground_rent = sum(grndrent > 0),
    # percent share
    perc_residential_bldg = round(num_residential_bldg / num_buildings, digits = 2),
    perc_vacant_lot = num_vacant_lot / num_properties,
    perc_vacant_bldg = num_vacant_bldg / num_buildings,
    perc_ground_rent = num_ground_rent / num_properties,
    # median values
    median_land_value = stats::median(currland, na.rm = TRUE),
    median_imprv_value = stats::median(currimpr, na.rm = TRUE),
    .by = .by
  )
}
