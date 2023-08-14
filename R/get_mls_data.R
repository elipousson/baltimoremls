#' Get Bright MLS Data for Baltimore City
#'
#' [get_mls_data()] is designed to use data from an internal data file provided
#' to the Baltimore City Department of Planning by Live Baltimore for analysis
#' of property sales data.
#'
#' @param data File path or URL for Bright MLS Sales data.
#' @inheritParams getdata::get_location_data
#' @param collapse_types If `TRUE` (default), create new ownership type and
#'   transaction type columns that collapse less frequent categories into a
#'   single combined other category.
#' @param ... Additional parameters passed to [getdata::get_location_data()]
#' @returns A sf data frame.
#' @rdname get_mls_data
#' @export
#' @importFrom getdata get_location_data rename_with_xwalk make_xwalk_list
#' @importFrom sf st_make_valid
#' @importFrom janitor clean_names
get_mls_data <- function(data,
                         location = NULL,
                         crop = TRUE,
                         trim = FALSE,
                         collapse_types = TRUE,
                         ...,
                         crs = 3857) {
  mls_data <- getdata::get_location_data(
    data = data,
    location = location,
    crop = crop,
    trim = trim,
    ...,
    crs = crs
  )

  mls_data <- sf::st_make_valid(mls_data)

  mls_data <- getdata::rename_with_xwalk(
    mls_data,
    xwalk = getdata::make_xwalk_list(mls_dictionary),
    .strict = FALSE
    # FIXME: Add .name_repair to rename_with_xwalk
  )

  mls_data <- janitor::clean_names(mls_data)

  if (collapse_types) {
    mls_data <- collapse_mls_transaction_type(mls_data)
    mls_data <- collapse_mls_ownership_type(mls_data)
  }

  mls_data
}
