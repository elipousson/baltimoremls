.onLoad <- function(lib, pkg) {
  utils::data(
    list = c("mls_dictionary"),
    package = pkg,
    envir = parent.env(environment())
  )
}

#' Select MLS columns for creation of a simplified summary table
#'
#' @keywords internal
#' @rdname select_mls_cols
#' @export
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr select any_of
select_mls_cols <- function(data,
                            ...,
                            .cols = c(
                              "list_price", "close_price", "close_year",
                              "sale_type", "owner_type", "dom", "cdom"
                            ),
                            .drop_cols = NULL,
                            .drop_geometry = FALSE) {
  if (.drop_geometry) {
    data <- sf::st_drop_geometry(data)
  }

  dplyr::select(
    data,
    dplyr::any_of(.cols),
    -dplyr::any_of(.drop_cols),
    ...
  )
}

#' Label MLS data using `mls_dictionary` and [getdata::label_with_xwalk()]
#'
#' @param cols Column names, Default: c("label", "nm")
#' @seealso
#'  [getdata::format_data()], [getdata::make_xwalk_list()]
#' @rdname label_mls_data
#' @export
#' @importFrom vctrs vec_rbind
#' @importFrom dplyr select all_of filter
#' @importFrom getdata label_with_xwalk make_xwalk_list
label_mls_data <- function(data, cols = c("label", "nm")) {
  label_dict <- vctrs::vec_rbind(
    dplyr::select(
      mls_dictionary,
      dplyr::all_of(cols)
    ),
    data.frame(
      label = c("Transaction type", "Owner type"),
      nm = c("sale_type", "owner_type")
    )
  )

  label_dict <- dplyr::filter(
    label_dict,
    .data[[cols[[2]]]] %in% names(data)
  )

  getdata::label_with_xwalk(
    data,
    xwalk = getdata::make_xwalk_list(label_dict, cols = rev(cols))
  )
}


#' Scale MLS price values by a supplied factor value
#'
#' @keywords internal
#' @rdname format_scale_mls_prices
#' @export
#' @importFrom dplyr mutate across any_of
format_scale_mls_prices <- function(data,
                                    ...,
                                    factor = 1000,
                                    cols = c(
                                      "list_price", "close_price",
                                      "original_list_price", "sales_price"
                                    )) {
  dplyr::mutate(
    data,
    dplyr::across(
      dplyr::any_of(cols),
      function(x) {
        x / factor
      }
    )
  )
}
