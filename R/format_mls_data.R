#' Format MLS data
#'
#' @description
#' [format_mls_data()] calls [format_mls_prices()],
#' [format_mls_price_per_area()], and [format_mls_dates()]
#'
#' Both [collapse_mls_ownership_type()] and [collapse_mls_transaction_type()]
#' are called by [get_mls_data()] when `collapse_types = TRUE`
#'
#' @param data A data frame of MLS data to format.
#' @param dom_col,cdom_col Days on market and cumulative days on market columns.
#'   Defaults to "dom" and "cdom".
#' @param original_list_price_col,sale_price_col Column names for the original
#'   list price and sales price. Defaults to "original_list_price" and
#'   "sales_price".
#' @returns A data frame.
#' @seealso
#'  [naniar::replace_with_na()]
#'  [dplyr::mutate()], [dplyr::distinct()]
#' @rdname format_mls_data
#' @export
#' @importFrom naniar replace_with_na
#' @importFrom dplyr mutate distinct
format_mls_data <- function(data,
                            quarter_type = "date_last",
                            ...) {
  data <- format_mls_prices(data, ...)

  data <- format_mls_price_per_area(data, ...)

  data <- format_mls_dates(data, quarter_type = quarter_type, ...)

  if (has_name(data, "reporting_region")) {
    data <- naniar::replace_with_na(
      data,
      replace = list("reporting_region" = "")
    )
  }

  # FIXME: Double-check that this input data has had duplicates removed:
  # If not add something like: dplyr::distinct(data, mls_number, .keep_all = TRUE)
  data
}

#' @name format_mls_dates
#' @rdname format_mls_data
#' @param list_date_col List date column name, Default: 'listing_date'
#' @param close_date_col PARAM_DESCRIPTION, Default: 'close_date'
#' @param contract_date_col Contract date column name, Default: 'contract_date'
#' @param with_year Passed to [lubridate::quarter()] Default: `TRUE`
#' @inheritParams lubridate::parse_date_time
#' @export
#' @importFrom dplyr mutate
#' @importFrom lubridate mdy year month quarter
format_mls_dates <- function(data,
                             list_date_col = "listing_date",
                             close_date_col = "close_date",
                             contract_date_col = "contract_date",
                             orders = "ymd HMS",
                             tz = "UTC",
                             quarter_type = "date_last",
                             dom_col = "dom",
                             cdom_col = "cdom") {
  stopifnot(
    all(has_name(data, c(list_date_col, close_date_col)))
  )

  data <- dplyr::mutate(
    data,
    "{list_date_col}" := lubridate::parse_date_time(.data[[list_date_col]], orders, tz),
    list_year = lubridate::year(.data[[list_date_col]]),
    list_month = lubridate::month(.data[[list_date_col]]),
    list_quarter = lubridate::quarter(.data[[list_date_col]], type = quarter_type),
    "{close_date_col}" = lubridate::parse_date_time(.data[[close_date_col]], orders, tz),
    close_year = lubridate::year(.data[[close_date_col]]),
    close_month = lubridate::month(.data[[close_date_col]]),
    close_quarter = lubridate::quarter(.data[[close_date_col]], type = quarter_type)
  )

  if (has_name(data, contract_date_col)) {
    data <- dplyr::mutate(
      data,
      "{contract_date_col}" := lubridate::parse_date_time(.data[[contract_date_col]], orders, tz)
    )
  }

  data
}

#' @rdname format_mls_data
#' @name format_mls_prices
#' @export
format_mls_prices <- function(data,
                              original_list_price_col = "original_list_price",
                              sale_price_col = "sales_price",
                              ...) {
  dplyr::mutate(
    data,
    ratio_sold_list = .data[[sale_price_col]] / .data[[original_list_price_col]],
    diff_list_sale_price = .data[[sale_price_col]] - .data[[original_list_price_col]]
  )
}

#' @rdname format_mls_data
#' @name format_mls_price_per_area
#' @param price_per_area_unit_col Column name to add for derived price per area
#'   unit column (based on supplied `area_col` parameter).
#' @export
format_mls_price_per_area <- function(data,
                                      sale_price_col = "sales_price",
                                      price_per_area_unit_col = "per_sq_ft_price",
                                      range = c(0, 250),
                                      na_values = c(0, 1),
                                      area_col = "interior_sq_ft") {
  stopifnot(
    all(has_name(data, c(sale_price_col, area_col)))
  )

  if (!has_name(data, area_col)) {
    cli_warn(
      "{.arg data} must have a column named {.val {area_col}}
      to derive a price per unit of area."
    )
    return(data)
  }

  data <- naniar::replace_with_na(
    data,
    replace = set_names(list(na_values), area_col)
  )

  data <- dplyr::mutate(
    data,
    "{price_per_area_unit_col}" := dplyr::if_else(
      !is.na(.data[[area_col]]),
      .data[[sale_price_col]] / .data[[area_col]],
      0
    ),
    # Remove outlying values for price per square foot based on incorrect
    # interior sq ft data
    "{price_per_area_unit_col}" := dplyr::case_when(
      .data[[price_per_area_unit_col]] > max(range) ~ 0,
      .data[[price_per_area_unit_col]] < min(range) ~ 0,
      .default = .data[[price_per_area_unit_col]]
    )
  )

  data <- naniar::replace_with_na(
    data,
    replace = set_names(list(0), price_per_area_unit_col)
  )

  data
}

#' @rdname format_mls_data
#' @name collapse_mls_transaction_type
#' @export
#' @importFrom dplyr mutate
#' @importFrom forcats fct_infreq fct_collapse
collapse_mls_transaction_type <- function(data,
                                          transaction_type_col = "transaction_type",
                                          type_col = "sale_type") {
  # FIXME: Suppressing warnings is only needed when reading a subset of MLS data
  # that may not have all transaction types represented in the data
  suppressWarnings(
    dplyr::mutate(
      data,
      "{type_col}" := forcats::fct_infreq(.data[[transaction_type_col]]),
      "{type_col}" := forcats::fct_collapse(
        .data[[type_col]],
        "Standard" = c("Standard"),
        "Auction" = c("Auction, Standard", "Auction"),
        "Foreclosure" = "Foreclosure",
        other_level = "Other type"
      )
    )
  )
}

#' @rdname format_mls_data
#' @name collapse_mls_ownership_type
#' @export
#' @importFrom dplyr mutate
#' @importFrom forcats fct_infreq fct_collapse
collapse_mls_ownership_type <- function(data,
                                        ownership_type_col = "ownership_type",
                                        collapsed_col = "owner_type",
                                        other_level = "Other (Condo, Coop, etc.)") {
  # FIXME: Suppressing warnings is only needed when reading a subset of MLS data
  # that may not have all transaction types represented in the data
  suppressWarnings(
    dplyr::mutate(
      data,
      "{collapsed_col}" := forcats::fct_infreq(.data[[ownership_type_col]]),
      "{collapsed_col}" := forcats::fct_collapse(
        .data[[collapsed_col]],
        "Fee Simple" = "Fee Simple",
        "Ground Rent" = "Ground Rent",
        other_level = other_level
      )
    )
  )
}
