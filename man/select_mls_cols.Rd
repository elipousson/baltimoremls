% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{select_mls_cols}
\alias{select_mls_cols}
\title{Select MLS columns for creation of a simplified summary table}
\usage{
select_mls_cols(
  data,
  ...,
  .cols = c("list_price", "close_price", "close_year", "sale_type", "owner_type", "dom",
    "cdom"),
  .drop_cols = NULL,
  .drop_geometry = FALSE
)
}
\description{
Select MLS columns for creation of a simplified summary table
}
\keyword{internal}
