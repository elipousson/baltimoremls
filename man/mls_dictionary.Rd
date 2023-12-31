% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{mls_dictionary}
\alias{mls_dictionary}
\title{Baltimore City Bright MLS Sales Data Dictionary}
\format{
A data frame with 68 rows and 15 variables:
\describe{
\item{\code{pos}}{Position}
\item{\code{name}}{Source column name}
\item{\code{label}}{Short label}
\item{\code{label_long}}{Long format label}
\item{\code{notes}}{Notes}
\item{\code{derived_from}}{Derived from column name}
\item{\code{col_type}}{Column type}
\item{\code{missing}}{Number of missing values}
\item{\code{class}}{Column class}
\item{\code{type}}{character COLUMN_DESCRIPTION}
\item{\code{n_na}}{Number of NA values}
\item{\code{missing_notes}}{Notes on missing values}
\item{\code{unique_values}}{Number of unique values}
\item{\code{range}}{Range}
\item{\code{nm}}{Short name (created with \code{\link[janitor:make_clean_names]{janitor::make_clean_names()}})}
}
}
\usage{
mls_dictionary
}
\description{
A data dictionary for an internal dataset from the Bright MLS property
listing service provided to Baltimore City agencies by Live Baltimore. The
dictionary includes labels for each attribute and limited information on the
use of different attributes for analysis. Several attributes are missing all
data from 2022.
}
\details{
DETAILS
}
\keyword{datasets}
