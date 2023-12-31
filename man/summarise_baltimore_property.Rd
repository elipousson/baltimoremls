% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarise_baltimore_property.R
\name{summarise_baltimore_property}
\alias{summarise_baltimore_property}
\alias{get_baltimore_property}
\alias{reframe_baltimore_property}
\title{Summarize Baltimore property data by intersection with another sf object}
\usage{
summarise_baltimore_property(
  property_data = NULL,
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
  call = caller_env()
)

get_baltimore_property(
  location = NULL,
  data = "baltimore_property",
  filename = "baltimore_property.gpkg",
  pkg = "mapbaltimore",
  cache_data = TRUE,
  crs = 2804,
  overwrite = FALSE
)

reframe_baltimore_property(
  property_data,
  .by = NULL,
  .cols = c("usegroup", "currland", "currimpr", "vacant_lot", "vacant_bldg", "grndrent"),
  .error_call = caller_env()
)
}
\arguments{
\item{property_data}{Property data to summarize. If \code{NULL}, get property data
for location with \code{\link[=get_baltimore_property]{get_baltimore_property()}}}

\item{location}{Optional location to use for property data. Ignored if
property_data is supplied. Defaults to \code{NULL}}

\item{input_sf}{Input sf object to join to property data. Must have a column
name matching id_col unless name is supplied. If input_sf is not supplied,
property data can be summarized based on existing variables or without any
grouping. Default: \code{NULL}}

\item{...}{Additional parameters passed to \code{\link[=get_baltimore_property]{get_baltimore_property()}} if
property_data is \code{NULL}}

\item{.by}{Passed to \code{\link[dplyr:reframe]{dplyr::reframe()}}}

\item{id_col}{Required string. Identifier column name, Default: 'name'}

\item{.fn}{Summary function to use, Default: \code{reframe_baltimore_property}}

\item{suffix}{Suffix passed to \code{\link[sf:st_join]{sf::st_join()}} when joining property_data to
input_sf, Default: c("_prop", "")}

\item{placement}{Placement for property data, Default: "surface" which uses
\code{\link[sf:geos_unary]{sf::st_point_on_surface()}}, "centroid" is also supported for
\code{\link[sf:geos_unary]{sf::st_centroid()}}}

\item{name}{Name to use for data if .id column is not present in input_sf or
property_data, Default: \code{NULL}}

\item{keep_geometry}{If \code{TRUE}, retain geometry for property data when
summarizing data by input_sf. Default: \code{FALSE}}

\item{geography}{Geography name, Default: \code{NULL}}

\item{county}{County name, Default: 'Baltimore city'}

\item{state}{State name, Default: 'MD'}

\item{join}{geometry predicate function with the same profile as \link[sf]{st_intersects}; see details}

\item{left}{logical; if \code{TRUE} return the left join, otherwise an inner join; see details.
see also \link[dplyr:mutate-joins]{left_join}}

\item{largest}{logical; if \code{TRUE}, return \code{x} features augmented with the fields of \code{y} that have the largest overlap with each of the features of \code{x}; see https://github.com/r-spatial/sf/issues/578}

\item{call}{The execution environment of a currently
running function, e.g. \code{caller_env()}. The function will be
mentioned in error messages as the source of the error. See the
\code{call} argument of \code{\link[rlang:abort]{abort()}} for more information.}

\item{data}{Data to cache.}

\item{filename}{File name to use for cached file. Defaults to name of data.
If the data is an sf object make sure to include the file type, e.g.
"data.gpkg", supported by \code{sf::write_sf()}. All other data is written to
rda with \code{readr::write_rds()}.}

\item{pkg}{The package names. Can include version requirements,
e.g. \code{"pkg (>= 1.0.0)"}.}

\item{cache_data}{If \code{TRUE} and cached data is not already available, call
\code{\link[=cache_baltimore_property]{cache_baltimore_property()}}. Defaults to \code{TRUE}.}

\item{crs}{Coordinate reference system.}

\item{overwrite}{Logical. Default \code{FALSE}. If \code{TRUE}, overwrite any existing
cached files that use the same filename.}

\item{.cols}{Required column names.}

\item{.error_call}{The execution environment of a currently
running function, e.g. \code{caller_env()}. The function will be
mentioned in error messages as the source of the error. See the
\code{call} argument of \code{\link[rlang:abort]{abort()}} for more information.}
}
\value{
A data frame or sf object (if keep_geometry is \code{TRUE})
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

\code{\link[=summarise_baltimore_property]{summarise_baltimore_property()}} enables the easy creation of a summary data
frame based on citywide or area property data. If an additional sf object is
supplied to input_sf, this object is joined to the property data to support
the creation of a grouped summary by planning area or other administrative
geography. This function is used to prepare the \code{baltimore_property_summary}
data included with this package. It is designed for use with the
\code{\link[=reframe_baltimore_property]{reframe_baltimore_property()}} summary function but an alternate function can
be supplied.
}
\seealso{
\code{\link[sf:st_join]{sf::st_join()}}
}
