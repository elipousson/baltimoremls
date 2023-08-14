#' Baltimore Properties by Neighborhood
#'
#' A summary of July 2023 real property data downloaded from Baltimore City with
#' [mapbaltimore::cache_baltimore_property()] by neighborhood and City Council
#' district.
#'
#' @format A data frame with 278 rows and 7 variables:
#' \describe{
#'   \item{`type`}{Neighborhood (2010) type}
#'   \item{`name`}{County, neighborhood (2010), or City Council District name}
#'   \item{`num_properties`}{Total number of properties}
#'   \item{`num_residential`}{Total number of properties with usecode attribute of R}
#'   \item{`perc_residential`}{num_residential as a percent of total properties}
#'   \item{`median_land_value`}{Median value for currland attribute (removing NA values and replacing 0 with NA)}
#'   \item{`median_imprv_value`}{Median value for currimpr attribute (removing NA values and replacing 0 with NA)}
#'   \item{`county`}{County name}
#'   \item{`state`}{State name}
#'   \item{`geography`}{Geography name}
#'}
"baltimore_property_summary"
