#' Baltimore Properties by Neighborhood
#'
#' A summary of July 2023 real property data downloaded from Baltimore City with
#' [mapbaltimore::cache_baltimore_property()] by neighborhood and City Council
#' district.
#'
#' @format A data frame with 278 rows and 7 variables:
#' \describe{
#'   \item{`name`}{County, neighborhood (2010), City Council District, or Planning District name}
#'   \item{`type`}{Type (for neighborhood (2010) geography only)}
#'   \item{`num_properties`}{Total number of properties}
#'   \item{`num_vacant_lot`}{Total number of properties with no improvements}
#'   \item{`num_vacant_bldg`}{Total number of vacant buildings}
#'   \item{`num_buildings`}{Total properties with improvements}
#'   \item{`num_residential_bldg`}{Number of improved properties with usecode value "R"}
#'   \item{`num_ground_rent`}{Number of properties with ground rent greater than 0}
#'   \item{`perc_residential_bldg`}{Residential buildings as a share of all buildings}
#'   \item{`perc_vacant_lot`}{Vacant lots as a share of all properties}
#'   \item{`perc_vacant_bldg`}{Vacant buildings as a share of all buildings}
#'   \item{`perc_ground_rent`}{Share of all properties with ground rent greater than 0}
#'   \item{`median_land_value`}{Median value for currland attribute (removing NA values and replacing 0 with NA)}
#'   \item{`median_imprv_value`}{Median value for currimpr attribute (removing NA values and replacing 0 with NA)}
#'   \item{`county`}{County name}
#'   \item{`state`}{State name}
#'   \item{`geography`}{Geography name}
#' }
"baltimore_property_summary"


#' Baltimore City Bright MLS Sales Data Dictionary
#'
#' A data dictionary for an internal dataset from the Bright MLS property
#' listing service provided to Baltimore City agencies by Live Baltimore. The
#' dictionary includes labels for each attribute and limited information on the
#' use of different attributes for analysis. Several attributes are missing all
#' data from 2022.
#'
#' @format A data frame with 68 rows and 15 variables:
#' \describe{
#'   \item{`pos`}{Position}
#'   \item{`name`}{Source column name}
#'   \item{`label`}{Short label}
#'   \item{`label_long`}{Long format label}
#'   \item{`notes`}{Notes}
#'   \item{`derived_from`}{Derived from column name}
#'   \item{`col_type`}{Column type}
#'   \item{`missing`}{Number of missing values}
#'   \item{`class`}{Column class}
#'   \item{`type`}{character COLUMN_DESCRIPTION}
#'   \item{`n_na`}{Number of NA values}
#'   \item{`missing_notes`}{Notes on missing values}
#'   \item{`unique_values`}{Number of unique values}
#'   \item{`range`}{Range}
#'   \item{`nm`}{Short name (created with [janitor::make_clean_names()])}
#'}
#' @details DETAILS
"mls_dictionary"
