#' whos: Retrieve data from the WMO Hydrological Observing System
#'
#' @aliases NULL whos-package
#' @import checkmate
#' @import httr
#' @import mapview
#' @import methods
#' @import sf
#' @import stats
#' @importFrom tibble tibble as_tibble new_tibble
#' @import XML
#'
#' @examples
#' \dontrun{
#' ## Set WHOS ID
#' my_id <- "whos-8b5fc809-75ef-4877-baff-04ae9e227243"
#' Sys.setenv(WHOS_ID=my_id)
#' whos_id()
#'
#' views <- whos_get_views()
#' server <- whos_get_server(view = "uk-nrfa")
#' sites <- whos_get_sites(server)
#'
#' ## To retrieve data, WHOS expects a value from the "SiteCode"
#' ## column. Here we find stations on the River Cherwell:
#' cherwell_sites <- grep("Cherwell", sites$SiteName)
#' site_code <- sites[cherwell_sites[1], "SiteCode", drop = TRUE]
#'
#' ## Get list of site_info measured at this site
#' site_info <- whos_get_site_info(server, site = site_code)
#' site_info
#'
#' ## Let's retrieve data for rainfall and streamflow
#' daily_Q_var_code <- site_info$VariableCode[1]
#' daily_P_var_code <- site_info$VariableCode[8]
#'
#' streamflow <- whos_get_values(
#'   server,
#'   site_code = site_code,
#'   variable_code = daily_Q_var_code,
#'   site_info = site_info
#' )
#' metadata(streamflow)
#' }
"_PACKAGE"
