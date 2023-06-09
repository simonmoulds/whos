#' Get values
#'
#' Retrieve data for a given site.
#'
#' @param server Character. WHOS server
#' @param site Character. Site code
#' @param variable Character. Variable code
#'
#' @return tibble
#' @export
whos_get_values <- function(server, site, variable) {
  values <- GetValues(server, site, variable)
  values
}
