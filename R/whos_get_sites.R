
#' Get sites from WHOS
#'
#' This function gets the table of sites from a given WHOS server
#'
#' @param server The URL of the WHOS server
#'
#' @return A tibble.
#' @export
whos_get_sites <- function(server, ...) {
  sites <- GetSites(server, ...)
  sites
}
