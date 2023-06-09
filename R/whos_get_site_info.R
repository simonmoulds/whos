#' Get site information
#'
#' Retrieve metadata for a given WHOS site.
#'
#' @param server Character. WHOS server
#' @param site Character. Site code
#' @param ... Additional arguments. None implemented.
#'
#' @return tibble
#' @export
whos_get_site_info <- function(server, site, ...) {
  info <- GetSiteInfo(server, site)
  info
}
