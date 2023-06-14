
#' Get sites from WHOS
#'
#' This function gets the table of sites from a given WHOS server
#'
#' @param server The URL of the WHOS server
#' @param ... Additional arguments. None implemented.
#'
#' @return A tibble.
#' @export
whos_get_sites <- function(server, ...) {
  sites <- GetSites(server, ...)
  sites <- new_tibble(
    sites, server = server,
    class = "whos_sites"
  )
  sites
}
