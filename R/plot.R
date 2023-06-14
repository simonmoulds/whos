
setOldClass("whos_sites")

#' View `whos_sites` object interactively
#'
#' A method for the generic function `mapview::mapView` which
#' allows users to view WHOS sites interactively.
#'
#' @param x An object of class whos_sites.
#' @param ... Additional arguments passed to `mapview::mapView`.
#'
#' @export
setMethod("mapView",
          signature(x = "whos_sites"),
          function(x, ...) {
            x <- sf::st_as_sf(x, crs = 4326, coords = c("Longitude", "Latitude"))
            x$view <- "uk-nrfa"
            dots <- list(...)
            if (!"label" %in% names(dots)) {
              dots[["label"]] <- "SiteName"
            }
            if (!"zcol" %in% names(dots)) {
              dots[["zcol"]] <- "view"
            }
            if (!"layer.name" %in% names(dots)) {
              dots[["layer.name"]] <- "WHOS view"
            }
            args <- c(list(x = x), dots)
            do.call(mapview::mapView, args)
            ## mapview::mapView(x, label = "SiteCode", ...)
          })
