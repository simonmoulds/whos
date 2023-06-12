#' Get values
#'
#' Retrieve data for a given site.
#'
#' @param server Character. WHOS server
#' @param site_info Tibble.
#'
#' @return tibble
#' @export
whos_get_values <- function(server, site_info) {
  n_variables <- nrow(site_info)
  values_list <- vector(mode = "list", length = n_variables)
  for (i in 1:n_variables) {
    ## tibble(data.frame(...)) removes non-standard attributes
    metadata <- tibble(data.frame(site_info[i,]))
    values <- GetValues(server, metadata[["SiteCode"]], metadata[["VariableCode"]])
    values <- new_tibble(values, metadata = as.list(metadata))
    values_list[[i]] <- values
  }
  if (length(values_list) == 1) {
    return(values_list[[1]])
  } else {
    nms <- sapply(values_list, FUN = function(x) attr(x, "metadata")$VariableCode)
    names(values_list) <- nms
    return(values_list)
  }
}

#' Get metadata for object returned by `whos_get_values`
#'
#' @param x Tibble.
#'
#' @return list
#' @export
metadata <- function(x) {
  return(attr(x, "metadata"))
}
