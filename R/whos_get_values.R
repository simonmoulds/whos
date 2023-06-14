check_site_info <- function(site_info, variable_code, site_code, server) {
  if (!is.null(site_info)) {
    ## Check `variable_code` present
    if (!all(variable_code %in% site_info$VariableCode)) {
      stop("Argument `site_info` does not have information for the variables specified in `variable_code`")
      site_info <- NULL
    }
    ## Check `site_code` present
    if (!site_code %in% site_info$SiteCode) {
      stop("Argument `site_info` does not have information for the site specified in `site_code`")
    }
    ## Check server matches
    site_info_server <- attr(site_info, "server")
    if (server != site_info_server) {
      stop(
        "The URL from which `site_info` was downloaded is not the same as that specified by argument `server`"
      )
    }
  }
  TRUE
}

#' Get values
#'
#' Retrieve data for a given site.
#'
#' @param server Character. The WHOS server.
#' @param site_code Character. The site code for which data should be
#'   retrieved. This should be taken from the `SiteCode` column in the
#'   tibble returned from [whos_get_sites()].
#' @param variable_code Character. The variable code(s) for which data
#'   should be retrieved. These should be taken from the `VariableCode`
#'   column in the tibble returned from [whos_get_site_info()].
#' @param site_info Tibble. Site metadata returned from
#'   [whos_get_site_info()]. This is optional to avoid the need to
#'   download site info if it is not required. If provided, it will
#'   trigger a number of checks on the compatability of the other
#'   arguments, and the corresponding metadata will be added as an
#'   attribute to the returned tibble(s).
#' @param ... Additional arguments. None implemented.
#'
#' @return Tibble or list.
#' @export
whos_get_values <- function(server, site_code, variable_code, site_info = NULL, ...) {
  ## Check arguments
  assert_character(
    server, any.missing = FALSE, len = 1
  )
  assert_character(
    site_code, any.missing = FALSE, len = 1
  )
  assert_character(
    variable_code, any.missing = FALSE, min.len = 1, unique = TRUE
  )
  assert_class(
    site_info, "whos_site_info", null.ok = TRUE
  )
  check_site_info(site_info, variable_code, site_code, server)
  ## Download variables
  n_variables <- length(variable_code)
  values_list <- vector(mode = "list", length = n_variables)
  for (i in 1:n_variables) {
    variable_code <- variable_code[i]
    metadata <- NULL
    if (!is.null(site_info)) {
      metadata <- site_info[site_info$VariableCode %in% variable_code, ]
      metadata <- tibble(data.frame(metadata))
    }
    values <- GetValues(server, site_code, variable_code)
    values <- new_tibble(
      values,
      metadata = as.list(metadata),
      class = "whos_values"
    )
    values_list[[i]] <- values
  }
  if (length(values_list) == 1) {
    return(values_list[[1]])
  } else {
    nms <- sapply(values_list, FUN = function(x) attr(x, "metadata")$VariableCode)
    names(values_list) <- nms
    class(values_list) <- c("whos_values_list", "list")
    return(values_list)
  }
}

#' @export
`[.whos_values_list` <- function(x, i, ...) {
  structure(NextMethod("["), class = class(x))
}

#' Get metadata for object returned by `whos_get_values`
#'
#' @param x Tibble.
#' @param ... Additional arguments. None implemented.
#'
#' @return list
#' @export
metadata <- function(x, ...) {
  UseMethod("metadata")
}

#' @export
metadata.whos_values <- function(x, ...) {
  return(attr(x, "metadata"))
}

#' @export
metadata.whos_values_list <- function(x, ...) {
  metadata_list <- list()
  for (i in 1:length(x)) {
    metadata_list[[i]] <- as_tibble(attr(x[[i]], "metadata"))
  }
  return(do.call("rbind", metadata_list))
}
