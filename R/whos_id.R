
whos_id <- function() {
  id <- Sys.getenv('WHOS_ID')
  if (identical(id, "")) {
    stop("Please set env var WHOS_ID to your WHOS personal token identifier. E.g. `Sys.setenv(WHOS_ID=<your whos id>)`", call. = FALSE)
  }
  id
}
