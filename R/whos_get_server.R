#' Get server
#'
#' Retrieve the URL for a given WHOS view.
#'
#' @param view WHOS view.
#'
#' @return Character
#' @export
whos_get_server <- function(view) {
  user_id <- whos_id()
  server <- sprintf(
      "https://whos.geodab.eu/gs-service/services/essi/token/%s/view/gs-view-and(whos,gs-view-source(%s))/cuahsi_1_1.asmx?WSDL",
    user_id, view
  )
  server
}
