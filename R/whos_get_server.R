
whos_get_server <- function(view) {
  server <- sprintf(
      "https://whos.geodab.eu/gs-service/services/essi/token/%s/view/gs-view-and(whos,gs-view-source(%s))/cuahsi_1_1.asmx?WSDL",
    user_token, view
  )
  server
}
