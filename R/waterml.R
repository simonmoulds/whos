#' @import XML
#' @import httr
WaterOneFlowNamespace <- function(version) {
  # WaterOneFlowNamespace
  #
  # A helper function that finds out the WaterOneFlow namespace information
  # based on the version number 1.0 or 1.1.
  #
  # @param version The version of the WaterOneFlow XML namespace. Must be either "1.0" or "1.1"
  # @return A list with the namespaces and corresponding prefixes. This namespace
  # information is important for correct parsing of the WaterML XML document.
  # @keywords WaterML
  # @export
  # @examples
  # ns <- WaterOneFlowNamespace("1.0")
  # ns <- WaterOneFlowNamespace("1.1")
  if (version == "1.0") {
    ns <- c(soap="http://schemas.xmlsoap.org/soap/envelope/",
            xsd="http://www.w3.org/2001/XMLSchema",
            xsi="http://www.w3.org/2001/XMLSchema-instance",
            sr="http://www.cuahsi.org/waterML/1.0/",
            gsr="http://www.cuahsi.org/his/1.0/ws/")
  } else {
    ns <- c(soap="http://schemas.xmlsoap.org/soap/envelope/",
            xsd="http://www.w3.org/2001/XMLSchema",
            xsi="http://www.w3.org/2001/XMLSchema-instance",
            sr="http://www.cuahsi.org/waterML/1.1/",
            gsr="http://www.cuahsi.org/his/1.1/ws/")
  }
  return(ns)
}

WaterMLVersion <- function(doc) {
  # WaterMLVersion
  #
  # A helper function that finds out the WaterML version from
  # the WaterML document. By default it checks for "http://www.opengis.net/waterml/2.0"
  # Otherwise it tries to detect "http://www.cuahsi.org/waterML/1.1/" (for WaterML 1.1) or
  # "http://www.cuahsi.org/WaterML/1.0/" (for WaterML 1.0)
  #
  # @param doc The XML document object
  # @return A character with the WaterML version: either 1.0, 1.1, or 2.0
  # @keywords WaterML
  # @export
  # @examples
  # library(httr)
  # library(XML)
  # url <- "http://www.waterml2.org/KiWIS-WML2-Example.wml"
  # response <- GET(url)
  # doc <- xmlParse(response)
  # version <- WaterMLVersion(doc)

  #check namespaces
  ns_list <- xmlNamespaceDefinitions(doc, simplify=TRUE)
  namespaces <- as.character(unlist(ns_list))

  wml_2_0_namespace <- "http://www.opengis.net/waterml/2.0"
  wml_1_1_namespace <- "http://www.cuahsi.org/waterML/1.1/"
  wml_1_0_namespace <- "http://www.cuahsi.org/waterML/1.0/"

  if (wml_2_0_namespace %in% namespaces) {
    return ("2.0")
  }

  if (wml_1_1_namespace %in% namespaces) {
    return ("1.1")
  }

  if (wml_1_0_namespace %in% namespaces) {
    return ("1.0")
  }

  #if not found assume 1.1
  return ("1.1")
}

WaterOneFlowVersion <- function(WSDL) {
  # WaterOneFlowVersion
  #
  # A helper function that finds out the WaterOneFlow service version from
  # the URL of the wsdl file. By default it checks for "cuahsi_1_0" or "cuahsi_1_1" in
  # the url. If that is not found, then the function checks the version inside the
  # WSDL file.
  #
  # @param WSDL The URL of the WSDL, for example http://icewater.usu.edu/MudLake/cuahsi_1_0.asmx?WSDL
  # @return A list with two items: Version (either 1.0 or 1.1), and Namespace
  # (either http://www.cuahsi.org/his/1.0/ws/ or http://www.cuahsi.org/his/1.1/ws/)
  # @keywords WaterML
  # @export
  # @examples
  # versionInfo <- WaterOneFlowVersion("http://icewater.usu.edu/MudLake/cuahsi_1_0.asmx?WSDL")
  # versionInfo$Version
  # versionInfo$Namespace

  #check CUAHSINamespace parameter
  validNamespace_1_0 <- "http://www.cuahsi.org/his/1.0/ws/"
  validNamespace_1_1 <- "http://www.cuahsi.org/his/1.1/ws/"

  if (grepl("cuahsi_1_1", WSDL)) {
    return(list(Version="1.1", Namespace=validNamespace_1_1))
  }
  if (grepl("cuahsi_1_0", WSDL)) {
    return(list(Version="1.0", Namespace=validNamespace_1_0))
  }

  #if not found then assume 1.0
  return(list(Version="1.0", Namespace=validNamespace_1_0))
}

GetServices <- function() {
  # GetServices
  #
  # This function gets the table of web services from the HIS Central catalog
  #
  # @import XML
  # @import httr
  # @keywords waterml
  # @export
  # @examples
  # GetServices()

  url <- "http://hiscentral.cuahsi.org/webservices/hiscentral.asmx/GetWaterOneFlowServiceInfo"

  download.time <- system.time(
    tryCatch({
      downloaded <- FALSE
      response <- GET(url)
      downloaded <- TRUE
    },error=function(e){
      warning(conditionMessage(e))
    })
  )

  if (!downloaded) {
    return(NULL)
  }
  status.code <- http_status(response)$category

  ######################################################
  # Parsing the WaterML XML Data                       #
  ######################################################
  doc <- tryCatch({
    xmlParse(response)
  }, warning = function(w) {
    warning("Error reading HIS Central Data: Bad XML format.")
    return(NULL)
  }, error = function(e) {
    warning("Error reading HIS Central Data: Bad XML format.")
    return(NULL)
  }
  )
  if (is.null(doc)) {
    return(NULL)
  }

  doc <- xmlRoot(doc, getDTD=FALSE, useInternalNodes = TRUE)

  N <- xmlSize(doc)

  colnames <- c("url","title","descriptionURL","organization","citation","abstract",
                "valuecount","variablecount","sitecount","id","networkName",
                "minLon","minLat","maxLon","maxLat")

  m <- matrix(ncol=15, nrow=N, dimnames=list(NULL, colnames))
  df <- as.data.frame(m)

  for(i in 1:N){
    element <- xmlToList(doc[[i]])
    #we replace NULL values with NA
    e <- lapply(element, function(x) {ifelse(is.null(x), NA, x)})
    df$url[i] <- e$servURL
    df$title[i] <- e$Title
    df$descriptionURL[i] <- e$ServiceDescriptionURL
    df$organization[i] <- e$organization
    df$citation[i] <- e$citation
    df$abstract[i] <- e$aabstract
    df$valuecount[i] <- e$valuecount
    df$sitecount[i] <- e$sitecount
    df$id[i] <- e$ServiceID
    df$networkName[i] <- e$NetworkName
    df$minLon[i] <- e$minx
    df$minLat[i] <- e$miny
    df$maxLat[i] <- e$maxx
    df$maxLon[i] <- e$maxy
  }
  return(df)
}

HISCentral_GetSites <- function(west=-180, south=-90, east=180, north=90,
                                serviceID=NULL, keyword=NULL, IncludeServerDetails=TRUE) {
  # HISCentral_GetSites
  #
  # This function gets the table of sites from the HIS Central catalog
  #
  # @import XML
  # @import httr
  # @param west The west longitude of the geographic
  #  bounding box in decimal degrees. Allowed values are between -180.0 and +180.0
  # @param south The south latitude of the geographic
  #  bounding box in decimal degrees. Allowed values are between -90.0 and +90.0
  # @param east The east longitude of the geographic
  #  bounding box in decimal degrees. Allowed values are between -180.0 and +180.0
  # @param north The north latitude of the geographic
  #  bounding box in decimal degrees. Allowed values are between -90.0 and +90.0
  # @param serviceID (optional): The ID of the service on HIS Central. To get the service ID,
  #  use the id column in the output of the GetServices() function.
  # @param keyword (optional): The concept keyword (common name of variable) for
  #  searching the sites on HIS Central. Examples include Temperature, Precipitation, Snow Depth,... If the Keyword is not
  #  specified then sites with any variable will be returned.
  # @param IncludeServerDetails (optional): If set to TRUE, then the output will
  # include the servCode and servURL for each site. If set to FALSE, then we assume
  # that all sites are from the same server and the servURL and servCode are not included.
  # @return a data.frame of sites. The data.frame has the following columns:
  # \itemize{
  # \item SiteName: The name of the site
  # \item SiteCode: A short unique code of the site
  # \item FullSiteCode: The complete unique code of the site in the format NETWORK:CODE.
  #               Use this value in the GetSiteInfo and GetValues functions
  # \item Latitude:  The WGS84 latitude in decimal degrees
  # \item Longitude: The WGS84 longitude in decimal degrees
  # \item ServCode: The code of the service in HIS Central. Same as the networkName in
  #                  the output from GetServices() function.
  #                  This column is only shown if IncludeServerDetails is TRUE.
  # \item ServURL:   The URL of the web service for this site as registered in HIS Central.
  #                  This column is only shown if IncludeServerDetails is TRUE.
  # }
  # @keywords waterml
  # @export
  # @examples
  # #Getting all sites from the (14.1E, 49.8N, 14.6E, 50.2N) bounding box from the GLDAS web service
  # sites <- HISCentral_GetSites(west=14.1, south=49.8, east=14.6, north=50.2, serviceID=262)

  catalog = "http://hiscentral.cuahsi.org/webservices/hiscentral.asmx/GetSitesInBox2"

  #create the URL
  servID = serviceID
  if (is.null(serviceID)) {
    servID=""
  }
  if (is.null(keyword)) {
    keyword=""
  }
  queryParameters <- list(xmin=west, ymin=south, xmax=east, ymax=north,
                          networkIDs=servID, conceptKeyword=keyword)
  url <- paste(catalog, "?", "&xmin=", west, "&ymin=", south, "&xmax=", east,
               "&ymax=", north, "networkIDs=", servID, "&conceptKeyword=", keyword,
               sep="")

  print(paste("searching sites from:", url, "..."))

  download.time <- system.time(
    tryCatch({
      downloaded <- FALSE
      response <- GET(catalog, query=queryParameters)
      downloaded <- TRUE
    },error=function(e){
      print(conditionMessage(e))
    })
  )

  if (!downloaded) {
    return(NULL)
  }

  status.code <- http_status(response)$category
  print(paste("download time:", round(download.time["elapsed"], 1), "seconds, status:", status.code))

  ######################################################
  # Parsing the WaterML XML Data                       #
  ######################################################

  print("reading sites XML data...")
  doc <- tryCatch({
    xmlParse(response)
  }, warning = function(w) {
    print("Error reading HIS Central Data: Bad XML format.")
    return(NULL)
  }, error = function(e) {
    print("Error reading HIS Central Data: Bad XML format.")
    return(NULL)
  }
  )
  if (is.null(doc)) {
    return(NULL)
  }

  # specify the namespace information for HIS Central
  ns <- c(xsd="http://www.w3.org/2001/XMLSchema",
          xsi="http://www.w3.org/2001/XMLSchema-instance",
          sr="http://hiscentral.cuahsi.org/20100205/")

  # extract the data columns with XPath
  SiteName = xpathSApply(doc, "//sr:SiteName", xmlValue, namespaces=ns)
  N <- length(SiteName)

  FullSiteCode = xpathSApply(doc, "//sr:SiteCode", xmlValue, namespaces=ns)
  SiteCode = sub(".*:", "", FullSiteCode)

  Latitude <- xpathSApply(doc, "//sr:Latitude", xmlValue, namespaces=ns)
  Longitude = xpathSApply(doc, "//sr:Longitude", xmlValue, namespaces=ns)

  if (IncludeServerDetails == TRUE) {
    ServCode <- xpathSApply(doc, "//sr:servCode", xmlValue, namespaces=ns)
    ServURL <- xpathSApply(doc, "//sr:servURL", xmlValue, namespaces=ns)
    df <- data.frame(
      SiteName = SiteName,
      SiteCode = SiteCode,
      FullSiteCode = FullSiteCode,
      Latitude = as.numeric(Latitude),
      Longitude = as.numeric(Longitude),
      ServCode = ServCode,
      ServURL = ServURL,
      stringsAsFactors = FALSE)
  } else {
    df <- data.frame(
      SiteName = SiteName,
      SiteCode = SiteCode,
      FullSiteCode = FullSiteCode,
      Latitude = as.numeric(Latitude),
      Longitude = as.numeric(Longitude),
      stringsAsFactors = FALSE)
  }

  return(df)
}

MakeSOAPEnvelope <- function(CUAHSINamespace, MethodName, parameters=NULL) {
  # MakeSOAPEnvelope
  #
  # A helper function that makes a SOAP envelope to send to the
  # CUAHSI WaterOneFlow SOAP web service. It is internally used by the GetSites,
  # GetSiteInfo, GetVariables and GetValues functions.
  #
  # @import httr
  # @param CUAHSINamespace The SOAP namespace. This must be either "http://www.cuahsi.org/his/1.0/ws"
  # for WaterML 1.0, or "http://www.cuahsi.org/his/1.1/ws" for WaterML 1.1
  # @param MethodName The name of the WaterOneFlow web service method. It can be one of the following
  # values: "GetSites", "GetSitesObject", "GetSitesByBoxObject", "GetSiteInfoObject",
  # "GetVariablesObject", "GetValuesObject"
  # @param parameters An optional vector of named parameters for the web method. For GetSites,
  # GetSitesObject and GetVariables no parameters are required. For GetSiteInfoObject you need the
  # "site" parameter. For GetValuesObject you need the "location", "variable", "startDate" and "endDate"
  # parameters.
  # @return A <soap:Envelope> text in XML format. This text is send in a HTTP POST body to the
  # SOAP service. Two headers must be sent in the request: Content-Type="text/XML" and
  # SOAPAction=paste(CUAHSINamespace, MethodName). For example if MethodName is GetSites and
  # the WaterML version is 1.1, then SOAPAction="http://www.cuahsi.org/his/1.1/ws/GetSites".
  # @keywords WaterML
  # @export
  # @examples
  # library(httr)
  # library(XML)
  # myEnvelope <- MakeSOAPEnvelope("http://www.cuahsi.org/his/1.1/ws/", "GetSitesObject")
  # SOAPAction <- "http://www.cuahsi.org/his/1.1/ws/GetSitesObject"
  # url <- "http://hydrodata.info/chmi-d/cuahsi_1_1.asmx"
  # response <- POST(url, body = myEnvelope,
  #                  add_headers("Content-Type" = "text/xml", "SOAPAction" = SOAPAction),
  #                  verbose())
  # status.code <- http_status(response)$category
  # WaterML <- xmlParse(response)
  # WaterML

  #check CUAHSINamespace parameter
  validNamespace_1_0 <- "http://www.cuahsi.org/his/1.0/ws/"
  validNamespace_1_1 <- "http://www.cuahsi.org/his/1.1/ws/"
  if (CUAHSINamespace != validNamespace_1_0 & CUAHSINamespace != validNamespace_1_1) {
    stop(paste("The CUAHSINamespace parameter must be either",
               validNamespace_1_0, "or", validNamespace_1_1))
  }
  #check MethodName
  validMethodNames <- c("GetSites", "GetSitesObject", "GetSitesByBoxObject",
                        "GetSiteInfoObject", "GetVariableInfoObject", "GetValuesObject")
  if (length(validMethodNames[validMethodNames == MethodName]) == 0) {
    message <- paste(validMethodNames, collapse=", ")
    stop(paste("The MethodName must be one of the following:", message))
  }

  soapAction <- paste(CUAHSINamespace, MethodName, sep="")
  #make the XML for parameters
  if (MethodName == "GetSitesObject" | MethodName == "GetSites") {
    XMLParameters <- c('<site></site>')
  } else {
    XMLParameters <- paste('<',names(parameters), '>', parameters, '</', names(parameters),'>\n',sep="")
  }
  envelope <- paste(
    '<?xml version="1.0" encoding="utf-8"?>\n',
    '<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">\n',
    '<soap:Body>\n',
    '<', MethodName, ' xmlns="', CUAHSINamespace, '">\n',
    paste(XMLParameters, sep="", collapse=""),
    '<authToken></authToken>\n',
    '</', MethodName, '>\n',
    '</soap:Body>\n',
    '</soap:Envelope>',
    sep="")

  return(envelope)
}

GetSites <- function(server, west=NULL, south=NULL, east=NULL, north=NULL) {
  # @import XML
  # @param server The URL of the web service ending with .WSDL,
  #  for example: http://icewater.usu.edu/MudLake/cuahsi_1_0.asmx?WSDL
  #  alternatively this can be the REST URL to get the sites.
  # @param west Optional parameter: The west longitude of the geographic
  #  bounding box in decimal degrees. Allowed values are between -180.0 and +180.0
  # @param south Optional parameter: The south latitude of the geographic
  #  bounding box in decimal degrees. Allowed values are between -90.0 and +90.0
  # @param east Optional parameter: The east longitude of the geographic
  #  bounding box in decimal degrees. Allowed values are between -180.0 and +180.0
  # @param north Optional parameter: The north latitude of the geographic
  #  bounding box in decimal degrees. Allowed values are between -90.0 and +90.0
  # @return a data.frame of sites. The data.frame has the following columns:
  # \itemize{
  # \item SiteID: The site ID in the original database
  # \item SiteName: The name of the site
  # \item SiteCode: A short unique code of the site
  # \item FullSiteCode: The complete unique code of the site in the format NETWORK:CODE.
  #               Use this value in the GetSiteInfo and GetValues functions
  # \item Latitude:  The WGS84 latitude in decimal degrees
  # \item Longitude: The WGS84 longitude in decimal degrees
  # \item Elevation: The elevation of the site above sea level in meters
  # \item State:     Only for sites in the USA: the state of the site
  # \item County:    Only for sites in the USA: The county of the site
  # \item Comments:  Additional comments about the sites (note: this field is often empty)
  # }
  # The output data.frame also has attributes with information about the status:
  # download.time, parse.time, download.status, parse.status
  # These attributes can be used for troubleshooting WaterOneFlow/WaterML server errors.

  # declare the default download timeout in seconds
  max_timeout = 360

  # declare empty return data frame
  df <- data.frame()

  # trim any leading and trailing whitespaces in server
  server <- gsub("^\\s+|\\s+$", "", server)

  versionInfo <- WaterOneFlowVersion(server)
  version <- versionInfo$Version

  #special case: WaterML 1.0 and bounding box: Delegate call to HIS Central
  if (!is.null(west) & !is.null(south) & !is.null(north) & !is.null(east) & version=="1.0") {
    services <- GetServices()
    serv <- services[services$url==server,]
    servID <- serv$id
    sitesDF <- HISCentral_GetSites(west, south, east, north,
                                   serviceID = servID,keyword=NULL,
                                   IncludeServerDetails = FALSE)
    sitesDF$SiteID <- sitesDF$SiteCode
    sitesDF$Elevation <- NA
    sitesDF$State <- NA
    sitesDF$County <- NA
    sitesDF$Comments <- NA
    return (data.frame(SiteID=sitesDF$SiteCode,
                       SiteName=sitesDF$SiteName,
                       SiteCode=sitesDF$SiteCode,
                       FullSiteCode=sitesDF$FullSiteCode,
                       Latitude=sitesDF$Latitude,
                       Longitude=sitesDF$Longitude,
                       Elevation=NA,
                       State=NA,
                       County=NA,
                       Comments=NA))
  }

  # if server ends with ?WSDL or ?wsdl, we assume that service is SOAP
  # otherwise, assume that service is REST
  SOAP <- TRUE

  # if server ends with .asmx, we also assume that the service is SOAP and we add ?WSDL
  m1 <- regexpr("asmx$", server)
  if (m1 > 1) {
    server <- paste(server, "WSDL", sep="?")
  }

  m <- regexpr("?WSDL|wsdl", server)
  if (m > 1) {
    url <- substr(server, 0, m - 2)
    SOAP <- TRUE
  } else {
    # in other cases we leave the URL as it is
    SOAP <- FALSE
  }

  #if the service is SOAP:
  if (SOAP) {
    versionInfo <- WaterOneFlowVersion(server)
    namespace <- versionInfo$Namespace
    version <- versionInfo$Version

    #choose the right SOAP web method based on WaterML version and parameters
    if (version == "1.0") {
      methodName <- "GetSites"

      envelope <- MakeSOAPEnvelope(namespace, methodName)
    } else {
      if (is.null(west) | is.null(south) | is.null(north) | is.null(east)) {
        methodName <- "GetSitesObject"
        envelope <- MakeSOAPEnvelope(namespace, methodName)
      } else {
        methodName <- "GetSitesByBoxObject"
        envelope <- MakeSOAPEnvelope(namespace, methodName,
                      parameters=c(west=west, south=south, north=north, east=east,IncludeSeries="false"))
      }
    }
    SOAPAction <- paste(namespace, methodName, sep="")
    headers <- c("Content-Type" = "text/xml", "SOAPAction" = SOAPAction)

    print(paste("downloading sites from:", url, "..."))

    downloaded <- FALSE
    download.time <- system.time(
      err <- tryCatch({
        response <- POST(url, body = envelope, add_headers(headers),
                         timeout(max_timeout))
        status <- http_status(response)$message
        downloaded <- TRUE
      },error = function(e) {
        print(conditionMessage(e))
        stop(e)
      }
      )
    )
    if (!downloaded) {
      attr(df, "download.time") <- download.time["elapsed"]
      attr(df, "download.status") <- err
      attr(df, "parse.time") <- NA
      attr(df, "parse.status") <- NA
      return(df)
    }

    status.code <- http_status(response)$category

    print(paste("download time:", round(download.time["elapsed"], 0), "seconds, status:", status.code))

    #in case of server error, print the error and exit
    if (tolower(status.code) == "server error") {
      status <- http_status(response)$message
      print(status)
      attr(df, "download.time") <- download.time["elapsed"]
      attr(df, "download.status") <- status
      attr(df, "parse.time") <- NA
      attr(df, "parse.status") <- NA
      return(df)
    }
  } else {
    # If the service is REST:
    print(paste("downloading sites from:", server, "..."))

    downloaded <- FALSE
    download.time <- system.time(
      err <- tryCatch({
        response <- GET(server)
        status <- http_status(response)$message
        downloaded <- TRUE
      },error=function(e){
        print(conditionMessage(e))
        stop(e)
      })
    )

    if (!downloaded) {
      attr(df, "download.time") <- download.time["elapsed"]
      attr(df, "download.status") <- err
      attr(df, "parse.time") <- NA
      attr(df, "parse.status") <- NA
      return(df)
    }

    status.code <- http_status(response)$category
    print(paste("download time:", download.time["elapsed"], "seconds, status:", status.code))
  }
  attr(df, "download.time") <- download.time["elapsed"]
  attr(df, "download.status") <- status.code

  ######################################################
  # Parsing the WaterML XML Data                       #
  ######################################################

  begin.parse.time <- Sys.time()

  print("reading sites WaterML data...")
  doc <- NULL
  err <- tryCatch({
    doc <- xmlParse(response)
  }, warning = function(w) {
    print("Error reading WaterML: Bad XML format.")
    attr(df, "parse.status") <- "Bad XML format"
    attr(df, "parse.time") <- 0
    return(df)
  }, error = function(e) {
    print("Error reading WaterML: Bad XML format.")
    attr(df, "parse.status") <- "Bad XML format"
    attr(df, "parse.time") <- 0
    return(df)
  }
  )
  if (is.null(doc)) {
    print("Error reading WaterML: Bad XML format.")
    attr(df, "parse.status") <- "Bad XML format"
    attr(df, "parse.time") <- 0
    return(df)
  }

  # specify the namespace information
  ns <- WaterOneFlowNamespace(version)

  # extract the data columns with XPath
  SiteName = xpathSApply(doc, "//sr:siteName", xmlValue, namespaces=ns)

  N <- length(SiteName)
  bigData <- 10000
  if (N > bigData) {
    print(paste("found", N,"sites"))
    print("processing SiteCode...")
  }
  SiteCode = xpathSApply(doc, "//sr:siteCode", xmlValue, namespaces=ns)
  Network = xpathSApply(doc, "//sr:siteCode", xmlGetAttr, name="network", namespaces=ns)

  SiteID <- xpathSApply(doc, "//sr:siteCode", xmlGetAttr, name="siteID", namespaces=ns)
  SiteID <- unlist(SiteID)

  Latitude <- xpathSApply(doc, "//sr:latitude", xmlValue, namespaces=ns)
  Longitude = xpathSApply(doc, "//sr:longitude", xmlValue, namespaces=ns)

  numSiteIDs <- length(SiteID)

  if (numSiteIDs != N) {
    SiteID <- SiteCode
  }

  Elevation <- xpathSApply(doc, "//sr:elevation_m", xmlValue, namespaces=ns)
  numElevations <- length(Elevation)
  if (numElevations != N) {
    Elevation <- NA
  }

  # State, County, Comments: different tags for WaterML 1.0 and 1.1
  if (version=="1.1"){
    State = xpathSApply(doc, "//sr:siteProperty[@name='State']", xmlValue, namespaces=ns)
    County = xpathSApply(doc, "//sr:siteProperty[@name='County']", xmlValue, namespaces=ns)
    Comments = xpathSApply(doc, "//sr:siteProperty[@name='Site Comments']", xmlValue, namespaces=ns)
  } else {
    State = xpathSApply(doc, "//sr:note[@title='State']", xmlValue, namespaces=ns)
    County = xpathSApply(doc, "//sr:note[@title='County']", xmlValue, namespaces=ns)
    Comments = NA
  }
  # Check for empty values of state, county, comments
  numStates <- length(State)
  if (numStates != N) {
    State <- NA
  }
  numCounties <- length(County)
  if (numCounties != N) {
    County <- NA
  }
  numComments <- length(Comments)
  if (numComments != N) {
    Comments <- NA
  }

  #special case: some site doesn't have latitude specified
  numLatitudes <- length(Latitude)
  if (numLatitudes < N) {
    numValid <- N - numLatitudes + 1
    SiteName <- SiteName[numValid:N]
    SiteCode <- SiteCode[numValid:N]
    SiteID <- SiteID[numValid:N]
    Network <- Network[numValid:N]
    Longitude <- Longitude[numValid:N]
    Latitude <- Latitude[numValid:N]
    Elevation <- Elevation[numValid:N]
    State <- State[numValid:N]
    County <- County[numValid:N]
    Comments <- Comments[numValid:N]
  }

  df <- data.frame(
    SiteID = SiteID,
    SiteName = SiteName,
    SiteCode = SiteCode,
    FullSiteCode = paste(Network, SiteCode, sep=":"),
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    Elevation = as.numeric(Elevation),
    State = State,
    County = County,
    Comments = Comments,
    stringsAsFactors = FALSE)

  end.parse.time <- Sys.time()
  parse.time <- as.numeric(difftime(end.parse.time, begin.parse.time, units="sec"))
  attr(df, "download.time") <- download.time["elapsed"]
  attr(df, "download.status") <- "success"
  attr(df, "parse.time") <- parse.time
  attr(df, "parse.status") <- "OK"
  return(df)
}

GetSiteInfo <- function(server, siteCode) {
  # GetSiteInfo
  #
  # This function gets the table variables measured at a specific site from the WaterML web service
  #
  # @import XML
  # @param server The URL of the web service ending with .asmx or .wsdl,
  #  for example: http://hydroportal.cuahsi.org/ipswich/cuahsi_1_1.asmx?WSDL
  # @param siteCode The full site code, for example: IRWA:FB-BV. To get a list of
  # available site codes, see GetSites() function and use the FullSiteCode field.
  # @return a data.frame of data values with the following columns:
  # \tabular{lll}{
  # Name \tab Type \tab Description \cr
  # SiteID \tab character \tab The site ID in the original database\cr
  # SiteName \tab character \tab The name of the site \cr
  # SiteCode \tab character \tab A short unique code of the site\cr
  # FullSiteCode \tab character \tab The complete unique code of the site \cr
  #               in the format NETWORK:CODE, for example SNOTEL:879.
  #               \cr
  # Latitude \tab  numeric \tab The WGS84 latitude in decimal degrees \cr
  # Longitude \tab numeric \tab The WGS84 longitude in decimal degrees \cr
  # Elevation \tab numeric \tab The elevation of the site above sea level in meters \cr
  # State \tab character \tab Only for sites in the USA: the state of the site \cr
  # County \tab character \tab Only for sites in the USA: The county of the site \cr
  # Comments \tab character \tab Additional comments about the sites \cr
  #                          (note: this field is often empty)
  #                          \cr
  # VariableCode \tab character \tab Short code of the variable \cr
  # FullVariableCode \tab character \tab The full variable code, for example: SNOTEL:SNWD. \cr
  #                   Use this value as the variableCode parameter in GetValues().
  #                   \cr
  # VariableName \tab character \tab The name of the variable \cr
  # ValueType \tab character \tab the type of observation: \cr
  #            Field Observation or Derived Value
  #            \cr
  # DataType \tab character \tab the aggregate data type: \cr
  #           Average, Continuous, Sporadic..
  #           \cr
  # GeneralCategory \tab character \tab the general category of the measurements: \cr
  #                  Climate, Water Quality..
  #                  \cr
  # SampleMedium \tab character \tab the sample medium: \cr
  #               for example water, atmosphere, soil..
  #               \cr
  # UnitName \tab character \tab The name of the measurement units \cr
  # UnitType \tab character \tab the type of the measurement units \cr
  # UnitAbbreviation \tab character \tab The abbreviation of the measurement units \cr
  #                   (m, cm, in..)
  #                   \cr
  # NoDataValue \tab numeric \tab The value that indicates missing data \cr
  # IsRegular \tab boolean \tab TRUE if the measurements are regular, FALSE otherwise \cr
  # TimeUnitName \tab character \tab The name of the time units \cr
  # TimeUnitAbbreviation \tab character \tab The time units abbreviation \cr
  # TimeSupport \tab character \tab The length of the time period over which \cr
  #              one measurement is taken
  #              \cr
  # Speciation \tab character \tab The chemical sample speciation \cr
  #             (as nitrogen, as phosphorus..)
  #             \cr
  # methodID \tab character \tab The ID of the sensor or measurement method \cr
  # methodCode \tab character \tab The code of the sensor or measurement method. \cr
  #             Usually the same as methodID.
  #             \cr
  # methodDescription \tab character \tab The description of the sensor or \cr
  #                    of the data collection instrumentation / measurement method.
  #                    \cr
  # methodLink \tab character \tab The hyperlink of the website \cr
  #             of the sensor or measurement method.
  #             \cr
  # sourceID \tab character \tab The ID of the data source or author \cr
  # organization \tab character \tab The name of the organization collecting the data \cr
  # sourceDescription \tab character \tab The description of organization collecting the data \cr
  # citation \tab character \tab Instruction how to cite the data \cr
  # qualityControlLevelID \tab character \tab The ID of the quality control level. \cr
  #                        Usually 0 means raw data and 1 means quality controlled data.
  #                        \cr
  # qualityControlLevelCode: \tab character \tab The code of the quality control level.
  #                           Usually same as qualityControlLevelID.
  #                           \cr
  # qualityControlLevelDefinition: \tab character \tab The quality control level definition. \cr
  # valueCount: \tab character \tab The number of observations in this time series \cr
  # beginDateTime: \tab POSIXct \tab The local date and time of the first available \cr
  #                              observation in this time series.
  #                              \cr
  # endDateTime: \tab POSIXct \tab The local date and time of the last available \cr
  #                            observation in this time series.
  #                            \cr
  # beginDateTimeUTC: \tab POSIXct \tab The UTC date and time of the last available
  #                                 observation in this time series.
  #                                 \cr
  # endDateTimeUTC: \tab POSIXct \tab The UTC date and time of the last available
  #                               observation in this time series.
  #                               \cr
  # }
  # The output data.frame also has attributes with information about the status:
  # download.time, parse.time, download.status, parse.status
  # These attributes can be used for troubleshooting WaterOneFlow/WaterML server errors.
  # If parse status is "NO_SERIES_FOUND", then this site doesn't have any available data.
  # @keywords waterml
  # @export
  # @examples
  # server <- "http://hydroportal.cuahsi.org/SNOTEL/cuahsi_1_1.asmx"
  # siteInfo <- GetSiteInfo(server, siteCode="SNOTEL:879")

  # declare the default download timeout in seconds
  max_timeout = 360

  # declare empty return data frame
  df <- data.frame()

  # trim any leading and trailing whitespaces in server
  server <- gsub("^\\s+|\\s+$", "", server)

  # if server ends with ?WSDL or ?wsdl, we assume that service is SOAP
  # otherwise, assume that service is REST
  SOAP <- TRUE

  # if server ends with .asmx, we also assume that the service is SOAP and we add ?WSDL
  m1 <- regexpr("asmx$", server)
  if (m1 > 1) {
    server <- paste(server, "WSDL", sep="?")
  }


  # if server ends with ?WSDL or ?wsdl, we assume that service is SOAP
  # otherwise, assume that service is REST
  m <- regexpr("?WSDL|wsdl", server)
  if (m > 1) {
    url <- substr(server, 0, m - 2)
    SOAP <- TRUE
  } else {
    SOAP <- FALSE
  }

  #if the service is SOAP:
  if (SOAP) {
    versionInfo <- WaterOneFlowVersion(server)
    namespace <- versionInfo$Namespace
    version <- versionInfo$Version
    methodName <- "GetSiteInfoObject"

    SOAPAction <- paste(namespace, methodName, sep="")
    headers <- c("Content-Type" = "text/xml","SOAPAction" = SOAPAction)
    envelope <- MakeSOAPEnvelope(namespace, methodName, c(site=siteCode))

    print(paste("downloading SiteInfo from:", url))

    downloaded <- FALSE
    download.time <- system.time(
      err <- tryCatch({
        response <- POST(url, body = envelope, add_headers(headers),
                         timeout(max_timeout))
        status <- http_status(response)$message
        downloaded <- TRUE
        },error = function(e) {
          warning(conditionMessage(e))
        }
      )
    )
    if (!downloaded) {
      attr(df, "download.time") <- download.time["elapsed"]
      attr(df, "download.status") <- err
      attr(df, "parse.time") <- NA
      attr(df, "parse.status") <- NA
      return(df)
    }

    status.code <- http_status(response)$category

    print(paste("download time:", round(download.time["elapsed"], 1), "seconds, status:", status.code))

  } else {
    #if the service is REST
    print(paste("downloading SiteInfo from:", server))
    downloaded <- FALSE
    download.time <- system.time(
      err <- tryCatch({
        response <- GET(server, timeout(max_timeout))
        status <- http_status(response)$message
        downloaded <- TRUE
        },error = function(e) {
          warning(conditionMessage(e))
        }
      )
    )
    if (!downloaded) {
      attr(df, "download.time") <- download.time["elapsed"]
      attr(df, "download.status") <- err
      attr(df, "parse.time") <- NA
      attr(df, "parse.status") <- NA
      return(df)
    }

    status.code <- http_status(response)$category
    version <- "1.1"

    print(paste("download time:", download.time["elapsed"], "seconds, status:", status.code))
  }
  attr(df, "download.time") <- download.time["elapsed"]
  attr(df, "download.status") <- status.code

  ######################################################
  # Parsing the WaterML XML Data                       #
  ######################################################
  begin.parse.time <- Sys.time()

  doc <- xmlParse(response)

  # specify the namespace information
  ns <- WaterOneFlowNamespace(version)

  #try to find faultstring to look for an error
  fault <- xpathSApply(doc, "//soap:Fault", xmlValue, namespaces=ns)
  if (length(fault) > 0) {
    warning(paste("SERVER ERROR in GetSiteInfo ", as.character(fault), sep=":"))
    attr(df, "download.time") <- download.time["elapsed"]
    attr(df, "download.status") <- as.character(fault)
    attr(df, "parse.time") <- NA
    attr(df, "parse.status") <- "SERVER FAULT"
    return(df)
  }

  SiteName = xpathSApply(doc, "//sr:siteName", xmlValue, namespaces=ns)
  N <- length(SiteName)
  SiteCode = xpathSApply(doc, "//sr:siteCode", xmlValue, namespaces=ns)
  Network = xpathSApply(doc, "//sr:siteCode", xmlGetAttr, name="network", namespaces=ns)

  SiteID <- xpathSApply(doc, "//sr:siteCode", xmlGetAttr, name="siteID", namespaces=ns)
  SiteID <- unlist(SiteID)
  numSiteIDs <- length(SiteID)
  if (numSiteIDs != N) {
    SiteID <- SiteCode
  }

  Latitude <- xpathSApply(doc, "//sr:latitude", xmlValue, namespaces=ns)
  Longitude = xpathSApply(doc, "//sr:longitude", xmlValue, namespaces=ns)

  Elevation <- xpathSApply(doc, "//sr:elevation_m", xmlValue, namespaces=ns)
  numElevations <- length(Elevation)
  if (numElevations != N) {
    Elevation <- NA
  }

  # State, County, Comments: different tags for WaterML 1.0 and 1.1
  if (version=="1.1"){
    State = xpathSApply(doc, "//sr:siteProperty[@name='State']", xmlValue, namespaces=ns)
    County = xpathSApply(doc, "//sr:siteProperty[@name='County']", xmlValue, namespaces=ns)
    Comments = xpathSApply(doc, "//sr:siteProperty[@name='Site Comments']", xmlValue, namespaces=ns)
  } else {
    State = xpathSApply(doc, "//sr:note[@title='State']", xmlValue, namespaces=ns)
    County = xpathSApply(doc, "//sr:note[@title='County']", xmlValue, namespaces=ns)
    Comments = NA
  }
  # Check for empty values of state, county, comments
  numStates <- length(State)
  if (numStates != N) {
    State <- NA
  }
  numCounties <- length(County)
  if (numCounties != N) {
    County <- NA
  }
  numComments <- length(Comments)
  if (numComments != N) {
    Comments <- NA
  }

  VariableCode <- xpathSApply(doc, "//sr:variableCode", xmlValue, namespaces=ns)
  N <- length(VariableCode)

  # Check for 'No Series Found' case
  if (N==0) {
    print(paste("NOTE: 0 time series found for site:", siteCode))

    end.parse.time <- Sys.time()
    parse.time <- as.numeric(difftime(end.parse.time, begin.parse.time, units="sec"))

    attr(df, "download.time") <- download.time["elapsed"]
    attr(df, "download.status") <- "success"
    attr(df, "parse.time") <- parse.time
    attr(df, "parse.status") <- "NO_SERIES_FOUND"
    return(df)
  }

  VariableName <- xpathSApply(doc, "//sr:variableName", xmlValue, namespaces=ns)

  VariableID <- unlist(xpathSApply(doc, "//sr:variableCode", xmlGetAttr, name="variableID", namespaces=ns))
  if (length(VariableID) == 0) { VariableID <- VariableCode }

  Vocabulary <- unlist(xpathSApply(doc, "//sr:variableCode", xmlGetAttr, name="vocabulary", namespaces=ns))

  #######################################################################################
  # START of SPECIAL CASE: process variable: use special case for WaterML 1.0           #
  #for WaterML 1.0 we must use a loop, because elements with unknown values are missing #
  #######################################################################################
  if (version == "1.0") {
    allVariables <- getNodeSet(doc, "//sr:series/sr:variable", namespaces=ns)
    i <- 1
    if (length(allVariables) < N) {
      print("Bad XML format: not enough details about the variables")

      end.parse.time <- Sys.time()
      parse.time <- as.numeric(difftime(end.parse.time, begin.parse.time, units="sec"))

      attr(df, "download.time") <- download.time["elapsed"]
      attr(df, "download.status") <- "success"
      attr(df, "parse.time") <- parse.time
      attr(df, "parse.status") <- "BAD_XML_FORMAT"
      return(df)
    }
    #allocate vectors for variables
    ValueType=rep("",N)
    DataType=rep("",N)
    GeneralCategory=rep("",N)
    SampleMedium=rep("",N)
    UnitName=rep("",N)
    UnitType=rep("",N)
    UnitAbbreviation=rep("",N)
    NoDataValue=rep(NA,N)
    IsRegular=rep("",N)
    TimeUnitName=rep("",N)
    TimeUnitAbbreviation=rep("",N)
    TimeSupport=rep("",N)
    Speciation=rep("",N)

    for (i in 1:N) {
      varObj <- allVariables[[i]]
      v <- unlist(xmlToList(varObj))
      ValueType[i] <- v["valueType"]
      DataType[i] <- v["dataType"]
      if (!is.null(v["generalCategory"])) {
        GeneralCategory[i] <- v["generalCategory"]
      }
      if (!is.null(v["sampleMedium"])) {
        SampleMedium[i] <- v["sampleMedium"]
      }
      UnitName[i] <- v["units.text"]
      if (!is.null(v["units..attrs.unitsType"])) {
        UnitType[i] <- v["units..attrs.unitsType"]
      }
      UnitAbbreviation[i] <- v["units..attrs.unitsAbbreviation"]
      IsRegular[i] <- ifelse(is.na(v["timeSupport..attrs.isRegular"]), v["timeSupport.isRegular"],
                                v["timeSupport..attrs.isRegular"])
      TimeUnitName[i] <- v["timeSupport.unit.UnitDescription"]
      if (is.na(TimeUnitName[i])) {
        TimeUnitName[i] <- v["timeSupport.unit.UnitName"]
      }
      TimeUnitAbbreviation[i] <- v["timeSupport.unit.UnitAbbreviation"]
      TimeSupport[i] <- v["timeSupport.timeInterval"]

      if (!is.null(v["NoDataValue"])) {
        NoDataValue[i] <- as.numeric(v["NoDataValue"])
      }
    }

    MethodID <- unlist(xpathSApply(doc, "//sr:Method", xmlGetAttr, name="methodID", namespaces=ns))
    MethodCode <- xpathSApply(doc, "//sr:MethodCode", xmlValue, namespaces=ns)

    MethodDescription <- xpathSApply(doc, "//sr:MethodDescription", xmlValue, namespaces=ns)
    if (length(MethodDescription) < N) { MethodDescription <- NA }

    MethodLink <- xpathSApply(doc, "//sr:MethodLink", xmlValue, namespaces=ns)
    if (length(MethodLink) < N) { MethodLink <- NA }

    if (length(MethodID) < N & length(MethodCode) == N) {
      MethodID <- MethodCode
    }
    if (length(MethodCode) < N & length(MethodID) == N) {
      MethodCode <- MethodID
    }
    if (length(MethodID) < N) { MethodID <- NA }
    if (length(MethodCode) < N) { MethodCode <- NA }

    SourceID <- unlist(xpathSApply(doc, "//sr:Source", xmlGetAttr, name="sourceID", namespaces=ns))
    if (length(SourceID) < N) { SourceID <- NA }

    Organization <- xpathSApply(doc, "//sr:Organization", xmlValue, namespaces=ns)
    if (length(Organization) < N) { Organization <- NA }

    SourceDescription <- xpathSApply(doc, "//sr:SourceDescription", xmlValue, namespaces=ns)
    if (length(SourceDescription) < N) { SourceDescription <- NA }

    Citation <- xpathSApply(doc, "//sr:Citation", xmlValue, namespaces=ns)
    if (length(Citation) < N) { Citation <- NA }

    QualityControlLevelID <- unlist(xpathSApply(doc, "//sr:QualityControlLevel", xmlGetAttr,
                                      name="qualityControlLevelID", namespaces=ns))

    QualityControlLevelCode <- xpathSApply(doc, "//sr:qualityControlLevelCode", xmlValue, namespaces=ns)

    if (length(QualityControlLevelID) < N & length(QualityControlLevelCode == N)) {
      QualityControlLevelID <- QualityControlLevelCode
    }
    if (length(QualityControlLevelCode) < N & length(QualityControlLevelID == N)) {
      QualityControlLevelCode <- QualityControlLevelID
    }
    if (length(QualityControlLevelID) < N) { QualityControlLevelID <- NA }
    if (length(QualityControlLevelCode) < N) { QualityControlLevelCode <- NA }

    QualityControlLevelDefinition=xpathSApply(doc, "//sr:QualityControlLevel", xmlValue, namespaces=ns)
    if (length(QualityControlLevelDefinition) < N) { QualityControlLevelDefinition <- NA }

    ValueCount <- xpathSApply(doc, "//sr:valueCount", xmlValue, namespaces=ns)

    BeginDateTime <- xpathSApply(doc, "//sr:beginDateTime", xmlValue, namespaces=ns)
    EndDateTime <- xpathSApply(doc, "//sr:endDateTime", xmlValue, namespaces=ns)

    BeginDateTimeUTC <- xpathSApply(doc, "//sr:beginDateTimeUTC", xmlValue, namespaces=ns)
    if (length(BeginDateTimeUTC) == 0) { BeginDateTimeUTC <- BeginDateTime }

    EndDateTimeUTC <- xpathSApply(doc, "//sr:endDateTimeUTC", xmlValue, namespaces=ns)
    if (length(EndDateTimeUTC) == 0) { EndDateTimeUTC <- EndDateTime }
  #################################################################################################
  # END of SPECIAL CASE of WaterML 1.0                                                            #
  #################################################################################################
  } else {

    ValueType <- xpathSApply(doc, "//sr:valueType", xmlValue, namespaces=ns)
    DataType <- xpathSApply(doc, "//sr:dataType", xmlValue, namespaces=ns)
    GeneralCategory <- xpathSApply(doc, "//sr:generalCategory", xmlValue, namespaces=ns)
    SampleMedium <- xpathSApply(doc, "//sr:sampleMedium", xmlValue, namespaces=ns)

    UnitName <- xpathSApply(doc, "//sr:units/sr:unitName", xmlValue, namespaces=ns)
    UnitType <- xpathSApply(doc, "//sr:units/sr:unitType", xmlValue, namespaces=ns)
    UnitAbbreviation <- xpathSApply(doc,
      "//sr:variable/sr:units/*[self::sr:unitsAbbreviation or self::sr:unitAbbreviation]",
      xmlValue, namespaces=ns)

    #if UnitName is not found, then we look for /variable/unit instead
    if (length(UnitName) == 0) {
      UnitName <- xpathSApply(doc, "//sr:variable/sr:unit/sr:unitName", xmlValue, namespaces=ns)
      UnitType <- xpathSApply(doc, "//sr:variable/sr:unit/sr:unitType", xmlValue, namespaces=ns)
      UnitAbbreviation <- xpathSApply(doc,
        "//sr:variable/sr:unit/*[self::sr:unitsAbbreviation or self::sr:unitAbbreviation]",
        xmlValue, namespaces=ns)
    }

    NoDataValue <- xpathSApply(doc, "//sr:noDataValue", xmlValue, namespaces=ns)

    IsRegular <- unlist(xpathSApply(doc, "//sr:timeScale", xmlGetAttr, name="isRegular", namespaces=ns))
    if (length(IsRegular) < N) {
      IsRegular <- (DataType != "Sporadic")
    }

    TimeUnitName <- xpathSApply(doc, "//sr:timeScale/sr:unit/sr:unitName", xmlValue, namespaces=ns)
    TimeUnitName <- unlist(TimeUnitName)
    if (length(TimeUnitName) < N) { TimeUnitName <- NA }

    TimeUnitAbbreviation <- xpathSApply(doc,
      "//sr:timeScale/sr:unit/*[self::sr:unitsAbbreviation or self::sr:unitAbbreviation]", xmlValue, namespaces=ns)
    TimeUnitAbbreviation <- unlist(TimeUnitAbbreviation)
    if (length(TimeUnitAbbreviation) < N) { TimeUnitAbbreviation <- NA }

    TimeSupport <- xpathSApply(doc, "//sr:timeSupport", xmlValue, namespaces=ns)
    Speciation <- xpathSApply(doc, "//sr:variable/sr:speciation", xmlValue, namespaces=ns)

    BeginDateTime <- xpathSApply(doc, "//sr:beginDateTime", xmlValue, namespaces=ns)
    EndDateTime <- xpathSApply(doc, "//sr:endDateTime", xmlValue, namespaces=ns)

    BeginDateTimeUTC <- xpathSApply(doc, "//sr:beginDateTimeUTC", xmlValue, namespaces=ns)
    if (length(BeginDateTimeUTC) == 0) { BeginDateTimeUTC <- BeginDateTime }

    EndDateTimeUTC <- xpathSApply(doc, "//sr:endDateTimeUTC", xmlValue, namespaces=ns)
    if (length(EndDateTimeUTC) == 0) { EndDateTimeUTC <- EndDateTime }

    ValueCount <- xpathSApply(doc, "//sr:valueCount", xmlValue, namespaces=ns)

    MethodID <- unlist(xpathSApply(doc, "//sr:method", xmlGetAttr, name="methodID", namespaces=ns))

    MethodCode <- xpathSApply(doc, "//sr:methodCode", xmlValue, namespaces=ns)

    MethodDescription <- xpathSApply(doc, "//sr:methodDescription", xmlValue, namespaces=ns)
    if (length(MethodDescription) < N) { MethodDescription <- NA }

    MethodLink <- xpathSApply(doc, "//sr:methodLink", xmlValue, namespaces=ns)
    if (length(MethodLink) < N) { MethodLink <- NA }

    if (length(MethodID) < N & length(MethodCode) == N) {
      MethodID <- MethodCode
    }
    if (length(MethodCode) < N & length(MethodID) == N) {
      MethodCode <- MethodID
    }
    if (length(MethodID) < N) { MethodID <- NA }
    if (length(MethodCode) < N) { MethodCode <- NA }

    SourceID <- unlist(xpathSApply(doc, "//sr:source", xmlGetAttr, name="sourceID", namespaces=ns))
    if (length(SourceID) < N) { SourceID <- NA }

    Organization <- xpathSApply(doc, "//sr:organization", xmlValue, namespaces=ns)
    if (length(Organization) < N) { Organization <- NA }

    SourceDescription <- xpathSApply(doc, "//sr:sourceDescription", xmlValue, namespaces=ns)
    if (length(SourceDescription) < N) { SourceDescription <- NA }

    Citation <- xpathSApply(doc, "//sr:citation", xmlValue, namespaces=ns)
    if (length(Citation) < N) { Citation <- NA }

    QualityControlLevelID=unlist(xpathSApply(doc, "//sr:qualityControlLevel", xmlGetAttr,
                                      name="qualityControlLevelID", namespaces=ns))
    QualityControlLevelCode=xpathSApply(doc, "//sr:qualityControlLevelCode", xmlValue, namespaces=ns)

    if (length(QualityControlLevelID) < N & length(QualityControlLevelCode == N)) {
      QualityControlLevelID <- QualityControlLevelCode
    }
    if (length(QualityControlLevelCode) < N & length(QualityControlLevelID == N)) {
      QualityControlLevelCode <- QualityControlLevelID
    }
    if (length(QualityControlLevelID) < N) { QualityControlLevelID <- NA }
    if (length(QualityControlLevelCode) < N) { QualityControlLevelCode <- NA }

    QualityControlLevelDefinition=xpathSApply(doc, "//sr:definition", xmlValue, namespaces=ns)
    if (length(QualityControlLevelDefinition) < N) { QualityControlLevelDefinition <- NA }
  }

  #define the columns for the output data frame
  df <- data.frame(SiteName=rep(SiteName, N),
                   SiteID=rep(SiteID, N),
                   SiteCode=rep(SiteCode, N),
                   FullSiteCode = rep(paste(Network, SiteCode, sep=":"), N),
                   Latitude=rep(as.numeric(Latitude), N),
                   Longitude=rep(as.numeric(Longitude), N),
                   Elevation=rep(as.numeric(Elevation), N),
                   State=rep(State, N),
                   County=rep(County, N),
                   Comments=rep(Comments, N),
                   VariableCode=VariableCode,
                   FullVariableCode=paste(Vocabulary, VariableCode, sep=":"),
                   VariableName=VariableName,
                   ValueType=ValueType,
                   DataType=DataType,
                   GeneralCategory=GeneralCategory,
                   SampleMedium=SampleMedium,
                   UnitName=UnitName,
                   UnitType=UnitType,
                   UnitAbbreviation=UnitAbbreviation,
                   NoDataValue=as.numeric(NoDataValue),
                   IsRegular=IsRegular,
                   TimeUnitName=TimeUnitName,
                   TimeUnitAbbreviation=TimeUnitAbbreviation,
                   TimeSupport=TimeSupport,
                   Speciation=Speciation,
                   methodID=MethodID,
                   methodCode=MethodCode,
                   methodDescription=MethodDescription,
                   methodLink=MethodLink,
                   sourceID=SourceID,
                   organization=Organization,
                   sourceDescription=SourceDescription,
                   citation=Citation,
                   qualityControlLevelID=QualityControlLevelID,
                   qualityControlLevelCode=QualityControlLevelCode,
                   qualityControlLevelDefinition=QualityControlLevelDefinition,
                   valueCount=ValueCount,
                   beginDateTime=as.POSIXct(strptime(BeginDateTime, "%Y-%m-%dT%H:%M:%S")),
                   endDateTime=as.POSIXct(strptime(EndDateTime, "%Y-%m-%dT%H:%M:%S")),
                   beginDateTimeUTC=as.POSIXct(strptime(BeginDateTimeUTC, "%Y-%m-%dT%H:%M:%S")),
                   endDateTimeUTC=as.POSIXct(strptime(EndDateTimeUTC, "%Y-%m-%dT%H:%M:%S")),
                   stringsAsFactors=FALSE)

  if (nrow(df) == 0) {
    print(paste("NOTE: 0 time series found for site:", siteCode))

    end.parse.time <- Sys.time()
    parse.time <- as.numeric(difftime(end.parse.time, begin.parse.time, units="sec"))

    attr(df, "download.time") <- download.time["elapsed"]
    attr(df, "download.status") <- "success"
    attr(df, "parse.time") <- parse.time
    attr(df, "parse.status") <- "NO_SERIES_FOUND"
    return(df)
  }

  end.parse.time <- Sys.time()
  parse.time <- as.numeric(difftime(end.parse.time, begin.parse.time, units="sec"))
  attr(df, "download.time") <- download.time["elapsed"]
  attr(df, "download.status") <- "success"
  attr(df, "parse.time") <- parse.time
  attr(df, "parse.status") <- "OK"
  return(df)
}

GetVariables <- function(server) {
  # GetVariables
  #
  # This function gets the table of variables from the WaterML web service
  #
  # @import XML
  # @param server The URL of the web service ending with ?WSDL,
  #  for example: http://worldwater.byu.edu/app/index.php/rushvalley/services/cuahsi_1_1.asmx?WSDL
  # @return a data.frame of variables with the following columns:
  # \tabular{lll}{
  # VariableCode \tab character \tab Short code of the variable \cr
  # FullVariableCode \tab character \tab The full variable code, for example: SNOTEL:897. Use this value
  #                   as the variableCode parameter in GetValues() function.
  #                   \cr
  # VariableName \tab character \tab The name of the variable \cr
  # ValueType \tab character \tab the type of observation: Field Observation or Derived Value \cr
  # DataType \tab character \tab the aggregate data type: Average, Continuous, Sporadic.. \cr
  # GeneralCategory \tab character \tab the general category of the measurements: Climate, Water Quality.. \cr
  # SampleMedium \tab character \tab the sample medium, for example water, atmosphere, soil.. \cr
  # UnitName \tab character \tab The name of the measurement units \cr
  # UnitType \tab character \tab the type of the measurement units \cr
  # UnitAbbreviation \tab character \tab The abbreviation of the measurement units (m, cm, in..) \cr
  # NoDataValue \tab numeric \tab The value that indicates missing data \cr
  # IsRegular \tab boolean \tab TRUE if the measurements are regular, FALSE otherwise \cr
  # TimeUnitName \tab character \tab The name of the time units \cr
  # TimeUnitAbbreviation \tab character \tab The time units abbreviation \cr
  # TimeSupport \tab character \tab The length of the time period over which one measurement is taken \cr
  # Speciation \tab character \tab The chemical sample speciation (as nitrogen, as phosphorus..) \cr
  # }
  # @keywords WaterML
  # @export
  # @examples
  # GetVariables("http://worldwater.byu.edu/app/index.php/rushvalley/services/cuahsi_1_1.asmx?WSDL")

  # declare the default download timeout in seconds
  max_timeout = 360

  # declare empty return data frame
  df <- data.frame()

  # trim any leading and trailing whitespaces in server
  server <- gsub("^\\s+|\\s+$", "", server)

  # if server ends with .asmx, we also assume that the service is SOAP and we add ?WSDL
  m1 <- regexpr("asmx$", server)
  if (m1 > 1) {
    server <- paste(server, "WSDL", sep="?")
  }

  # if server ends with ?WSDL or ?wsdl, we assume that service is SOAP
  # otherwise, assume that service is REST
  SOAP <- TRUE
  m <- regexpr("?WSDL|wsdl", server)
  if (m > 1) {
    url <- substr(server, 0, m - 2)
    SOAP <- TRUE
  } else {
    SOAP <- FALSE
  }

  #if the service is SOAP:
  if (SOAP) {
    versionInfo <- WaterOneFlowVersion(server)
    namespace <- versionInfo$Namespace
    version <- versionInfo$Version
    methodName <- "GetVariableInfoObject"

    SOAPAction <- paste(namespace, methodName, sep="")
    envelope <- MakeSOAPEnvelope(namespace, methodName, c(variable=""))
    headers <- c("Content-Type" = "text/xml", "SOAPAction" = SOAPAction)

    print(paste("GetVariables from", url))

    downloaded <- FALSE
    download.time <- system.time(
      err <- tryCatch({
        response <- POST(url, body = envelope, add_headers(headers),
                         timeout(max_timeout))
        status <- http_status(response)$message
        downloaded <- TRUE
      },error = function(e) {
        print(conditionMessage(e))
      }
      )
    )
    if (!downloaded) {
      attr(df, "download.time") <- download.time["elapsed"]
      attr(df, "download.status") <- err
      attr(df, "parse.time") <- NA
      attr(df, "parse.status") <- NA
      return(df)
    }

    status.code <- http_status(response)$category
    print(paste("download time:", round(download.time["elapsed"], 1), "seconds, status:", status.code))

  } else {
    #GetVariables using REST
    print(paste("downloading variables from:", server, "..."))

    downloaded <- FALSE
    download.time <- system.time(
      err <- tryCatch({
        response <- GET(server)
        status <- http_status(response)$message
        downloaded <- TRUE
      },error=function(e){
        print(conditionMessage(e))
      })
    )

    if (!downloaded) {
      attr(df, "download.time") <- download.time["elapsed"]
      attr(df, "download.status") <- err
      attr(df, "parse.time") <- NA
      attr(df, "parse.status") <- NA
      return(df)
    }

    status.code <- http_status(response)$category
    print(paste("download time:", download.time["elapsed"], "seconds, status:", status.code))
  }

  # check for server error category
  if (http_status(response)$category == "server error") {
    attr(df, "download.time") <- download.time["elapsed"]
    attr(df, "download.status") <- http_status(response)$message
    attr(df, "parse.time") <- NA
    attr(df, "parse.status") <- NA
    return(df)
  }


  attr(df, "download.time") <- download.time["elapsed"]
  attr(df, "download.status") <- status.code

  ######################################################
  # Parsing the WaterML XML Data                       #
  ######################################################

  begin.parse.time <- Sys.time()

  print("reading variables WaterML data...")
  doc <- NULL
  err <- tryCatch({
    doc <- xmlParse(response)
  }, warning = function(w) {
    print("Error reading WaterML: Bad XML format.")
    attr(df, "parse.status") <- "Bad XML format"
    attr(df, "parse.time") <- 0
    return(df)
  }, error = function(e) {
    print("Error reading WaterML: Bad XML format.")
    attr(df, "parse.status") <- "Bad XML format"
    attr(df, "parse.time") <- 0
    return(df)
  }
  )
  if (is.null(doc)) {
    print("Error reading WaterML: Bad XML format.")
    attr(df, "parse.status") <- "Bad XML format"
    attr(df, "parse.time") <- 0
    return(df)
  }

  # specify the namespace information
  ns <- WaterOneFlowNamespace(version)

  vars <- getNodeSet(doc, "//sr:variable", namespaces=ns)

  N <- xmlSize(vars)
  #define the columns
  df <- data.frame(
    VariableCode=rep("",N), FullVariableCode=rep("",N),
    VariableName=rep("",N), ValueType=rep("",N),
    DataType=rep("",N), GeneralCategory=rep("",N),SampleMedium=rep("",N),
    UnitName=rep(NA,N), UnitType=rep(NA,N), UnitAbbreviation=rep(NA,N),
    NoDataValue=rep(NA,N), IsRegular=rep("",N),
    TimeUnitName=rep("",N), TimeUnitAbbreviation=rep("",N),
    TimeSupport=rep("",N), Speciation=rep("",N),
    stringsAsFactors=FALSE)

  for(i in 1:N) {
    varObj <- vars[[i]]
    v <- unlist(xmlToList(varObj))
    varcode <- v["variableCode.text"]
    df$VariableCode[i] <- varcode
    df$FullVariableCode[i] <- paste(v["variableCode..attrs.vocabulary"], varcode, sep=":")
    df$VariableName[i] <- v["variableName"]
    df$ValueType[i] <- v["valueType"]
    df$DataType[i] <- v["dataType"]
    df$GeneralCategory[i] <- v["generalCategory"]
    df$SampleMedium[i] <- v["sampleMedium"]
    if (version == "1.1") {
      df$UnitName[i] <- ifelse(is.na(v["unit.unitName"]), v["units.unitName"], v["unit.unitName"])
      df$UnitType[i] <- v["unit.unitType"]
      df$UnitAbbreviation[i] <- v["unit.unitAbbreviation"]
      df$IsRegular[i] <- ifelse(is.na(v["timeScale..attrs.isRegular"]), v["timeScale.isRegular"],
                             v["timeScale..attrs.isRegular"])

      df$TimeUnitName[i] <- v["timeScale.unit.unitName"]
      df$TimeUnitAbbreviation[i] <- v["timeScale.unit.unitAbbreviation"]
      df$TimeSupport[i] <- v["timeScale.timeSupport"]
      df$NoDataValue[i] <- as.numeric(v["noDataValue"])
    } else {
      df$UnitName[i] <- v["units.text"]
      df$UnitType[i] <- v["units..attrs.unitsType"]
      df$UnitAbbreviation[i] <- v["units..attrs.unitsAbbreviation"]
      df$IsRegular[i] <- ifelse(is.na(v["timeSupport..attrs.isRegular"]), v["timeSupport.isRegular"],
                                   v["timeSupport..attrs.isRegular"])
      df$TimeUnitName[i] <- v["timeSupport.unit.UnitDescription"]
      df$TimeUnitAbbreviation[i] <- v["timeSupport.unit.UnitAbbreviation"]
      df$TimeSupport[i] <- v["timeSupport.timeInterval"]
      df$NoDataValue[i] <- as.numeric(v["NoDataValue"])
    }
    df$Speciation[i] <- v["speciation"]
  }

  end.parse.time <- Sys.time()
  parse.time <- as.numeric(difftime(end.parse.time, begin.parse.time, units="sec"))
  attr(df, "download.time") <- download.time["elapsed"]
  attr(df, "download.status") <- "success"
  attr(df, "parse.time") <- parse.time
  attr(df, "parse.status") <- "OK"
  return(df)
}

GetValues <- function(server, siteCode=NULL, variableCode=NULL, startDate=NULL, endDate=NULL,
                      methodID=NULL, sourceID=NULL, qcID=NULL, daily=NULL) {
  # GetValues
  #
  # This function gets the time series data values from the WaterML web service
  #
  # @import stats
  # @import XML
  # @import httr
  # @param server The URL of the web service,
  #  for example: http://worldwater.byu.edu/interactive/rushvalley/services/index.php/cuahsi_1_1.asmx?WSDL.
  #  This can be also a custom REST URL or the file name of the WaterML file.
  # @param siteCode The site code. To get a list of available site codes, see GetSites() function
  #  and use the FullSiteCode field.
  # @param variableCode The variable code. To get a list of possible variable codes, see GetVariables()
  #  function and use the FullVariableCode field
  # @param startDate (optional) The start date in "yyyy-mm-dd" format
  # @param endDate (optional) The end date in "yyyy-mm-dd" format
  # @param methodID (optional) The ID of the observation method. To get a list of possible method IDs, see
  # methodID column in the output of GetSiteInfo(). If methodID is not specified, then the observations
  # in the output data.frame won't be filtered by method.
  # @param sourceID (optional) The ID of the source. To get a list of possible source IDs, see
  # sourceID column in the output of GetSiteInfo(). If sourceID is not specified, then the observations
  # in the output data.frame won't be filtered by source.
  # @param qcID (optional) The ID of the quality control level. Typically 0 is used for raw data and 1 is
  # used for quality controlled data. To get a list of possible quality control level IDs, see
  # QualityControlLevelID column in the output of GetSiteInfo(). If qcID is not specified, then the
  # observations in the output data.frame won't be filtered by quality control level.
  # @param daily (optional) If you set daily="max", daily="min" or daily="mean", then the
  # data values are aggreagted to daily time step.
  # @return a data.frame of data values with the following columns:
  # \itemize{
  # \item time: The local date/time of the observation. The data type is POSIXct. POSIXct is
  #             a data type in R for storing time.
  # \item DataValue: The observed data value
  # \item UTCOffset: The difference between local time and UTC time in hours
  # \item CensorCode: The code for censored observations. Possible values are nc (not censored),
  #             gt (greater than), lt (less than),
  #             nd (non-detect), pnq (present but not quantified)
  # \item DateTimeUTC: The UTC time of the observation. The data type is POSIXct.
  #             POSIXct is a special data type in R for storing time.
  # \item MethodCode: The code of the method or instrument used for the observation
  # \item SourceCode: The code of the data source
  # \item QualityControlLevelCode: The code of the quality control level. Possible values are
  #             -9999 (Unknown), 0 (Raw data), 1 (Quality controlled data),
  #             2 (Derived products), 3 (Interpreted products), 4 (Knowledge products)
  # }
  # The output data.frame also has attributes with information about the status:
  # download.time, parse.time, download.status, parse.status
  # These attributes can be used for troubleshooting WaterOneFlow/WaterML server errors.
  # If parse status is "NO_VALUES_FOUND",
  # then this time series doesn't have any available data for the selected time period.
  # @keywords waterml
  # @export
  # @examples
  # #example 1: Get Values from a known site and variable from Ipswich River server.
  # server <- "http://hydroportal.cuahsi.org/ipswich/cuahsi_1_1.asmx?WSDL"
  # v1 <- GetValues(server, site="IRWA:FB-BV", variable="IRWA:DO",
  #                 startDate="1999-01-01", endDate="1999-12-31")
  # #example 2: Get values from an external REST URL (in this case the Provo USGS NWIS site id 10163000)
  # url <- "http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites=10163000&parameterCd=00060"
  # v2 <- GetValues(url)
  # #example 3: Get values from WaterML 2.0 file and show year, month, day
  # url2 <- "http://www.waterml2.org/KiWIS-WML2-Example.wml"
  # waterml2_data <- GetValues(url2)
  # waterml2_data$year <- strftime(waterml2_data$time, "%Y")
  # waterml2_data$month <- strftime(waterml2_data$time, "%M")
  # waterml2_data$day <- strftime(waterml2_data$time, "%d")

  #file or url?
  isFile <- FALSE

  # declare the default download timeout in seconds
  max_timeout = 360

  # declare empty return data frame
  df <- data.frame()

  # trim any leading and trailing whitespaces in server
  server <- gsub("^\\s+|\\s+$", "", server)

  #if server is a file name

  SOAP <- TRUE

  # if server ends with .asmx, we also assume that the service is SOAP and we add ?WSDL
  m1 <- regexpr("asmx$", server)
  if (m1 > 1) {
    server <- paste(server, "WSDL", sep="?")
  }

  #save variableCode for possible future use
  original_variable_code <- NULL
  if (!is.null(variableCode)) {
    original_variable_code <- variableCode
  }

  #check startDate, endDate if it is null
  startDateParam <- ifelse(is.null(startDate), "", strftime(as.POSIXct(startDate), "%Y-%m-%dT%H:%M:%S"))
  endDateParam <- ifelse(is.null(endDate), "", strftime(as.POSIXct(endDate), "%Y-%m-%dT%H:%M:%S"))

  # if server ends with ?WSDL or ?wsdl, we assume that service is SOAP
  # otherwise, assume that service is REST
  SOAP <- TRUE
  m <- regexpr("?WSDL|wsdl", server)
  if (m > 1) {
    url <- substr(server, 0, m - 2)
    SOAP <- TRUE
  } else {
    SOAP <- FALSE
  }

  #if the service is SOAP:
  if (SOAP) {
    versionInfo <- WaterOneFlowVersion(server)
    namespace <- versionInfo$Namespace
    version <- versionInfo$Version
    methodName <- "GetValuesObject"

    #format the variable with the methodID, sourceID, qcID parameters
    variableCodeParam <- variableCode
    if (!is.null(methodID)) {
      variableCodeParam <- paste(variableCodeParam, ":methodCode=", methodID, sep="")
    }
    if (!is.null(sourceID)) {
      variableCodeParam <- paste(variableCodeParam, ":sourceCode=", sourceID, sep="")
    }
    if (!is.null(qcID)) {
      variableCodeParam <- paste(variableCodeParam, ":qualityControlLevelCode=", qcID, sep="")
    }

    SOAPAction <- paste(namespace, methodName, sep="")
    envelope <- MakeSOAPEnvelope(namespace, methodName, c(location=siteCode,
                                                          variable=variableCodeParam,
                                                          startDate=startDateParam,
                                                          endDate=endDateParam))
    headers <- c("Content-Type" = "text/xml", "SOAPAction" = SOAPAction)

    print(paste("downloading values from:", url, "..."))

    downloaded <- FALSE
    download.time <- system.time(
      err <- tryCatch({
        response <- POST(url, body = envelope, add_headers(headers),
                         timeout(max_timeout))
        status <- http_status(response)$message
        downloaded <- TRUE
      },error = function(e) {
        warning(conditionMessage(e))
      }
      )
    )
    if (!downloaded) {
      attr(df, "download.time") <- as.numeric(download.time["elapsed"])
      attr(df, "download.status") <- err
      attr(df, "parse.time") <- NA
      attr(df, "parse.status") <- NA
      return(df)
    }

    status.code <- http_status(response)$category
    print(paste("download time:", round(download.time["elapsed"], 1), "seconds, status:", status.code))
    # check for bad status code
    if (tolower(status.code) != "success") {
      status.message <- http_status(response)$message
      attr(df, "download.time") <- as.numeric(download.time["elapsed"])
      attr(df, "download.status") <- status.message
      attr(df, "parse.time") <- NA
      attr(df, "parse.status") <- NA
      return(df)
    }
  } else {
    #REST
    version <- "1.1"

    if (substr(server, 1, 4) == "http")
    {
      print(paste("downloading values from:", server, "..."))

      downloaded <- FALSE
      download.time <- system.time(
        err <- tryCatch({
          response <- GET(server, timeout(max_timeout))
          status <- http_status(response)$message
          downloaded <- TRUE
        },error = function(e) {
          warning(conditionMessage(e))
        }
        )
      )
      if (!downloaded) {
        attr(df, "download.time") <- as.numeric(download.time["elapsed"])
        attr(df, "download.status") <- err
        attr(df, "parse.time") <- NA
        attr(df, "parse.status") <- NA
        return(df)
      }

      status.code <- http_status(response)$category
      print(paste("download time:", download.time["elapsed"], "seconds, status:", status.code))
      # check for bad status code
      if (tolower(status.code) != "success") {
        status.message <- http_status(response)$message
        attr(df, "download.time") <- as.numeric(download.time["elapsed"])
        attr(df, "download.status") <- status.message
        attr(df, "parse.time") <- NA
        attr(df, "parse.status") <- status.message
        return(df)
      }
    } else {
      #we are using a local file..
      isFile <- TRUE
    }
  }

  if (!isFile) {
    download.time <- as.numeric(download.time["elapsed"])
    download.status <- status.code
    attr(df, "download.time") <- download.time
    attr(df, "download.status") <- download.status
  } else {
    download.time <- 0
    download.status <- "success"
  }

  ######################################################
  # Parsing the WaterML XML Data                       #
  ######################################################
  begin.parse.time <- Sys.time()

  print("reading data values WaterML ...")
  doc <- NULL
  status.code <- "xml parse error"
  err <- tryCatch({
    if (isFile) {
      doc <- xmlParseDoc(server)
      status.code <- "success"
    } else {
      doc <- xmlParse(response)
    }
  }, warning = function(w) {
    warning(paste("Error reading WaterML:", conditionMessage(w)))
    attr(df, "parse.status") <- conditionMessage(w)
    attr(df, "parse.time") <- 0
    return(df)
  }, error = function(e) {
    warning(paste("Error reading WaterML:", conditionMessage(e)))
    attr(df, "parse.status") <- conditionMessage(e)
    attr(df, "parse.time") <- 0
    return(df)
  })
  if (is.null(doc)) {
    warning("WaterML data from the GetValues response could not be parsed.")
    attr(df, "parse.status") <- "XML parse error"
    attr(df, "parse.time") <- 0
    return(df)
  }

  # Check for WaterML version 2.0 (special code - adopted from dataRetrieval package..)
  waterml_version <- WaterMLVersion(doc)
  if (waterml_version == "2.0") {

    ns <- xmlNamespaceDefinitions(doc, simplify = TRUE)
    timeSeries <- xpathApply(doc, "//wml2:Collection", namespaces = ns)

    if(0 == length(timeSeries)){
      df <- data.frame()
      print("NOTE: No data values found in this time series")
      end.parse.time <- Sys.time()
      parse.time <- as.numeric(difftime(end.parse.time, begin.parse.time, units="sec"))
      attr(df, "parse.status") <- "NO_VALUES_FOUND"
      attr(df, "parse.time") <- parse.time
      return(df)
    }

    for (i in 1:length(timeSeries)){

      chunk <- xmlDoc(timeSeries[[i]])
      chunk <- xmlRoot(chunk)
      chunkNS <- xmlNamespaceDefinitions(chunk, simplify = TRUE)

      xp <- xpathApply(chunk, "//wml2:MeasurementTimeseries/wml2:point/wml2:MeasurementTVP",
                       xpathSApply, ".//*[not(*)]",
                       function(x) setNames(ifelse(nzchar(xmlValue(x)),
                                                   xmlValue(x),
                                                   ifelse("qualifier" == xmlName(x),
                                                          xpathSApply(x,"./@xlink:title",namespaces = ns),"")), #originally I had the "" as xmlAttr(x)
                                            xmlName(x,full=TRUE)),
                       namespaces = chunkNS)

      if(length(xpathApply(doc,
                           "//wml2:MeasurementTimeseries/wml2:point/wml2:MeasurementTVP/wml2:metadata/wml2:TVPMeasurementMetadata",
                           xmlValue, namespaces = ns)) != 0){
        xp <- xp[-1]
      }

      # allow for the case where optional <wml2:metadata> appears within <wml2:MeasurementTVP>
      if(length(xp) > 0){
        numElements = length(names(xp[[1]]))
      }

      xp2 <- unlist(xp)
      xpTimes <- xp2[seq(1, length(xp2), numElements)]
      xpVals <- xp2[seq(2, length(xp2), numElements)]

      DF2 <- data.frame(time=xpTimes, value=xpVals, stringsAsFactors = FALSE)

      DF2$time <- substr(gsub(":","",DF2$time),1, 17)
      DF2$time <- ifelse(nchar(DF2$time) > 18,
                         as.POSIXct(DF2$time, format="%Y-%m-%dT%H%M%S%z",tz="UTC"),
                         as.POSIXct(DF2$time, format="%Y-%m-%dT%H%M%S",tz="UTC"))

      DF2$time <- as.POSIXct(DF2$time, origin = "1970-01-01", tz="UTC")
      DF2$value <- as.numeric(DF2$value)

      #########################################
      # Very specific to USGS:
      defaultQualifier <- as.character(xpathApply(chunk, "//wml2:defaultPointMetadata/wml2:DefaultTVPMeasurementMetadata/wml2:qualifier/@xlink:title",namespaces = chunkNS))

      if (length(defaultQualifier) == 0 && (typeof(defaultQualifier) == "character")) {
        defaultQualifier <- "NA"
      }

      if("swe:value" %in% names(DF2)){
        isQual <- as.character(xpathApply(chunk,
                                          "//wml2:MeasurementTimeseries/wml2:point/wml2:MeasurementTVP/wml2:metadata/wml2:TVPMeasurementMetadata/wml2:qualifier/@xlink:title",
                                          namespaces = chunkNS))
        DF2$qualifier <- ifelse(defaultQualifier != isQual,isQual,defaultQualifier)
        DF2$`swe:value` <- NULL
      } else {
        DF2$qualifier <- rep(defaultQualifier,nrow(DF2))
      }

      names(DF2) <- c("time", "DataValue", "Qualifier")
      DF2$UTCOffset <- 0
      DF2$CensorCode <- "nc"
      DF2$DateTimeUTC <- DF2$LocalDateTime
      DF2$MethodCode <- NA
      DF2$SourceCode <- NA
      DF2$QualityControlLevelCode <- NA

      end.parse.time <- Sys.time()
      parse.time <- as.numeric(difftime(end.parse.time, begin.parse.time, units="sec"))
      attr(DF2, "download.time") <- download.time
      attr(DF2, "download.status") <- download.status
      attr(DF2, "parse.status") <- "OK"
      attr(DF2, "parse.time") <- parse.time
    }
    return (DF2)
  }

  # specify the namespace information
  ns <- WaterOneFlowNamespace(version)

  #try to find faultstring to look for an error
  fault <- xpathSApply(doc, "//soap:Fault", xmlValue, namespaces=ns)
  if (length(fault) > 0) {
    print(paste("SERVER ERROR in GetValues ", as.character(fault), sep=":"))
    end.parse.time <- Sys.time()
    parse.time <- as.numeric(difftime(end.parse.time, begin.parse.time, units="sec"))
    attr(df, "parse.status") <- as.character(fault)
    attr(df, "parse.time") <- parse.time
    return(df)
  }

  #again check for the status code
  if (tolower(status.code) == "server error") {
    print(paste("SERVER ERROR in GetValues ", http_status(response)$message))
    end.parse.time <- Sys.time()
    parse.time <- as.numeric(difftime(end.parse.time, begin.parse.time, units="sec"))
    attr(df, "parse.status") <- http_status(response)$message
    attr(df, "parse.time") <- parse.time
    return(df)
  }


  # extract the data columns with XPath
  val = xpathSApply(doc, "//sr:value", xmlValue, namespaces=ns)
  N <- length(val)

  # if N is 0: the document does not have data values or the xml is probably not valid
  if (N == 0) {
    timeSeriesElement <- unlist(xpathSApply(doc, "//sr:timeSeries", xmlValue, namespaces=ns))
    if (is.null(timeSeriesElement)) {
      print("Error reading WaterML: Bad XML format. <timeSeries> tag not found.")
      end.parse.time <- Sys.time()
      parse.time <- as.numeric(difftime(end.parse.time, begin.parse.time, units="sec"))
      attr(df, "parse.status") <- "Bad XML format. <timeSeries> tag not found."
      attr(df, "parse.time") <- parse.time
      return(df)
    } else {
      #no data values were found
      print("NOTE: No data values found in this time series")

      #special case: methodID, sourceID or qcID is specified. Try again with
      #empty methodID, sourceID, qcID
      if (!is.null(methodID) | !is.null(sourceID) | !is.null(qcID)) {
        print("Trying GetValues again without methodID, sourceID, qcID...")
        daily_param <- daily
        return(GetValues(server, siteCode, original_variable_code, startDate, endDate,
                         methodID=NULL, sourceID=NULL, qcID=NULL, daily=daily_param))
      }

      end.parse.time <- Sys.time()
      parse.time <- as.numeric(difftime(end.parse.time, begin.parse.time, units="sec"))
      attr(df, "parse.status") <- "NO_VALUES_FOUND"
      attr(df, "parse.time") <- parse.time
      return(df)
    }
  }

  #look for zoneOffset
  time_diff <- NULL
  zoneOffset <- xpathSApply(doc, "//sr:defaultTimeZone", xmlGetAttr, name="zoneOffset", namespaces=ns)
  zoneOffset <- unlist(zoneOffset)
  zoneName <- "GMT"
  if (length(zoneOffset) > 0) {
    offset_split <- strsplit(zoneOffset, ":")
    diff_text <- offset_split[[1]][1]
    time_diff <- as.difftime(as.numeric(diff_text), units="hours")
    if (as.numeric(diff_text) > 0) {
      zoneName <- paste("Etc/GMT+", as.numeric(diff_text), sep="")
    } else {
      zoneName <- paste("Etc/GMT", as.numeric(diff_text), sep="")
    }
  }

  bigData <- 10000
  if (N > bigData) {
    print(paste("found", N,"data values"))
  }

  if (N > bigData) { print("processing censorCode...") }
  censorCode = xpathSApply(doc, "//sr:value", xmlGetAttr, name="censorCode", namespaces=ns)
  censorCode <- unlist(censorCode)
  if (is.null(censorCode)) {
    censorCode <- rep("nc", N)
  }
  if (N > bigData) { print("processing qualifiers...") }
  qualifier <- xpathSApply(doc, "//sr:value", xmlGetAttr, name="qualifiers", namespaces=ns)
  qualifier <- unlist(qualifier)
  if (is.null(qualifier)) {
    qualifier <- rep(NA, N)
  }
  # currently we require that all values have a qualifier attached to it,
  # or none of the values have a qualifier
  if (length(qualifier) < N) {
    qualifier <- rep(NA, N)
  }

  if (version == "1.1") {

    #if defaultTimeZone is not specified, then read it for each value
    if (N > bigData) { print("processing dateTimeUTC...") }

    DateTimeUTC = xpathSApply(doc, "//sr:value", xmlGetAttr, name="dateTimeUTC", namespaces=ns)

    if (is.null(unlist(DateTimeUTC))) {
      time_diff <- 0
      diff_text <- "0"
    }

    if (is.null(time_diff)) {
      DateTimeUTC <- as.POSIXct(DateTimeUTC, format="%Y-%m-%dT%H:%M:%S", tz="GMT")

      UTCOffset = xpathSApply(doc, "//sr:value", xmlGetAttr, name="timeOffset", namespaces=ns)

      if (is.null(unlist(UTCOffset))) {
        utcDiff = rep(as.difftime(0, units="hours"), N)
      } else {
        UTCOffset <- ifelse(grepl(":", UTCOffset),
                            as.numeric(substr(UTCOffset, nchar(UTCOffset)-4, nchar(UTCOffset)-3)),
                            as.numeric(UTCOffset))
        utcDiff = as.difftime(UTCOffset, units="hours")
      }
      DateTime = as.POSIXct(DateTimeUTC + utcDiff)
      if (utcDiff[1] == 0) {
        attr(DateTime, "tzone") <- "Etc/GMT"
        UTCOffset = rep(0, N)
      } else {
        if (UTCOffset[1] > 0) {
          attr(DateTime, "tzone") <- paste("Etc/GMT+", UTCOffset[1], sep="")
        }
        if (UTCOffset[1] < 0) {
          attr(DateTime, "tzone") <- paste("Etc/GMT", UTCOffset[1], sep="")
        }
      }
    } else {
      DateTime <- xpathSApply(doc, "//sr:value", xmlGetAttr, name="dateTime", namespaces=ns)
      zone="GMT"
      if (as.numeric(diff_text) > 0) {
        zone <- paste("Etc/GMT+", as.numeric(diff_text), sep="")
      }
      if (as.numeric(diff_text) < 0) {
        zone <- paste("Etc/GMT", as.numeric(diff_text), sep="")
      }
      DateTime <- as.POSIXct(DateTime, format="%Y-%m-%dT%H:%M:%S", tz=zone)
      UTCOffset = rep(as.numeric(diff_text), N)
      DateTimeUTC = DateTime - time_diff
      attr(DateTimeUTC, "tzone") <- "GMT"
    }

    if (N > bigData) { print("processing methodCode...") }
    methodCode = xpathSApply(doc, "//sr:value", xmlGetAttr, name="methodCode", namespaces=ns)
    methodCode <- unlist(methodCode)
    if (is.null(methodCode)) { methodCode <- NA }

    if (N > bigData) { print("processing sourceCode...") }
    sourceCode = xpathSApply(doc, "//sr:value", xmlGetAttr, name="sourceCode", namespaces=ns)
    sourceCode <- unlist(sourceCode)
    if (is.null(sourceCode)) { sourceCode <- NA }

    if (N > bigData) { print("processing qualityControlLevelCode...") }
    qcCode = xpathSApply(doc, "//sr:value", xmlGetAttr, name="qualityControlLevelCode", namespaces=ns)
    qcCode <- unlist(qcCode)
    if (is.null(qcCode)) { qcCode <- NA }

    nodata = as.numeric(xpathSApply(doc, "//sr:noDataValue", xmlValue, namespaces=ns))

  } else {

    #WaterML 1.0 usually doesn't provide information on UTC offset
    if (N > bigData) { print("processing dateTime...") }
    DateTimeUTC = xpathSApply(doc, "//sr:value", xmlGetAttr, name="dateTime", namespaces=ns)
    DateTime <- DateTimeUTC
    UTCOffset <- rep(0, N)

    if (N > bigData) { print ("processing methodID...")}
    methodCode <-  xpathSApply(doc, "//sr:value", xmlGetAttr, name="methodID", namespaces=ns)
    methodCode <- unlist(methodCode)
    if (is.null(methodCode)) { methodCode <- NA }

    if (N > bigData) { print ("processing sourceID...")}
    sourceCode <- xpathSApply(doc, "//sr:value", xmlGetAttr, name="sourceID", namespaces=ns)
    sourceCode <- unlist(sourceCode)
    if (is.null(sourceCode)) { sourceCode <- NA }

    if (N > bigData) { print ("processing qualityControlLevel...")}
    qcCode = xpathSApply(doc, "//sr:value", xmlGetAttr, name="qualityControlLevel", namespaces=ns)
    qcCode <- unlist(qcCode)
    if (is.null(qcCode)) { qcCode <- NA }
    if (length(qcCode) < N) { qcCode <- NA }

    nodata = as.numeric(xpathSApply(doc, "//sr:NoDataValue", xmlValue, namespaces=ns))
  }

  #make the data frame
  df <- data.frame(
    time=DateTime,
    DataValue=as.numeric(val),
    UTCOffset=UTCOffset,
    Qualifier=qualifier,
    CensorCode=censorCode,
    DateTimeUTC=DateTimeUTC,
    MethodCode=methodCode,
    SourceCode=sourceCode,
    QualityControlLevelCode=qcCode,
    stringsAsFactors=TRUE
  )

  if (nrow(df) == 0) {
    print("NOTE: No data values found in this time series")
    end.parse.time <- Sys.time()
    parse.time <- as.numeric(difftime(end.parse.time, begin.parse.time, units="sec"))
    attr(df, "parse.status") <- "NO_VALUES_FOUND"
    attr(df, "parse.time") <- parse.time
    return(df)
  }

  #normal case: no aggregation
  df[df$DataValue == nodata,2] <- NA

  #special case: daily data aggregation
  if (!is.null(daily)) {
    validdata <- na.omit(df)
    if (nrow(validdata) == 0) {
      end.parse.time <- Sys.time()
      parse.time <- as.numeric(difftime(end.parse.time, begin.parse.time, units="sec"))
      attr(df, "parse.status") <- "NO_VALUES_FOUND"
      attr(df, "parse.time") <- parse.time
      print("no valid data found!")
      return(df)
    }
    validdata$time <- as.Date(as.POSIXct(validdata$time))
    dailyValues <- aggregate(validdata$DataValue, list(validdata$time), daily)
    names(dailyValues)[1] <- "time"
    names(dailyValues)[2] <- "DataValue"

    end.parse.time <- Sys.time()
    parse.time <- as.numeric(difftime(end.parse.time, begin.parse.time, units="sec"))
    attr(dailyValues, "download.status") <- attr(df, "download.status")
    attr(dailyValues, "download.time") <- attr(df, "download.time")
    attr(dailyValues, "parse.status") <- "OK"
    attr(dailyValues, "parse.time") <- parse.time
    return(dailyValues)
  }

  end.parse.time <- Sys.time()
  parse.time <- as.numeric(difftime(end.parse.time, begin.parse.time, units="sec"))
  attr(df, "download.time") <- download.time
  attr(df, "download.status") <- download.status
  attr(df, "parse.status") <- "OK"
  attr(df, "parse.time") <- parse.time
  return(df)
}
