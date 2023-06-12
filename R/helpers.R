
get_waterml_attributes <- function(obj) {
  return(list(
    download_time = attr(obj, "download.time"),
    download_status = attr(obj, "download.status"),
    parse_status = attr(obj, "parse.status"),
    parse_time = attr(obj, "parse.time")
  ))
}
