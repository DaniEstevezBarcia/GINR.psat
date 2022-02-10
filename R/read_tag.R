#' Read either argos or internal data from tags
#'
#' @description
#' read_tag loads a file provided by a Psat tag from Wildlife computers Inc and makes a number of adjustments from the existing variables on the data. The function works differently whether you got data from the satellite (argos) or from the tag itself (internal).
#' @param tagfile string Path to the file where tag data is stored.
#' @param type string One of two values: "argos" or "internal".
#' @param temporal double Variable containing the time at which an individual observation took place. This variable is intended to make the internal variable of time readable in POSIXct format. Default NULL.
#' @param tz string Time zone to be used for modifying temporal variable in internal data. Set to UTC by default.
#' @param delim string The separation between columns in the data set. Either ",", " ", "\\t".
#' @param keep logical Specifies whether to keep the temporal variable which is tranformed to POSIXct. Default FALSE
#' @return Object of class data.frame, tbl_df and tbl (same as in dplyr function read_delim)
#' @export
read_tag <- function(tagfile, type, temporal = NULL, tz = "GMT", delim = ",", keep = FALSE) {
  message("Reading PSAT tag data")
  
  Date <- NULL
  
  if (type == "argos") {
    data <- readr::read_delim(file = tagfile, col_names = T, delim = delim, col_types = readr::cols())
  } else if (type == "internal") {
    data <- readr::read_delim(file = tagfile, col_names = T, delim = delim, col_types = readr::cols(), comment = "#")
    
    if (keep == FALSE) {
      data <- dplyr::mutate(data, Date = as.POSIXct({{ temporal }}, origin = "1970-01-01", tz = tz)) %>%
        dplyr::select(-{{ temporal }})
      data <- data[, c(ncol(data), 1:(ncol(data) - 1))]
    } else if (keep == TRUE) {
      data <- dplyr::mutate(data, Date = as.POSIXct({{ temporal }}, origin = "1970-01-01", tz = tz))
      data <- data[, c(ncol(data), 1:(ncol(data) - 1))]
    }
  } else if (any(type != c("argos", "internal"))) {
    stop("The type of file is not recognised by the function")
  }
  
  beepr::beep(sound = "ping")
  
  return(data)
}
