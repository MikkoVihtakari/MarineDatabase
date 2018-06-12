#' @title Match station names against standardized station names for Kongsfjorden and Ripfjorden
#' @description Matches and fixes inconsistencies in station names for NPI's standard marine biological stations
#' @param x A character vector or factor to be matched
#' @return Returns a names character vector of corrected station names
#' @author Mikko Vihtakari
#' @export


fix_station_names <- function(x) {

 alternatives <- lapply(STATIONS$variations, function(l) {
    trimws(unlist(strsplit(l, ";")))
  })
 
  #k <- tolower(x)[4]
  new_names <- sapply(tolower(gsub(" ", "", x)), function(k) {
    STATIONS[unlist(lapply(alternatives, function(l) k %in% l)), "station"]
  })
  
  if(any(is.na(new_names))) {
    warning("Could not find all station names.")
  } else {
    new_names
  }
  
}