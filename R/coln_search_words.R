#' @title A list of search words used to find column names in Excel sheets
#' @description A list containing search words used in \code{\link{guess_colname}} function. 
#' @param column A required column name as character string
#' @param return_name Logical. Should name of \code{column} be returned instead of value from the list? Used in internal conditional functions.
#' @details The function accepts following required column names: \code{expedition}, \code{station}, \code{type}, \code{sample_name}, \code{longitude}, \code{latitude}, \code{date}, \code{bottom_depth}, \code{gear}, \code{from}, \code{to}, \code{responsible} and \code{comment}.
#' @author Mikko Vihtakari, Conrad Helgeland
#' @export

coln_search_words <- function(column, return_name = FALSE) {
  
  candidates <- list(
    expedition = "expedition",
    sample_name = "name",
    station = "station",
    latitude = "(latitude)|(latitude\\sdecimal)",
    longitude = "(longitude)|(longitude\\sdecimal)",
    bottom_depth = "bottom depth",
    date = "date",
    gear = "gear",
    from = "depth m from",
    to = "depth m to",
    #filtered_volume = "(filtered|filtration).volume",
    type = "type",
    responsible = "(person)|(responsible)|(contact)",
    comment = "comment")
  
  
  if(!column %in% names(candidates)) {
     stop(paste(column, "column type not set"))
  } else {
    if(return_name) names(candidates[column]) else {
      candidates[[column]]
    }
  }
  
  
}
