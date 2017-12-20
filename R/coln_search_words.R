#' @title A list of search words used to find column names in Excel sheets
#' @description A switch list containing search words used in \code{guess_colname} function
#' @param column A required column name as character string
#' @details The function accepts following required column names: \code{expedition}, \code{station}, \code{type}, \code{sample_name}, \code{longitude}, \code{latitude}, \code{date}, \code{bottom_depth}, \code{gear}, \code{from}, \code{to}, \code{filtered_volume}, \code{responsible} and \code{comment}.
#' @author Mikko Vihtakari, Conrad Helgeland
#' @export

coln_search_words <- function(column) {
  switch (column,
  expedition = "expedition",
  sample_name = "name",
  station = "station",
  latitude = "latitude decimal",
  longitude = "longitude decimal",
  bottom_depth = "bottom depth",
  date = "date",
  gear = "gear",
  from = "depth m from",
  to = "depth m to",
  filtered_volume = "(filtered|filtration).volume",
  type = "type",
  responsible = "person",
  comment = "comment",
  stop(paste(column, "column type not set"))
)
}
