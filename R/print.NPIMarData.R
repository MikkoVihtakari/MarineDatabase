##' @title Print \code{NPIMarData} objects
##' @description \code{\link{print}} function for \code{\link[=make_station_data]{NPIMarData}} objects
##' @param x \code{NPIMarData} object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print NPIMarData
##' @import data.tree
##' @export
##' @author Mikko Vihtakari
##' @seealso \code{\link{make_station_data}}

print.NPIMarData <- function(x, ...) {

  title <- "NPI Marine Database object"
  title2 <- paste0("A list of data from ", names(x), ". File size ", format(object.size(x), units = "Kb"), ". ", length(x), " separate stations.")

  titlels <- "Data list structure with following elements:"

  treestr <- FromListSimple(x)

  cat(title, sep = "\n")
  cat(NULL, sep = "\n")
  cat(title2)
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  cat(titlels, sep = "\n")
  print(treestr)
  }

