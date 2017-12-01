##' @title Print \code{MetaData} objects
##' @description \code{\link{print}} function for \code{\link[=export_metadata]{MetaData}} objects
##' @param x \code{MetaData} object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print MetaData
##' @import data.tree
##' @export
##' @author Mikko Vihtakari
##' @seealso \code{\link{make_station_data}}

print.MetaData <- function(x, ...) {

  cat("NPI Marine Database MetaData object", sep = "\n")
  cat(paste("From", as.character(levels(x$meta$expedition)), "expedition"))
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  cat("A list of data containing following elements:", names(x), sep = " ")
  cat(NULL, sep = "\n")
  cat(paste0("File size ", format(object.size(x), units = "Kb"), ". ", nlevels(x$meta$station), " separate stations."), sep = "\n")
  cat(NULL, sep = "\n")
  cat("Meta-data (use $meta to index):", sep = "\n")
  cat(paste0(nrow(x$meta), " records. ", ncol(x$meta), " columns."), sep = "\n")
  cat("Stations:", levels(x$meta$station), sep = " ")
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  cat("Deleted meta-data (use $deleted to index):", sep = "\n")
  cat(paste0(nrow(x$deleted), " records. ", sep = "\n"))
  cat("Sample names:", as.character(unique(x$deleted$sample_name)), sep = " ")
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste0("File name: ", x$file_id, ".csv"))
  }

