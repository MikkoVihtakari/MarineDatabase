##' @title Print \code{ZooplanktonData} objects
##' @description \code{\link{print}} function for \code{\link[=read_zooplankton_data]{ZooplanktonData}} objects
##' @param x \code{ZooplanktonData} object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print ZooplanktonData
##' @family ZooplanktonData
##' @export
##' @author Mikko Vihtakari

print.ZooplanktonData <- function(x, ...) {

  cat("NPI Marine Database ZooplanktonData object", sep = "\n")
  cat("A list of data containing following elements:", sep = "\n")
  cat(names(x), sep = ", ")
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste0("File size ", format(object.size(x), units = "Kb"), ". ", nrow(x$meta), " samples, ", length(unique(x$splist$species)), " unique species, and ", nrow(x$splist), " species entries"), sep = "\n")
  cat(NULL, sep = "\n")
  cat("Abundance data (use $data to index):", sep = "\n")
  cat(paste0(nrow(x$data), " rows. ", ncol(x$data), " columns."), sep = "\n")
  cat(NULL, sep = "\n")
  cat("Meta-data (use $meta to index):", sep = "\n")
  cat(paste0(nrow(x$meta), " rows. ", ncol(x$meta), " columns."), sep = "\n")
  cat(paste("Temporal range:", min(x$meta$date), "to" , max(x$meta$date)), sep = "\n")
  cat(paste("Unique stations:", length(unique(select(strsplit(x$meta$id, "_"), 1)))), sep = "\n")
  cat(NULL, sep = "\n")
  cat("Species information (use $splist to index):", sep = "\n")
  cat(paste0(nrow(x$splist), " rows. ", ncol(x$splist), " columns."), sep = "\n")
  }
