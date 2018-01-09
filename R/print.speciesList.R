##' @title Print \code{speciesList} objects
##' @description \code{\link{print}} function for \code{\link[=make_species_list]{speciesList}} objects
##' @param x \code{speciesList} object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print speciesList
##' @export
##' @author Mikko Vihtakari
##' @seealso \code{\link{make_species_list}}

print.speciesList <- function(x, ...) {

  cat("speciesList object", sep = "\n")
  cat(NULL, sep = "\n")
  cat("A list of data containing following elements:", sep = "\n")
  cat(names(x), sep = ", ")
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste0(nlevels(x$data$or_name), " species records. Tidied to ", nlevels(x$data$search_term), " search terms"), sep = "\n")
  cat(NULL, sep = "\n")
  cat("Species list (use $data to index):", sep = "\n")
  cat(paste0(nrow(x$data), " rows. ", ncol(x$data), " columns."), sep = "\n")
  cat(NULL, sep = "\n")
  print(format(head(x$data), justify = "left"), row.names = TRUE)
  cat("Summarized information (use $sum_info to index):", sep = "\n")
  print(format(x$sum_info, justify = "left"), row.names = TRUE)
  }

