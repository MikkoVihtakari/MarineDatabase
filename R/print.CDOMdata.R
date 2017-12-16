##' @title Print \code{CDOMdata} objects
##' @description \code{\link{print}} function for \code{\link[=process_cdom_data]{CDOMdata}} objects
##' @param x \code{CDOMdata} object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print CDOMdata
##' @export
##' @author Mikko Vihtakari
##' @seealso \code{\link{process_cdom_data}}

print.CDOMdata <- function(x, ...) {

  cat("CDOMdata object", sep = "\n")
  cat(NULL, sep = "\n")
  cat("A list of data containing following elements:", sep = "\n")
  cat(names(x), sep = ", ")
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste0("File size ", format(object.size(x), units = "Kb"), ". ", nlevels(x$data$sample_name), " samples."), sep = "\n")
  cat(NULL, sep = "\n")
  cat("Spectral data (use $spectra to index):", sep = "\n")
  cat(paste0(nrow(x$spectra), " rows. ", ncol(x$spectra), " columns. ", "Wavelengths between ", min(x$spectra$wavelength), " and ", max(x$spectra$wavelength), " nm."), sep = "\n")
  cat(NULL, sep = "\n")
  cat("Data (use $data to index):", sep = "\n")
  print(format(head(x$data), justify = "left"), row.names = TRUE)
  cat(paste0("Contains ", sum(is.na(x$data$sl400)), " bad samples."), sep = "\n")
  }

