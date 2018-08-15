##' @title Print \code{MetaData} objects
##' @description \code{\link{print}} function for \code{\link[=read_metadata]{MetaData}} objects
##' @param x \code{MetaData} object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print MetaData
##' @export
##' @author Mikko Vihtakari
##' @seealso \code{\link{read_metadata}}

print.MetaData <- function(x, ...) {

  cat("NPI Marine Database MetaData object", sep = "\n")
  cat(paste("From", as.character(levels(x$meta$expedition)), "expedition"), sep = "\n")
  cat(NULL, sep = "\n")
  cat("A list of data containing following elements:", sep = "\n")
  cat(names(x), sep = ", ")
  cat(NULL, sep = "\n")
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
  if(!is.null(x$duplicates)) {
    cat(paste(length(x$duplicate_names), "sample names are duplicated. Write $duplicates to see them."))
  } else {
    cat("No duplicates.")
  }
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  cat(ifelse(x$merge_allowed, "Merging with data allowed.", "Merging with data is not allowed. Fix the duplicate problem."))
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste0("File name: ", x$file_id, ".csv"))
  cat(NULL, sep = "\n")
  }

