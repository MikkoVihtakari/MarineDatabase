#' @title Compile data and meta-data to wide format
#' @description Compiles data with meta-data using the standardized sample names. The compiled datasets can be readily exported as Excel sheets.
#' @param dat A MarineDatabase data object. A data file that has passed through one of the process data functions in the MarineDatabase package, such as \code{\link{process_cdom_data}}.
#' @param meta A \code{\link[=export_metadata]{MetaData}} object.
#' @param na.rm Logical indicating whether sample names that are missing from meta-data should be removed. Defaults TRUE.
#' @details The \code{sample_name} column is used to merge data. Sample names in \code{dat} and \code{meta} have to match and be unique.
#' @return Returns a data frame that contains all entries in \code{dat} that could be \code{\link{merge}}d with \code{meta}.
#' @author Mikko Vihtakari
#' @export
#' 

compile_data <- function(dat, meta, na.rm = TRUE) {

if(any(duplicated(dat$data$sample_name))) stop(paste(dat$data$sample_name[duplicated(dat$data$sample_name)], "are duplicated in dat. Sample names have to be unique for function to work."))

## Extract information ####

data_type <- unique(select(strsplit(as.character(dat$data$sample_name), "-"), 1))

if(length(data_type) > 1) warning("Several data types in dat. The function might not work as intended.")

## Merge ####

x <- merge(meta$meta, dat$data, by = "sample_name", all.y = TRUE)

## Remove columns ####

if(data_type %in% c("CDO")) {
  x <- x[!colnames(x) %in% "filtered_volume"]
}

## Quality checks and conditions ####

missing_samples <- as.character(x[is.na(x$type), "sample_name"])

if(length(missing_samples) > 0) warning("Samples ", paste(missing_samples, collapse = ", "), " were not found from meta-data. Check the sample names")

if(!all(dat$data$sample_name %in% x$sample_name)) warning("Samples ", paste(dat$data$sample_name[!dat$data$sample_name %in% x$sample_name], collapse = ", "), "removed") 

if(na.rm) {
  x <- x[!is.na(x$type),]
}

## Return data frame ####

x

}