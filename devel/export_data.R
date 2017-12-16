#' @title Quality control data and export to NPI database input format
#' @description Opens Excel files with data, checks them against \link[export_metadata]{station meta-data}, and returns a data frame ready to be sent to NPI database managers.
#' @param data_file Name of the excel file containing the dataset to be exported.
#' @param sheet sheet number or name where meta-data are located. See \code{\link[openxlsx]{read.xlsx}}
#' @param meta \code{\link[MetaData]{export_metadata}} object containing meta-data for the station.
#' @param sample_name Column name in \code{data_file} specifying the sample name column. If NULL (default) the \code{sample_name} column is guessed using \code{\link{guess_colname}} function and search word "sample name".
#' @param value_cols Names of columns that contain values that should be exported to the database. If NULL (default), the column names are guessed based on the list of allowed variable types for each sample code.
#' @param units NULL (default) or a character vector specifying the units for \code{value_cols}. The character vector has to be same length than \code{value_cols}. If NULL, \code{\link{find_units}} function is used to find correct units for variables.  
#' @author Mikko Vihtakari
#' @import reshape2
#' @export

data_file = paste0(twice, "Ammonium/glacierfront_2017_ammonium_urea.xlsx"); sheet = "ammonium_urea_DB"; meta = export_metadata(paste0(twice, "GlacierFront_2017_Samplelog_20171211.xlsx"), sheet = "SAMPLELOG", guess_colnames = TRUE); sample_name = NULL; value_cols = c("Ammonium.[µM].average", "Urea.[µM]"); precision_cols = c("Ammonium.[µM].stdev", NULL); units = NULL
  
#export_data <- function(data_file, ) {
  
file_ext <- get_file_ext(data_file)

if(file_ext %in% c("xlsx", "xls")) {
  dt <- read.xlsx(data_file, sheet = sheet)
} else {
  stop("Other read methods than Excel have not been implemented yet")
}

 if(is.null(sample_name)) {
  sample_name <- unname(guess_colname("sample_name", dt))
 } 
 
 if(is.null(value_cols)) {
   stop("Guessing value_cols has not been implemented yet. Define value_cols as a character vector")
 }

## Subset relevant columns 
   
 dt <- dt[c(sample_name, value_cols, precision_cols)]
 colnames(dt)[colnames(dt) == sample_name] <- "sample_name" # rename sample_name column

## Column classes
 
suppressWarnings(dt[c(value_cols, precision_cols)] <- lapply(dt[c(value_cols, precision_cols)], function(g) as.numeric(as.character(g))))

## Add multiple sample names separated by ";" as own rows
 
 i = 97
 
 tp <- lapply(1:nrow(dt), function(i) {
  tmp <- dt[i,]
  
  ## Add samples that are separated by ; on one row as separate samples
  if(grepl(";", tmp$sample_name)) {
    tmp$n <- length(trimws(unlist(strsplit(as.character(tmp$sample_name), ";"))))
    tmp$sample_name <- trimws(unlist(strsplit(as.character(tmp$sample_name), ";")))[1]
    tmp
    } else {
     tmp$n <- 1
     tmp
   }
})

dt <- do.call(rbind, tp) 

## Remove rows with missing sample_name

removed <- dt[is.na(dt$sample_name),]

dt <- dt[!is.na(dt$sample_name),]

## Data to long format 

dt <- reshape2::melt(dt, id = "sample_name")

## Value column names ####

type <- unique(na.omit(select(strsplit(dt$sample_name, "-"), 1)))

if(length(type) != 1) stop("There are several sample types in data_file.")

allowed_types <- unlist(strsplit(TYPES[TYPES$code == type, "parameters"], ";"))
 
levels(dt$variable)

guess_colname()

 if(is.null(units)) {
   units <- find_units(value_cols)
 }
 
 

 
}