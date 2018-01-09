#' @title Export meta-data to NPI database input format
#' @description Exports meta-data from Excel files to the format accepted by NPI data managers
#' @param meta_file File name of the meta-data table or a data frame containing meta-data. If file name, the file has to have \code{.xlsx} extension.
#' @param sheet sheet number or name where meta-data are located. See \code{\link[openxlsx]{read.xlsx}}
#' @param guess_colnames Logical indicating whether \code{\link{guess_colname}} function should be used to search for required column names. Defaults to \code{TRUE}. See Details.
#' @param additional Additional columns to be included in meta-data. Must be specified as a named character vector, which lists the exact column names to be included together with exported column names as names of the vector. See Examples.
#' @param add_time Hours to be added to the ISO 8601 \code{date}s. See Details. 
#' @param date_origin The origin for recorded dates in the Excel sheet in "YYYY-MM-DD" format. See Details.
#' @param expedition Column name specifying the name of the expedition. See Details.
#' @param station Column name specifying the station name. See Details.
#' @param type Column name specifying the sample type column. See Details.
#' @param sample_name Column name specifying the sample name column. See Details.
#' @param longitude Column name specifying the longitude column. Coordinates should be given in decimal degrees. See Details.
#' @param latitude Column name specifying the latitude column. Coordinates should be given in decimal degrees. See Details.
#' @param date Column name specifying the sampling time. The information will be transformed to ISO 8601 standard format. See note about date conversion in Details. See Details.
#' @param bottom_depth Column name specifying the bottom depth during sampling. See Details.
#' @param gear Name of the column specifying the sampling gear. See Details.
#' @param from Name of the column specifying the depth from which sampling was started. See Details.
#' @param to Name of the column specifying the depth from which sampling was ended. See Details.
#' @param responsible Name of the column specifying the responsible persons for the sampling. See Details.
#' @param comment Name of the column specifying comments. Any non-numeric values in \code{from} and \code{to} will be transferred to this column. See Details.
#' @details The \strong{\code{\link{guess_colname}}} function is used to search for column names in \code{meta_file} by default. This procedure saves the user from specifying all the required column names (\code{expedition}, \code{station}, \code{type}, \code{sample_name}, \code{longitude}, \code{latitude}, \code{date}, \code{bottom_depth}, \code{gear}, \code{from}, \code{to}, \code{filtered_volume}, \code{responsible} and \code{comment}). The function works well with tested Excel sheets, but might cause an error if column names are far from tested names.
#' 
#' The easiest solution is to change the column names close to column names listed in Usage (or close to those listed in \code{\link{coln_search_words}}). Alternatively you can list all column names manually. Note that any white space in column names must be denoted by period (\code{.}). All column names should be specified as character strings of length 1. The column names refer to the meta-data table (\code{dt}).
#' 
#' \strong{Date conversion} between Excel and R is fairly unstable business as your current locale (i.e. to what time zone your computer is set to) might affect the outcome. The easiest way to convert dates is to enter \code{meta_file} as a character string specifying the Excel file name (and location). This allows the usage of \emph{openxlsx} package's \code{\link[openxlsx]{convertToDateTime}} function, which often reads date-times correctly, given that the dates are entered in a consistent format (this is not always the case in sample logs). 
#' 
#' Another option is to input a data frame already opened from Excel. This option might be necessary when doing changes to the data frame prior passing it through the \code{export_metadata} function. In this case, you have to make sure that \code{date_origin} is correct for your operating system (see \url{https://www.r-bloggers.com/date-formats-in-r/}).
#' 
#' The \code{add_time} argument can be used to add or subtract hours from the output, if the times do not match with those in the Excel sheet. This can be helpful in either cases, if your locale causes an offset between recorded dates. 
#' 
#' @return Returns a list of class \code{MetaData} that contains modified meta-data records (\code{$meta}), removed meta-data records (\code{$deleted}), file name to be used when saved for database import (\code{file_id}) and a data frame containing row numbers of duplicates in the original Excel sheet (\code{$duplicates}).
#' 
#' \strong{Dates} (\code{$meta$date}) are returned as UTC date-time in ISO 8601 format.
#' @examples \donttest{
#' ## Read meta-data and let the function to find errors in it:
#' x <- export_metadata("Samplelog MOSJ 2015.xlsx")
#'
#' ## Meta-data reading follows openxlsx syntax. 
#' ## You can add columns using the additional argument:
#' x <- export_metadata("GlacierFront_2017_Samplelog_20171024.xlsx", 
#' sheet = "SAMPLELOG", additional = c(Conveyance = "Sampled.from"))
#' }
#' @import openxlsx utils
#' @importFrom lubridate year month day
#' @export

#meta_file = paste0(devel, "Samplelog MOSJ 2015.xlsx") ;sheet = 1 ;expedition = "Expedition"; station = "Station"; type = "Sample.type"; sample_name = "Sample.name"; longitude = "Longitude.(decimals)"; latitude = "Latitude.(decimals)"; date = "Sampling.date.(UTC)"; bottom_depth = "Bottom.depth.(m)"; gear = "Gear"; from = "Sampling.depth.(m).from"; to = "Sampling.depth.(m).to"; filtered_volume = "Filtered.volume"; responsible = "Responsible.person"; comment = "Comment"; additional = NULL; guess_colnames = FALSE

#meta_file = paste0(twice, "GlacierFront_2017_Samplelog_20171211.xlsx"); sheet = "SAMPLELOG"; guess_colnames = TRUE#; filtered_volume = "Filtration.volume.(ml)"; responsible = "Contact.person"

#meta_file = paste0(devel, "test.xlsx"); sheet = 1; guess_colnames = TRUE; additional = NULL; add_time = 0


export_metadata <- function(meta_file, sheet = 1, guess_colnames = TRUE, additional = NULL, add_time = 0, date_origin = "1899-12-30", expedition = "Expedition", station = "Station", type = "Sample.type", sample_name = "Sample.name", longitude = "Longitude.(decimals)", latitude = "Latitude.(decimals)", date = "Sampling.date.(UTC)", bottom_depth = "Bottom.depth.(m)", gear = "Gear", from = "Sampling.depth.(m).from", to = "Sampling.depth.(m).to", responsible = "Responsible.person", comment = "Comment") {

## File handling ####

if(is.data.frame(meta_file)) {
  dt <- meta_file
  file_ext <- NA
} else {
  file_ext <- get_file_ext(meta_file)

  if(file_ext %in% c("xlsx", "xls")) {
    dt <- read.xlsx(meta_file, sheet = sheet)
  } else {
    stop("Other read methods than Excel have not been implemented yet")
  }
}
    

required_cols <- c("expedition", "station", "type", "sample_name", "longitude", "latitude", "date", "bottom_depth", "gear", "from", "to", "responsible", "comment")

if(!guess_colnames) {
  sapply(required_cols, function(k) {
  if(!get(k) %in% colnames(dt)) {
    stop(paste(k, "was not found from column names of dt. Check column name specifications or add the column."))
  }
})
}

## Quality flag to whether meta-data can be merged with data

quality.flag <- TRUE
dups <- NULL
dup.rows <- NULL

## Structure

if(guess_colnames) {
  dt <- dt[c(unname(guess_colname(required_cols, dt)), unname(additional))]
} else {
 dt <- dt[c(unname(sapply(required_cols, function(k) get(k))), unname(additional))]
}

colnames(dt) <- c(required_cols, names(additional))

## Trim whitespace

dt$sample_name <- trimws(dt$sample_name)

## Column classes

factor.cols <- c("expedition", "station", "type")
dt[factor.cols] <- lapply(dt[factor.cols], function(k) factor(k))

## Dates ####

if(is.numeric(dt$date) & file_ext %in% c("xlsx", "xls")) {
  dt$temp_date <- convertToDateTime(dt$date, tz = "UTC")
  dt$temp_date <- dt$temp_date + add_time*3600
  dt$date <- strftime(as.POSIXct(dt$temp_date, "UTC"), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  message(paste("Date converted to ISO 8601 format. Stored as", class(dt$date), "assuming", getDateOrigin(meta_file), "as origin date. Control that dates match with the Excel sheet. You can use add_time to adjust if there is offset."))
} else {
  if(is.numeric(dt$date)) {
  dt$temp_date <- as.POSIXct(as.numeric(dt$date) * (60*60*24), tz = "UTC", origin = date_origin)
  dt$temp_date <- dt$temp_date + add_time*3600
  dt$date <- strftime(as.POSIXct(dt$temp_date, "UTC"), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  message(paste("Date converted to ISO 8601 format. Stored as", class(dt$date), "class assuming", date_origin, "as origin date. Control that dates match with the Excel sheet. You can use add_time to adjust if there is offset."))
  } else {
  stop("Implement new date conversion. Does not work for these data.")
}}

## Expedition 

levels(dt$expedition) <- gsub(" ", "", levels(dt$expedition))
dt <- droplevels(dt)

if(nlevels(dt) > 1) warning("There are several levels for expedition in the meta-data.")

## Station 

if(any(duplicated(tolower(levels(dt$station))))) warning("Station names may contain typos. Check the station names from the output.")

## Sample type 

#data(sample_types)

dt$temp_type <- MarineDatabase::select(strsplit(as.character(dt$sample_name), "\\-"), 1)

#i <- 2
tp <- lapply(1:nrow(dt), function(i) {
  tmp <- dt[i,]
  
  ## Add samples that are separated by ; on one row as separate samples
  if(grepl(";", tmp$sample_name)) {
    tp <- lapply(trimws(unlist(strsplit(as.character(tmp$sample_name), ";"))), function(g) {
      tp <- tmp
      tp$sample_name <- g
      tp
    })
   tmp <- do.call(rbind, tp) 
  }
  
  temp_type2 <- ifelse(length(TYPES[TYPES$code %in% tmp$temp_type ,"sample_type"]) == 0, NA, TYPES[TYPES$code %in% tmp$temp_type ,"sample_type"])
  tmp$temp_type2 <- temp_type2
  tmp
})

original <- do.call(rbind, tp)

removed <- original[is.na(original$temp_type2),]
removed <- removed[-grep("temp", colnames(removed))]
removed$sample_name <- factor(removed$sample_name)
removed$gear <- factor(removed$gear)
  
dt <- original[!is.na(original$temp_type2),]
dt$type <- factor(dt$temp_type2)

## Warn about sample names that do not contain enough 0s 

tmp <- strsplit(as.character(dt$sample_name), split = "-")
index <- unlist(lapply(tmp, function(k) nchar(gsub("[[:alpha:]]", "", k[2]))))

old_names <- as.character(dt$sample_name[index < 3])

if(length(old_names) > 0) {
new_names <- sapply(strsplit(old_names, "-"), function(k) {
    k[2] <- ifelse(nchar(k[2]) == 1, paste0("00", k[2]), ifelse(nchar(k[2]) == 2, paste0("0", k[2]), k[2]))
    paste(k, collapse = "-")
  })

levels(dt$sample_name)[levels(dt$sample_name) %in% old_names] <- new_names

message(paste0(length(old_names), " sample_names contained too few numbers (from ", old_names[1], " to ", old_names[length(old_names)], "). Replaced by new names (from ", new_names[1], " to ", new_names[length(new_names)], ")."))
}

dt$sample_name <- factor(dt$sample_name)

## Coordinates

if(!is.numeric(dt$longitude)) {
  dt$longitude <- as.numeric(as.character(dt$longitude))
  warning("longitude converted to numeric. Check longitude records for possible mistakes. Should be decimal degrees")
}

if(!is.numeric(dt$latitude)) {
  dt$latitude <- as.numeric(as.character(dt$latitude))
  warning("latitude converted to numeric. Check latitude records for possible mistakes. Should be decimal degrees")
}

## Gear ####

#data("gear_types")
dt$gear <- as.character(dt$gear)

i <- 1
tp <- lapply(1:nrow(dt), function(i) {
  #print(i)
  tmp <- dt[i,]
  
  if(is.na(tmp$gear)) tmp$gear <- "Blank"
  
  if(tmp$temp_type == "CTD") {
    temp_gear <- TYPES[TYPES$code == "CTD", "gear_type"]
  } else {
    if(tmp$temp_type == "CTM") {
      temp_gear <- TYPES[TYPES$code == "CTM", "gear_type"]
    } else {
    
      if(tmp$gear == "Ice corer 14cm") tmp$gear <-  "Ice corer 14 cm"
      if(tmp$gear == "Ice corer 9cm") tmp$gear <- "Ice corer 9 cm"
        
      if(!as.character(tmp$gear) %in% GEAR$gear) {
        
              temp_gear <- agrep(tmp$gear, GEAR$gear, value = TRUE)

      # Exceptions
          if(tmp$gear == "Multinet") temp_gear <- grep("200", temp_gear, value = TRUE)
          if(length(temp_gear) != 1) temp_gear <- agrep(gsub("[[:punct:]]", " ", tmp$gear), GEAR$gear, value = TRUE)
          if(length(temp_gear) != 1 & length(TYPES[TYPES$code == tmp$temp_type, "gear_type"]) != 0) temp_gear <- TYPES[TYPES$code == tmp$temp_type, "gear_type"]
          if(length(temp_gear) != 1) stop(paste("Fuzzy matching", tmp$gear, "does not work for row", i))    
  } else {
    temp_gear <- tmp$gear
  }}}
  
    tmp$gear <- temp_gear
    tmp
})


dt <- do.call(rbind, tp)

dt$gear <- factor(dt$gear)

## Comments

dt$comment <- as.character(dt$comment)

## Sampling depths

if(!is.numeric(dt$from)) {
dt <- convert_sampling_depths(dt, "from")
}

if(!is.numeric(dt$to)) {
dt <- convert_sampling_depths(dt, "to")
}

## Bottom depth

if(!is.numeric(dt$bottom_depth)) {
  suppressWarnings(dt$bottom_depth <- as.numeric(as.character(dt$bottom_depth)))
  message("bottom_depth converted to numeric. NAs maybe produced. Check the data.")
}

## Filtered volume

# if(!is.numeric(dt$filtered_volume)) {
#   suppressWarnings(dt$filtered_volume <- as.numeric(as.character(dt$filtered_volume)))
#   message("filtered_volume converted to numeric. NAs maybe produced. Check the data.")
# }
# 
# if(any(na.omit(dt$filtered_volume < 10))) warning("Filtered volumes should be given in ml. Values less than 10 ml were found. Are you sure?")

## Responsible

dt$responsible <- as.character(dt$responsible)

## Duplicated sample types

if(any(duplicated(dt[c("expedition", "sample_name")]))) {

  dups <- as.character(dt$sample_name[duplicated(dt[c("expedition", "sample_name")])])
  dup_dat <- original[original$sample_name %in% dups, c("sample_name", "type", "station")]
  dup_dat$row_number <- as.numeric(rownames(dup_dat)) +1
  dup_dat <-  dup_dat[order(dup_dat$sample_name),]
  rownames(dup_dat) <- 1:nrow(dup_dat)

  warning(paste(length(dups), "sample names are duplicated. Write $duplicates to see them. Note that this problem has to be fixed before you can use the meta-data to export data files"))
quality.flag <- FALSE
} else {
  dups <- NULL
  dup_dat <- NULL
}

## Output parameters ####

file_id <- paste(levels(dt$expedition), "metadata", sep = "_")

## Remove temporary columns

dt <- dt[-grep("temp", colnames(dt))]

#rm(TYPES)
#rm(GEAR)

## Output

out <- list(dt, removed, file_id, dups, dup_dat, quality.flag)
names(out) <- c("meta", "deleted", "file_id", "duplicate_names", "duplicates", "merge_allowed")

class(out) <- "MetaData"

## Messages

if(nrow(out$deleted) != 0) message(paste(nrow(out$deleted), "entries removed. Write $deleted to see the entries"))

## Return

out

}
