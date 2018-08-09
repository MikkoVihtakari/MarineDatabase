#' @title Read meta-data from Excel sheets to NPI database input format
#' @description Reads meta-data from Excel files to the format accepted by NPI data managers
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
#' @details The \strong{\code{\link{guess_colname}}} function is used to search for column names in \code{meta_file} by default. This procedure saves the user from specifying all the required column names (\code{expedition}, \code{station}, \code{type}, \code{sample_name}, \code{longitude}, \code{latitude}, \code{date}, \code{bottom_depth}, \code{gear}, \code{from}, \code{to}, \code{responsible} and \code{comment}). The function works well with tested Excel sheets, but might cause an error if column names are far from tested names. Modify \code{\link{coln_search_words}} in such case (guessing is done using regular expressions).
#' 
#' The easiest solution is to change the column names close to column names listed in Usage (or close to those listed in \code{\link{coln_search_words}}). Alternatively you can list all column names manually. Note that any white space in column names must be denoted by period (\code{.}). All column names should be specified as character strings of length 1. The column names refer to the meta-data table (\code{dt}).
#' 
#' \strong{Date conversion} between Excel and R is fairly unstable business as your current locale (i.e. to what time zone your computer is set to) might affect the outcome. The easiest way to convert dates is to enter \code{meta_file} as a character string specifying the Excel file name (and location). This allows the usage of \emph{openxlsx} package's \code{\link[openxlsx]{convertToDateTime}} function, which often reads date-times correctly, given that the dates are entered in a consistent format (this is not always the case in sample logs). 
#' 
#' Another option is to input a data frame already opened from Excel. This option might be necessary when doing changes to the data frame prior passing it through the \code{read_metadata} function. In this case, you have to make sure that \code{date_origin} is correct for your operating system (see \url{https://www.r-bloggers.com/date-formats-in-r/}).
#' 
#' The \code{add_time} argument can be used to add or subtract hours from the output, if the times do not match with those in the Excel sheet. This can be helpful in either cases, if your locale causes an offset between recorded dates. 
#' 
#' @return Returns a list of class \code{MetaData} that contains modified meta-data records (\code{$meta}), removed meta-data records (\code{$deleted}), file name to be used when saved for database import (\code{file_id}) and a data frame containing row numbers of duplicates in the original Excel sheet (\code{$duplicates}).
#' 
#' \strong{Dates} (\code{$meta$date}) are returned as UTC date-time in ISO 8601 format.
#' @examples \donttest{
#' ## Read meta-data and let the function to find errors in it:
#' x <- read_metadata("Samplelog MOSJ 2015.xlsx")
#'
#' ## Meta-data reading follows openxlsx syntax. 
#' ## You can add columns using the additional argument:
#' x <- read_metadata("GlacierFront_2017_Samplelog_20171024.xlsx", 
#' sheet = "SAMPLELOG", additional = c(Conveyance = "Sampled.from"))
#' }
#' @import openxlsx utils
#' @importFrom lubridate year month day
#' @export

# additional = NULL; add_time = 0; date_origin = "1899-12-30"; expedition = NULL; station = NULL; type = NULL; sample_name = NULL; longitude = NULL; latitude = NULL; date = NULL; bottom_depth = NULL; gear = NULL; from = NULL; to = NULL; responsible = NULL; comment = NULL; guess_colnames = TRUE

read_metadata <- function(meta_file, sheet = 1, additional = NULL, add_time = 0, date_origin = "1899-12-30", expedition = NULL, station = NULL, type = NULL, sample_name = NULL, longitude = NULL, latitude = NULL, date = NULL, bottom_depth = NULL, gear = NULL, from = NULL, to = NULL, responsible = NULL, comment = NULL, guess_colnames = TRUE) {

## File handling ####

if(is.data.frame(meta_file)) {
  dt <- meta_file
  file_ext <- NA
} else {
  file_ext <- get_file_ext(meta_file)

  if(file_ext %in% c("xlsx", "xls")) {
    dt <- openxlsx::read.xlsx(meta_file, sheet = sheet)
  } else {
    stop("Other read methods than Excel have not been implemented yet")
  }
}
    

required_cols <- c("expedition", "station", "type", "sample_name", "longitude", "latitude", "date", "bottom_depth", "gear", "from", "to", "responsible", "comment")

if(!guess_colnames) {
  sapply(required_cols, function(k) {
    if(is.null(get(k))) {
      stop(paste(k, "is not defined. All required columns have to be defined, if guess_colnames = FALSE"))
    } else if(!get(k) %in% colnames(dt)) {
        stop(paste(k, "was not found from column names of dt. Check column name specifications or add the column."))
    }
  })
}

## Quality flag to whether meta-data can be merged with data

quality.flag <- TRUE
dups <- NULL
dup.rows <- NULL
old.data.format <- FALSE

## Structure

if(guess_colnames) {
  def_cols <- unname(sapply(required_cols, function(k) !is.null(get(k))))
  
  if(any(def_cols)) {
    guess_cols <- required_cols[!def_cols]
    
    dc <- sapply(required_cols[def_cols], function(k) get(k))
    dc2 <- guess_colname(guess_cols, dt)
    
    dc_all <- c(dc, dc2)
    dc_all <- dc_all[required_cols]
    
    if(any(is.na(dc_all))) {
      stop(paste(required_cols[is.na(dc_all)], collapse = ", "), " column was not found. It is required.")
    }
    
    dt <- dt[c(unname(dc_all), unname(additional))]
  
    } else {
    
    dc_all <- guess_colname(required_cols, dt)
    
    if(any(is.na(dc_all))) {
      stop(paste(required_cols[is.na(dc_all)], collapse = ", "), " column was not found. It is required.")
    }
    
    dt <- dt[c(unname(dc_all), unname(additional))]
  }
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
  dt$temp_date <- openxlsx::convertToDateTime(dt$date, tz = "UTC")
  dt$temp_date <- dt$temp_date + add_time*3600
  dt$date <- strftime(as.POSIXct(dt$temp_date, "UTC"), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  message(paste("Date converted to ISO 8601 format. Stored as", class(dt$date), "assuming", openxlsx::getDateOrigin(meta_file), "as origin date. Control that dates match with the Excel sheet. You can use add_time to adjust if there is offset."))
} else {
  if(is.numeric(dt$date)) {
  dt$temp_date <- as.POSIXct(as.numeric(dt$date) * (60*60*24), tz = "UTC", origin = date_origin)
  dt$temp_date <- dt$temp_date + add_time*3600
  dt$date <- strftime(as.POSIXct(dt$temp_date, "UTC"), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  message(paste("Date converted to ISO 8601 format. Stored as", class(dt$date), "class assuming", date_origin, "as origin date. Control that dates match with the Excel sheet. You can use add_time to adjust if there is offset."))
  } else {
  if(class(dt$date) == "Date") {
  dt$temp_date <- dt$date + add_time*3600
  dt$date <- strftime(as.POSIXct(dt$temp_date, "UTC"), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  message(paste("Date converted to ISO 8601 format. Stored as", class(dt$date), "class assuming", date_origin, "as origin date. Control that dates match with the Excel sheet. You can use add_time to adjust if there is offset."))
  } else {
    
  ## If date is character (meaning there are typos), try to fix them
  if(class(dt$date) == "character") {
  temp_date <- suppressWarnings(is.na(as.numeric(dt$date)))
  if(any(temp_date)) {
    
    temp_date <- lapply(dt$date, function(k) {
      if(grepl("UTC", k)) {
        out <- strptime(k, format = "%Y-%m-%d %H:%M", tz = "UTC")
        out <- out + add_time*3600
       
      } else {
        out <- strptime(k, format = "%d.%m.%Y %H:%M", tz = "UTC")
        out <- out + add_time*3600
      } 
      
        if(is.na(out)) { #last save
        out <- as.POSIXct(as.numeric(k) * (60*60*24), tz = "UTC", origin = date_origin)
        out <- out + add_time*3600
        }
          
       strftime(as.POSIXct(out, "UTC"), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
      
      })
    
  temp_date <- unlist(temp_date)
    
  if(any(is.na(temp_date))) { 
  warning("Typo in date format for records ", paste(unique(dt$date[is.na(temp_date)]), collapse = ", "), " on rows ", paste(which(is.na(temp_date)), collapse = ", "), ". NAs produced.")
  
  dt$date <- temp_date  
    
  } else {
    
    dt$date <- temp_date
  message(paste("Date converted to ISO 8601 format. Stored as", class(dt$date), "class. Control that dates match with the Excel sheet. You can use add_time to adjust if there is offset."))
  
  }} else {
  dt$temp_date <- as.POSIXct(as.numeric(dt$date) * (60*60*24), tz = "UTC", origin = date_origin)
  dt$temp_date <- dt$temp_date + add_time*3600
  dt$date <- strftime(as.POSIXct(dt$temp_date, "UTC"), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  message(paste("Date converted to ISO 8601 format. Stored as", class(dt$date), "class assuming", date_origin, "as origin date. Control that dates match with the Excel sheet. You can use add_time to adjust if there is offset."))  
  }
    
  } else {
  stop("Implement new date conversion. Does not work for these data.")  

}}}}


## Expedition 

levels(dt$expedition) <- gsub(" ", "", levels(dt$expedition))
dt <- droplevels(dt)

if(nlevels(dt) > 1) warning("There are several levels for expedition in the meta-data.")

## Station 

if(any(duplicated(tolower(levels(dt$station))))) warning("Station names may contain typos. Check the station names from the output.")
levels(dt$station) <- trimws(levels(dt$station))

## Sample type 

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

if(all(is.na(original$temp_type2))) {
  message("Sample codes are not according to the standard. Old data format? Sanity checks reduced.")
  dt <- original
  removed <- original[0,]
  old.data.format <- TRUE
} else {
  removed <- original[is.na(original$temp_type2),]
  removed <- removed[-grep("temp", colnames(removed))]
  removed$sample_name <- factor(removed$sample_name)
  removed$gear <- factor(removed$gear)
  
  dt <- original[!is.na(original$temp_type2),]
  dt$type <- factor(dt$temp_type2)
}

## Warn about sample names that do not contain enough 0s 

if(!old.data.format) {

dt$temp_sample_name <- dt$sample_name # needed to find duplicate rows from original sheet

tmp <- strsplit(as.character(dt$sample_name), split = "-")
index <- unlist(lapply(tmp, function(k) nchar(gsub("[[:alpha:]]", "", k[2]))))

if(any(is.na(index))) warning(length(dt$sample_name[is.na(index)]), " sample_names (", paste(unique(dt$sample_name[is.na(index)]), collapse=", "), ") are missing sample number.")

if(any(index[!is.na(index)] < 3)) {
  new_names <- sapply(as.character(dt$sample_name), function(k) {
    
    tmp <- strsplit(k, split = "-")[[1]]
    Index <- nchar(gsub("[[:alpha:]]", "", tmp[2]))
    
    if(any(Index >= 3, is.na(Index))) {
      k
    } else {
      tmp[2] <- ifelse(nchar(tmp[2]) == 1, paste0("00", tmp[2]), ifelse(nchar(tmp[2]) == 2, paste0("0", tmp[2]), tmp[2]))
      paste(tmp, collapse = "-")
    }
    
  })
  if(!identical(as.character(dt$sample_name), names(new_names))) stop("Name conversion does not work.")
  
  message(paste0(sum(index < 3, na.rm = TRUE), " sample_names contained too few numbers (from ", dt$sample_name[index < 3][1], " to ", dt$sample_name[index < 3][length(dt$sample_name[index < 3])], "). Replaced by new names (from ", new_names[index < 3][1], " to ", new_names[index < 3][length(new_names[index < 3])], ")."))
  
  dt$sample_name <- unname(new_names)
}

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
  
  if(old.data.format & !tmp$temp_type %in% TYPES$code) {
    temp_gear <- "old"
  } else {
  
  if(tmp$temp_type == "CTD") {
    
    temp_gear <- TYPES[TYPES$code == "CTD", "gear_type"]
  
    } else {
    if(tmp$temp_type == "CTM") {
      
      temp_gear <- TYPES[TYPES$code == "CTM", "gear_type"]
      
    } else {
    if(grepl("Ice core", tmp$gear)) {
      
      temp_gear <- grep(gsub("\\D*", "", tmp$gear, perl = TRUE), grep("Ice core", GEAR$gear, value = TRUE), value = TRUE)
      if(length(temp_gear) == 0) stop("Ice corer diameter not found. Check and add to the list.")
      
    } else {      
      #if(tmp$gear == "Ice corer 14cm") tmp$gear <- "Ice corer 14 cm"
      #if(tmp$gear == "Ice corer 9cm") tmp$gear <- "Ice corer 9 cm"
        
      if(!as.character(tmp$gear) %in% GEAR$gear) {
        
              temp_gear <- agrep(tmp$gear, GEAR$gear, value = TRUE)

      # Exceptions
          if(tmp$gear == "Multinet") temp_gear <- grep("200", temp_gear, value = TRUE)
          if(length(temp_gear) != 1) temp_gear <- agrep(gsub("[[:punct:]]", " ", tmp$gear), GEAR$gear, value = TRUE)
          if(length(temp_gear) != 1 & length(TYPES[TYPES$code == tmp$temp_type, "gear_type"]) != 0) {
            candidates <- trimws(unlist(strsplit(TYPES[TYPES$code == tmp$temp_type, "gear_type"], "\\;")))
            result <- agrep(tmp$gear, candidates, value = TRUE)
            
            if(length(result) == 0) {
              temp_gear <- candidates[1]
              #message(paste0("Gear guessed for ", tmp$sample_name))
            } else {
              temp_gear <- result[1]
            }
          }
          if(length(temp_gear) != 1) stop(paste("Fuzzy matching", tmp$gear, "does not work for row", i))    
  } else {
    temp_gear <- tmp$gear
  }}}}}
  
  temp_gear
  
})

tp <- unlist(tp)

if(any(tp == "old")) message("Old data format. Gear types not standardized")

dt$gear <- ifelse(tp == "old", dt$gear, tp)

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

  dups <- as.character(dt$temp_sample_name[duplicated(dt[c("expedition", "sample_name")])])
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
