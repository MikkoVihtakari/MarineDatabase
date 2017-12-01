#' @title Export meta-data to NPI database input format
#' @description Exports meta-data from Excel files to the format accepted by NPI data managers
#' @import openxlsx
#' @importFrom lubridate year month day
#' @export

#meta_file = paste0(devel, "Samplelog MOSJ 2015.xlsx")
#sheet = 1

export_metadata <- function(meta_file, sheet = 1, expedition = "Expedition", station = "Station", type = "Sample.type", sample_name = "Sample.name", longitude = "Longitude.(decimals)", latitude = "Latitude.(decimals)", date = "Sampling.date.(UTC)", bottom_depth = "Bottom.depth.(m)", gear = "Gear", from = "Sampling.depth.(m).from", to = "Sampling.depth.(m).to", filtered_volume = "Filtered.volume", responsible = "Responsible.person", comment = "Comment", additional = NULL) {

file_ext <- getFileNameExtension(meta_file)

if(file_ext %in% c("xlsx", "xls")) {
  dt <- read.xlsx(meta_file, sheet = sheet)
} else {
  stop("Other read methods than Excel have not been implemented yet")
}

required_cols <- c("expedition", "station", "type", "sample_name", "longitude", "latitude", "date", "bottom_depth", "gear", "from", "to", "filtered_volume", "responsible", "comment")

sapply(required_cols, function(k) {
  if(!get(k) %in% colnames(dt)) {
    stop(paste(k, "was not found from column names of dt. Check column name specifications or add the column."))
  }
})

## Structure

dt <- dt[unname(sapply(required_cols, function(k) get(k)))]
colnames(dt) <- required_cols
dt <- rapply(object = dt, f = factor, classes = "character", how = "replace")

## Dates

if(is.numeric(dt$date) & file_ext %in% c("xlsx", "xls")) {
  dt$temp_date <- convertToDateTime(dt$date, tz = "UTC")
  dt$date <- strftime(as.POSIXct(dt$temp_date, "UTC"), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  message(paste("Date converted to ISO 8601 format. Stored as", class(dt$date), "class assuming", getDateOrigin(meta_file), "as origin date. Control that dates match with the Excel sheet."))
} else {
  stop("Implement new date conversion. Does not work for these data.")
}

## Expedition

levels(dt$expedition) <- gsub(" ", "", levels(dt$expedition))
dt <- droplevels(dt)

if(nlevels(dt) > 1) warning("There are several levels for expedition in the meta-data.")

## Station

if(any(duplicated(tolower(levels(dt$station))))) warning("Station names may contain typos. Check the station names from the output.")

## Sample type

data(sample_types)

dt$temp_type <- select(strsplit(as.character(dt$sample_name), "\\-"), 1)

i <- 2
tp <- lapply(1:nrow(dt), function(i) {
  tmp <- dt[i,]
  temp_type2 <- ifelse(length(TYPES[TYPES$code %in% tmp$temp_type ,"sample_type"]) == 0, NA, TYPES[TYPES$code %in% tmp$temp_type ,"sample_type"])
  tmp$temp_type2 <- temp_type2
  tmp
})

dt <- do.call(rbind, tp)

removed <- dt[is.na(dt$temp_type2),]

dt <- dt[!is.na(dt$temp_type2),]

dt$type <- factor(dt$temp_type2)

## Coordinates

if(!is.numeric(dt$longitude)) {
  dt$longitude <- numeric(dt$longitude)
  warning("longitude converted to numeric. Check longitude records for possible mistakes. Should be decimal degrees")
}

if(!is.numeric(dt$latitude)) {
  dt$latitude <- numeric(dt$latitude)
  warning("latitude converted to numeric. Check latitude records for possible mistakes. Should be decimal degrees")
}

## Gear

data("gear_types")

i <- 1
tp <- lapply(1:nrow(dt), function(i) {
  #print(i)
  tmp <- dt[i,]
  if(!as.character(tmp$gear) %in% GEAR$gear) {
    temp_gear <- agrep(as.character(tmp$gear), GEAR$gear, value = TRUE)

    if(length(temp_gear) != 1) stop(paste("Gear type approximate matching does not work for row", i))

    tmp$gear <- temp_gear
  } else {
    tmp$gear <- tmp$gear
  }
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

## Filtered volume

if(!is.numeric(dt$filtered_volume)) {
suppressWarnings(dt$filtered_volume <- as.numeric(dt$filtered_volume))
warning("filtered_volume converted to numeric. NAs produced. Check the data.")
  }

## Responsible

dt$responsible <- as.character(dt$responsible)

## Output parameters

file_id <- paste(levels(dt$expedition), "metadata", sep = "_")

## Remove temporary columns

dt <- dt[-grep("temp", colnames(dt))]

## Output

out <- list(dt, removed, file_id)
names(out) <- c("meta", "deleted", "file_id")

class(out) <- "MetaData"

out

}


getFileNameExtension <- function (fn) {
# remove a path
splitted    <- strsplit(x=fn, split='/')[[1]]
# or use .Platform$file.sep in stead of '/'
fn          <- splitted [length(splitted)]
ext         <- ''
splitted    <- strsplit(x=fn, split='\\.')[[1]]
l           <-length (splitted)
if (l > 1 && sum(splitted[1:(l-1)] != ''))  ext <-splitted [l]
# the extention must be the suffix of a non-empty name
ext
}
