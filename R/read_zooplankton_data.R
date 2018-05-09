#' @title Read NPI's zooplanton data from an Excel sheet
#' @description Reads NPI's standard format zooplankton data
#' @param data_file Path to the Excel file containing zooplankton data
#' @param sheet The name or index of the sheet to read the zooplanton data from. See \code{\link[openxlsx]{read.xlsx}}
#' @param dataStart The row number where zooplanton data starts from
#' @param dataEnd The row number where zooplankton data ends
#' @param dataCols Optional numeric index indicating the column numbers that contain zooplankton data
#' @param control_stations Logical indicating whether station names should be controlled against a list of standardized station names (see \code{\link{STATIONS}}).
#' @param output_format Output formar for date. See \code{\link{convert_dates}}.
#' @param add_coordinates If \code{TRUE} coordinates will be added to metadata from the list of standardized station names.
#' @param control_species Either \code{NULL} or a named list giving the name of species, stage and length columns in data, which will be used to control species names against a list of standardized zooplanton species (see \code{ZOOPL}).
#' @param species_info_cols Character vector specifying the names of species information columns that should be preserved. If \code{control_species = TRUE}, the infomation will be used to match species entries agains the zooplankton lookup list (see \code{ZOOPL}).
#' @param lookup_cols Character vector specifying the names of columns from the zooplankton lookup list (\code{ZOOPL}) that should be returned together with \code{species_info_cols}. If \code{NULL} (default), only \code{species_info_cols} will be returned. Has no effect, if \code{control_species = FALSE}. 
#' @param remove_missing Logical indicating whether species with column sums of 0 should be removed from the output.
#' @return Returns a list of class \code{ZooplanktonData}. The list contains 3 data frames: \code{$data} (abundance data), \code{$meta} (meta-data), and \code{$splist} (species information).
#' @import openxlsx
#' @importFrom plyr mapvalues
#' @author Mikko Vihtakari
#' @export

# Test parameters
# data_file = "Data/Kongsfjorden_zooplankton_allyears.xlsx"; sheet = "ALL ind m3"; dataStart = 11; dataEnd = 266; control_stations = TRUE; output_format = "as.Date"; add_coordinates = TRUE; dataCols = NULL; control_species = list(species = "species", stage = "stage", length = "length"); species_info_cols = c("group", "species", "stage", "length"); lookup_cols = NULL; remove_missing = TRUE
# data_file = "Data/Kongsfjord_zooplankton_2005.xlsx"; sheet = "Arkusz1"; dataStart = 11; dataEnd = 1000; control_stations = TRUE; output_format = "as.Date"; species_info_cols = c("species", "stage", "length"); lookup_cols = c("size_group", "origin", "biomass_conv"); add_coordinates = TRUE; control_species = list(species = "species", stage = "stage", length = "length"); dataCols = NULL


read_zooplankton_data <- function(data_file, sheet = 1, dataStart = 11, dataEnd = 1000, dataCols = NULL, control_stations = TRUE, output_format = "as.Date", add_coordinates = FALSE, control_species = list(species = "species", stage = "stage", length = "length"), species_info_cols = c("species", "stage", "length"), lookup_cols = NULL, remove_missing = TRUE) {

## Open the file ####
  
if(is.data.frame(data_file)) {
  stop("Other read methods than Excel have not been implemented yet")
  #dt <- data_file
  #file_ext <- NA
} else {
  file_ext <- get_file_ext(data_file)

  if(file_ext %in% c("xlsx", "xls")) {
    dt <- openxlsx::read.xlsx(data_file, sheet = sheet, startRow = dataStart, rows = dataStart:dataEnd)
    meta <- openxlsx::read.xlsx(data_file, sheet = sheet, startRow = 1, rows = 1:(dataStart-1), colNames = FALSE)
    meta <- t(meta)
  } else {
    stop("Other read methods than Excel have not been implemented yet")
  }
}


##################
#### Metadata ####

tmp <- as.data.frame(meta[-1,], stringsAsFactors = FALSE)
names(tmp) <- make.names(as.character(meta[1,]))
tmp <- tmp[!grepl("NA", names(tmp))]

### Column names

colns <- guess_colname(cols = c("expedition", "station", "vessel", "sample_name", "date", "from", "to", "unit", "comment"), df = tmp)
tmp <- tmp[colns]
names(tmp) <- names(colns)

## Column classes

numbers_only <- function(x) suppressWarnings(all(!is.na(as.numeric(as.character(x)))))

# k <- tmp$date
tmp[names(tmp)] <- lapply(names(tmp), function(k) {
  if(numbers_only(tmp[[k]])) {
    as.numeric(as.character(tmp[[k]]))
  } else {
    tmp[[k]]
  }
})

### Dates

tmp <- convert_dates(dt = tmp, excel_file = data_file, file_ext = file_ext, output_format = output_format)

### Station 

tmp$station <- factor(tmp$station)

## Control stations

if(control_stations) {
  
  alternatives <- lapply(STATIONS$variations, function(l) {
    trimws(unlist(strsplit(l, ";")))
  })
  
  #k <- tolower(levels(tmp$station))[4]
  new_names <- sapply(tolower(levels(tmp$station)), function(k) {
    STATIONS[unlist(lapply(alternatives, function(l) k %in% l)), "station"]
  })
  
  if(any(is.na(new_names))) {
    stop("Could not find all station names. Check or use control_stations = FALSE")
  } else {
    levels(tmp$station) <- unname(new_names)
  }
} 

if(any(duplicated(tolower(levels(tmp$station))))) warning("Station names may contain typos. Check the station names from the output.")

### Expedition

tmp$expedition <- factor(tmp$expedition)
levels(tmp$expedition) <- gsub(" ", "", levels(tmp$expedition))
levels(tmp$expedition) <- gsub("\\d", "", levels(tmp$expedition))

## Sample name and other known factors

factor_cols <- c("type", "sample_name", "vessel", "unit")

tmp[names(tmp) %in% factor_cols] <- lapply(tmp[names(tmp) %in% factor_cols], function(k) factor(k))

## Add coordinates

if(add_coordinates) {
  tmp$longitude <- as.numeric(as.character(plyr::mapvalues(tmp$station, STATIONS$station, STATIONS$lon, warn = FALSE)))
  tmp$latitude <- as.numeric(as.character(plyr::mapvalues(tmp$station, STATIONS$station, STATIONS$lon, warn = FALSE)))
}

## Add ID

tmp$id <- paste0(abbreviate(tmp$expedition, method = "both.sides"), substr(tmp$date, 3,4), "-", tmp$station, "-", gsub(" ", "", tmp$sample_name), "_", tmp$from, "-", tmp$to)

## Final manipulation

required_cols <- c("expedition", "station", "type", "sample_name", "longitude", "latitude", "date", "bottom_depth", "gear", "from", "to", "responsible", "comment")

first_cols <- names(tmp)[names(tmp) %in% required_cols]
first_cols <- required_cols[required_cols %in% first_cols]  

tmp <- tmp[c(first_cols, names(tmp)[!names(tmp) %in% required_cols])]

meta <- droplevels(tmp)

row.names(meta) <- 1:nrow(meta)

# meta2 <- meta %>% group_by(expedition, station, longitude, latitude, date, vessel) %>% summarise(from = max(from), to = min(to)) %>% arrange(date, station)
# meta2 <- as.data.frame(meta2)
# write.xlsx(list("all_samples" = meta, "stations" = meta2), file = "kongsfjorden zooplankton metadata.xlsx")

##############
#### Data ####

## Species information ####

sp <- dt[species_info_cols]

if(is.list(control_species)) {
  
  sp$species <- gsub("\\(cf.\\)", "", sp$species)
  sp$species <- gsub(" Total", "", sp$species)
  sp$species <- gsub(" indet.", "", sp$species)
  sp$species <- trimws(sp$species)
  sp$idno <- 1:nrow(sp)
  
  sp <- merge(sp, ZOOPL[c("species", "stage", "length", "species_ID", lookup_cols)], by.x = unname(unlist(control_species)), by.y = c("species", "stage", "length"), all.x = TRUE, sort = FALSE)

  sp <- sp[order(sp$idno),]
  sp <- sp[!names(sp) %in% "idno"]
  
  if(any(is.na(sp$species_ID))) {
    tmp <- sp[is.na(sp$species_ID), "species"]
    sp$species_ID[is.na(sp$species_ID)] <- ifelse(nchar(tmp) > 8, abbreviate(tmp, minlength = 6), tmp)
  }
    
  names(sp)[names(sp) %in% "species_ID"]  <- "id"

} else {

### Short species names

tmp <- gsub("Calanus finmarchicus", "Cfin", trimws(sp$species))
tmp <- gsub("Calanus glacialis", "Cgla", tmp)
tmp <- gsub("Calanus hyperboreus", "Chyp", tmp)
tmp <- gsub("Pseudocalanus spp.", "PSCAL", tmp)
tmp <- gsub("Pseudocalanus acuspes", "PSCALacu", tmp)
tmp <- gsub("Pseudocalanus minutus", "PSCALmin", tmp)
tmp <- gsub("Metridia longa", "Mlong", tmp)
tmp <- gsub("Microcalanus spp.", "MICRCAL", tmp)
tmp <- gsub("Paraeuchaeta spp.", "PARA", tmp)
tmp <- gsub("Paraeuchaeta norvegica", "PARAnor", tmp)
tmp <- gsub("Thysanoessa inermis", "THYine", tmp)
tmp <- gsub("Thysanoessa longicaudata", "THYlon", tmp)
tmp <- gsub("Thysanoessa raschii", "THYras", tmp)
tmp <- gsub("Meganyctiphanes norvegica", "MEGAnor", tmp)
tmp <- gsub("Beröe cucumis", "BERcuc", tmp)
tmp <- gsub("Euphausiacea", "EUPH", tmp)
tmp <- gsub("Total", "T", tmp)
tmp <- gsub("\\(cf.\\)", "", tmp)
tmp <- trimws(gsub("\\.", "", tmp))
tmp <- ifelse(nchar(tmp) > 8, abbreviate(tmp, minlength = 6), tmp)

tmp2 <- gsub("CV", "5", trimws(sp$stage))
tmp2 <- gsub("AF", "F", tmp2)
tmp2 <- gsub("AM", "M", tmp2)
tmp2 <- gsub("CIV", "4", tmp2)
tmp2 <- gsub("CIII", "3", tmp2)
tmp2 <- gsub("C3", "3", tmp2)
tmp2 <- gsub("III", "3", tmp2)
tmp2 <- gsub("CII", "2", tmp2)
tmp2 <- gsub("CI", "1", tmp2)
tmp2 <- gsub("F M", "F-M", tmp2)
tmp2 <- gsub("ikke delt i lengdegrupper", "", tmp2)
tmp2 <- gsub("larvae furcilia", "furcilia", tmp2)
tmp2 <- gsub("larvae calyptopis", "calyptopis", tmp2)
tmp2 <- gsub("larvae metatrochophora", "metatrochophora", tmp2)
tmp2 <- gsub("veliger \\(cf. Margarites\\)", "veliger", tmp2)
tmp2 <- gsub("larvae mitraria", "mitraria", tmp2)
tmp2 <- ifelse(nchar(tmp2) > 4, abbreviate(tmp2), tmp2)

tmp3 <- gsub("=>", "L", trimws(sp$size_op))
tmp3 <- gsub("<", "S", tmp3)

short_names <- ifelse(!is.na(tmp2), paste0(tmp, tmp2), ifelse(!is.na(sp$length), paste0(tmp, tmp3, sp$length), tmp))

sp$id <- make.names(short_names, unique = TRUE)

}

## Remove Excel's extra whitespace...
sp <- rapply(sp, function(x) trimws(x), classes = "character", how = "replace")

## Abundance data ####

if(is.null(dataCols)) {

  tmp <- names(dt)[grepl("X", names(dt)) & sapply(dt, class) == "numeric"]
  tmp <- as.numeric(gsub("\\D", "", tmp))
  firstCol <- paste0("X", min(tmp[tmp > 4]))
  dat <- dt[which(names(dt) == firstCol):ncol(dt)]

  } else {
dat <- dt[dataCols]  
}

dat <- t(dat)

dat <- as.data.frame(dat, stringsAsFactors = FALSE)

if(nrow(meta) != nrow(dat)) stop("Number of meta-data rows does not match with number of data rows. The error may be caused by the function not being able to define data columns correctly. Try using a numeric index in dataCols argument")

if(nrow(sp) != ncol(dat)) stop("Number of species list rows and data does not match. No idea why this happens. Try debugging the function section by section.")

colnames(dat) <- sp$id
rownames(dat) <- meta$id

############################
#### Compile and export ####

### Remove missing species

if(remove_missing) {
  missing_sps <- names(dat)[colSums(dat, na.rm = TRUE) == 0]
  dat <- dat[!names(dat) %in% missing_sps]
  dat[is.na(dat)] <- 0
  
  sp <- sp[!sp$id %in% missing_sps,]
}

out <- list(data = dat, meta = meta, splist = sp)

class(out) <- "ZooplanktonData"

return(out)

}
#write.xlsx(dat, file = "control_del.xlsx", row.names = TRUE)



