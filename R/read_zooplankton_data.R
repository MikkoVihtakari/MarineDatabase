#' @title Read NPI zooplanton data from an Excel sheet
#' @description Reads IOPAN and NPI standard format zooplankton data
#' @param data_file Path to the Excel file containing zooplankton data
#' @param sheet The name or index of the sheet to read the zooplanton data from. See \code{\link[openxlsx]{read.xlsx}}
#' @param dataStart The row number where zooplanton data starts from. If \code{NULL} (default), the starting row number is guessed based on the first record of "Calanus finmarchicus". 
#' @param dataEnd The row number where zooplankton data ends. Larger than real row numbers in data are ignored. The default is 1000. Set to a higher value, if your dataset has more rows than that. 
#' @param dataCols Optional numeric index indicating the column numbers that contain zooplankton data. Not implemented yet.
#' @param control_stations Logical indicating whether station names should be controlled against a list of standardized station names (see \code{\link{STATIONS}}). Should be \code{FALSE} for any other dataset than the standard monitoring (MOSJ) datasets.
#' @param output_format Output formar for date. See \code{\link{convert_dates}}.
#' @param add_coordinates If \code{TRUE} coordinates will be added to metadata from the list of standardized station names.
#' @param control_species Either \code{NULL} or a named list giving the name of species, stage, size operator and length columns in data, which will be used to control species names against a list of standardized zooplanton species (see \code{\link{ZOOPL}}). Recommended. Setting this to \code{NULL} may lead to unexpected behaviour. The size operator (\code{size_op}) element is used to match old type zooplankton files (most of them) and can be ignored for the new standardized files by setting \code{size_op = NULL}. 
#' @param species_info_cols Character vector specifying the names of species information columns that should be preserved. Required only if \code{control_species = NULL}, otherwise ignored. Adds some flexibility if species names are messed up, but use of \code{control_species} list is recommended.
#' @param lookup_cols Character vector specifying the names of columns from the zooplankton lookup list (\code{\link{ZOOPL}}) that should be returned together with \code{species_info_cols}. If \code{NULL} (default), only \code{species_info_cols} will be returned. Has no effect, if \code{control_species = FALSE}. 
#' @param remove_missing Logical indicating whether species with column sums of 0 should be removed from the output.
#' @param control_sample_names Logical indicating whether non-standard symbols in sample names should be replaced by standardized equivalents. May fix problems when trying to merge zooplankton samples with meta data from another file. These names tend to have typos. 
#' @param round2ceiling Logical indicating whether decimals should be rounded to ceiling integers: some Polish data come rounded this way and combining data may require standardizing all values to ceiling integers.  
#' @return Returns a list of class \code{ZooplanktonData}. The list contains 3 data frames: \code{$data} (abundance data), \code{$meta} (meta-data), and \code{$splist} (species information).
#' @details Zooplankton taxonomy data from IOPAN are received in (more or less) standard format on MS Excel sheets. This function attempts to read that format and enable passing data to futher manipulation in R. The structure of the Excel sheet is explained in Figure 1. 
#' 
#' \figure{zooplankton_data_sheet.png}{options: width=700}
#' 
#' Figure 1. Example how zooplankton Excel sheets tend to be arranged. 
#'  \enumerate{
#'     \item \strong{Meta data} are arranged row-vise (with headers on rows) and should contain following fields: "expedition", "station", "sample_name", "date", "from", "to", "unit", and "comment". The field names will be \link[=guess_colname]{guessed}. If the function does not guess the names correctly, try changing the names to the required field names. The \code{dataCols} argument may be used as help to specify the column indices containing data to help the function (currently not implemented). 
#'     \item \strong{Data} are listed column-wise for each station. Make sure that there are \strong{no blank data columns with meta data} (entirely blank columns are OK) as the function does not manage to separate such columns yet. Specify the row number for beginning (\code{dataStart}) of the data section. Rows > \code{dataStart} will be considered as meta data. The \code{dataEnd} argument can be used in cases where the sheet contains scrap data. Rows > \code{dataEnd} will be dropped. 
#'     \item \strong{Species list} is arranged column-wise and the field headers should be listed in the \code{control_species} argument.
#'     \item The correct Excel sheet containing all data is often named "ALL...", but this varies (purple text).
#' }
#' 
#' The function sums up duplicate species entries for each sample. The function attemps to match the species names in \code{data_file} with the accepted ones listed in \code{\link{ZOOPL}}. Sometimes this routine fails and manual fixes are required. 
#' 
#' The function is currently relatively unstable and most likely requires manual debugging for each dataset. 
#' @family ZooplanktonData
#' @import openxlsx reshape2
#' @importFrom plyr mapvalues
#' @author Mikko Vihtakari, Anette Wold
#' @export

# Test parameters
# data_file = "../../../Rijpfjorden Carbon Bridge/Data/Zooplankton/zoopl_rjipfj_2010_2013_ind_m3.xlsx"; sheet = "ALL_to_R"; dataStart = 11; dataEnd = 1000; dataCols = NULL; control_stations = FALSE; output_format = "as.Date"; add_coordinates = FALSE; control_species = list(species = "species", stage = "stage", size_op = "size_op", length = "length"); species_info_cols = NULL; lookup_cols = NULL; remove_missing = TRUE
# data_file = "Data/Kongsfjorden_zooplankton_allyears.xlsx"; sheet = "ALL ind m3"; dataStart = 11; dataEnd = 266; control_stations = TRUE; output_format = "as.Date"; add_coordinates = TRUE; dataCols = NULL; control_species = list(species = "species", stage = "stage", length = "length"); species_info_cols = c("group", "species", "stage", "length"); lookup_cols = NULL; remove_missing = TRUE
# data_file = "Data/Kongsfjord_zooplankton_2005.xlsx"; sheet = "Arkusz1"; dataStart = 11; dataEnd = 1000; control_stations = TRUE; output_format = "as.Date"; species_info_cols = c("species", "stage", "length"); lookup_cols = c("size_group", "origin", "biomass_conv"); add_coordinates = TRUE; control_species = list(species = "species", stage = "stage", length = "length"); dataCols = NULL


read_zooplankton_data <- function(data_file, sheet = 1, dataStart = NULL, dataEnd = 1000, dataCols = NULL, output_format = "as.Date", control_species = list(species = "species", stage = "stage", size_op = NULL, length = "length"), lookup_cols = "biomass_conv", species_info_cols = NULL, remove_missing = TRUE, control_stations = FALSE, add_coordinates = FALSE, control_sample_names = TRUE, round2ceiling = FALSE) {

## Switches ####

duplicate_sp <- FALSE

if(!is.null(dataCols)) stop("dataCols argument has not been implemented yet.")
if(!is.null(control_species) & any(names(control_species) != c("species", "stage", "size_op", "length"))) stop("control_species has to be either NULL or a named list with elements species, stage, size_op, length. See Usage for an example, Arguments and Details for explanation")


## Open the file ###
  
if(is.data.frame(data_file)) {
  stop("Other read methods than Excel have not been implemented yet")
  #dt <- data_file
  #file_ext <- NA
} else {
  file_ext <- get_file_ext(data_file)

  if(file_ext %in% c("xlsx", "xls")) {
    
    if(is.null(dataStart)) {
      tmp <- openxlsx::read.xlsx(data_file, sheet = sheet)[1]
      dataStart <- which(trimws(tmp[[1]]) == "Calanus finmarchicus")[1]
    }
    
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

colns <- guess_colname(cols = c("expedition", "station", "sample_name", "date", "from", "to", "unit", "comment"), df = tmp)
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
  
 new_names <- fix_station_names(levels(tmp$station))
  
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
levels(tmp$expedition) <- gsub("\\-", "", levels(tmp$expedition))

### Sample names

if(control_sample_names) {
  tmp$sample_name <- gsub("_", "-", as.character(tmp$sample_name))
}

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

####################
## Species list ####

if(is.null(species_info_cols)) species_info_cols <- unname(unlist(control_species))
  
sp <- dt[species_info_cols]

if(is.null(species_info_cols)) names(sp) <- names(unlist(control_species))

if(is.list(control_species)) {
## ###  
  sp$species <- gsub("\\b\\s\\s\\b", " ", sp$species, perl = TRUE)
  sp$species <- gsub("\\(cf.\\)", "", sp$species)
  sp$species <- gsub(" cf. ", " ", sp$species)
  sp$species <- gsub("\\(cf.\\s\\w+\\)", "", sp$species) # This removes any speculative species. Modify in data to counter.
  sp$species <- gsub(" Total", "", sp$species)
  sp$species <- gsub(" indet.", "", sp$species)
  sp$species <- trimws(sp$species)
  
  ## Typical typos
  sp$species[sp$species == "Triconia (=Oncaea) borealis"] <- "Triconia borealis"
  sp$species[sp$species == "Jashnovia brevis"] <- "Jaschnovia brevis"
  sp$species[sp$species == "Hyperiidea"] <- "Hyperiidae"
  sp$species[sp$species == "Isopoda Bopyridae"] <- "Bopyridae"
  
  sp[sp$species == "Calanoida nauplii", c("species", "stage")] <- c("Calanoida", "nauplii")
  sp[sp$species == "Nemertea pilidium", c("species", "stage")] <- c("Nemertea", "pilidium")
  
  ## Id to sort columns
  sp$idno <- 1:nrow(sp)
  
  ## Correct the old names to accepted ones 
  # k <- sp$species[157]
  sp$species <- sapply(sp$species, function(k) {
    if(k %in% ZOOPL$species) {
      k
    } else if(k %in% ZOOPL$old_names) {
      unique(ZOOPL[ZOOPL$old_names %in% k, "species"])
    } else {
      k
    }
  }, USE.NAMES = FALSE)
  
  ## Stage
  sp$stage <- trimws(sp$stage)
  sp$stage <- gsub("indet. ", "", sp$stage)
  sp$stage[sp$stage == "AF AM" & !is.na(sp$stage)] <- "AF/AM"
  sp$stage[sp$stage == "larvae furcilia" & !is.na(sp$stage)] <- "furcilia"
  sp$stage[sp$stage == "larvae calyptopis" & !is.na(sp$stage)] <- "calyptopis"
  sp$stage[sp$stage == "larvae trochophora" & !is.na(sp$stage)] <- "trochophora"
  sp$stage[sp$stage == "larvae metatrochophora" & !is.na(sp$stage)] <- "metatrochophora"
  sp$stage[sp$stage == "larvae mitraria" & !is.na(sp$stage)] <- "mitraria"
  sp$stage[sp$stage == "parasitic nauplii" & !is.na(sp$stage)] <- "nauplii"
  sp$stage <- trimws(sp$stage)
  
  ## Merge with ZOOPL

  if(any(names(unlist(control_species)) %in% "size_op")) {
    zoopl <- ZOOPL[c("species", "stage", "size_op", "length_old", "species_ID", lookup_cols)]
    sp <- merge(sp, zoopl, by.x = unname(unlist(control_species)), by.y = c("species", "stage", "size_op", "length_old"), all.x = TRUE, sort = FALSE)
  } else {
    zoopl <- ZOOPL[c("species", "stage", "length", "species_ID", lookup_cols)]
    sp <- merge(sp, zoopl, by.x = unname(unlist(control_species)), by.y = c("species", "stage", "length"), all.x = TRUE, sort = FALSE)
  }
  
  sp <- sp[order(sp$idno),]
  sp <- sp[!names(sp) %in% "idno"]

## ###  
  if(any(is.na(sp$species_ID))) {
    
    if(length(sp[is.na(sp$species_ID),"size_op"]) > 0) {
      stop(paste(sp[is.na(sp$species_ID), "species"], collapse = " "), " may have size_op the wrong way round")
    }
    
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

if(any(duplicated(sp$id))) {
  duplicate_sp <- TRUE
  dup_sps <- sp[duplicated(sp$id),"id"]
}


rownames(sp) <- 1:nrow(sp)

##############
#### Data ####

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

if(nrow(meta) != nrow(dat)) stop("Number of meta-data rows does not match with number of data rows. The error may be caused by the function not being able to define data columns correctly (usual problem: there are blank columns for species data with information for meta-data). Try using a numeric index in dataCols argument or deleting the blank columns.")

if(nrow(sp) != ncol(dat)) stop("Number of species list rows and data does not match. No idea why this happens. Possibly a typo in stage or size_op columns or then in ZOOPL. Try debugging the function section by section.")

colnames(dat) <- sp$id
rownames(dat) <- meta$id
dat[is.na(dat)] <- 0

if(round2ceiling) {
  dat <- as.data.frame(apply(dat, 2, ceiling))
} else {
  
  tmp <- sapply(1:nrow(dat), function(i) {
    x <- as.numeric(dat[i,])
    #all(abs(c(x%%1, x%%1-1)) < .Machine$double.eps^0.5)
    all(x %% 1 == 0)
  })
  
  if(any(tmp)) warning("Some of the samples appear to be rounded to closest integer (probably ceiling). Consider round2ceiling = TRUE to standardize the numbers. Alternatively fix the precision.")
  
}

################################
## Sum up duplicate species ####

if(duplicate_sp) {
 
  newdat <- dat
  newdat$id <- rownames(dat)
  
  newdat <- reshape2::melt(newdat, id = "id")
  newdat <- dcast(newdat, id ~ variable, sum)
  rownames(newdat) <- newdat$id
  newdat <- newdat[!names(newdat) %in% "id"]
  
  message(paste(ncol(dat) - ncol(newdat), "species entries summed up.", nrow(dat) - nrow(newdat), "rows lost."))
  
  dat <- newdat
  
  ## Remove the duplicates from the species list
  sp <- sp[!duplicated(sp$id),]
  rownames(sp) <- 1:nrow(sp)
}


###############################
### Remove missing species ####

if(remove_missing) {
  missing_sps <- names(dat)[colSums(dat, na.rm = TRUE) == 0]
  dat <- dat[!names(dat) %in% missing_sps]
  
  sp <- sp[!sp$id %in% missing_sps,]
  rownames(sp) <- 1:nrow(sp)
  
  message(length(missing_sps), " species entries have been removed from the dataset since their colSum was 0.")
}

if(duplicate_sp) message(paste(sp$id[sp$id %in% dup_sps], collapse = ", "), " were duplicated species entries. Abundances for these entries has been summed up")

#############################
#### Check species names ####

if(is.list(control_species)) {
  gen_sp_names <- sp[!sp$id %in% ZOOPL$species_ID, "id"]
  if(length(gen_sp_names > 0)) {
    message(paste(gen_sp_names, collapse = ", "), " were not found from ZOOPL.")
  }
}

############################
#### Compile and export ####

out <- list(data = dat, meta = meta, splist = sp)

class(out) <- "ZooplanktonData"

return(out)

}
#write.xlsx(dat, file = "control_del.xlsx", row.names = TRUE)


