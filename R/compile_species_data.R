#' @title Compile species data from ice cores
#' @description Compiles species data across a larger dataset with duplicate species names as defined by a species list from \code{\link{confirm_species_list}} function. 
#' @param dat data frame containing ice core species data to be combined. Must be in long format.
#' @param splist species list. Preferably from \code{\link{confirm_species_list}}.
#' @param core_cols A character vector specifying columns that separate individual ice-cores.
#' @param sp_col Character specifying the name of the species column in \code{dat}. 
#' @param ab_col Character specifying the name of the abundance column in \code{dat}. 
#' @param unit_col Character specifying the name of the unit for the abundance column in \code{dat}. 
#' @param start_col Character argument specifying the name of the column in \code{dat} for the section start for the ice core in centimeters or meters.
#' @param end_col Character argument specifying the name of the column in \code{dat} for the section end for the ice core in centimeters or meters.
#' @param ice_thick_col Character argument specifying the name of the column in \code{dat} for ice thickness preferably in \strong{centimeters}
#' @param add_cols A character vector containing names of the additional columns that should be included in the output. These columns are not used by the function otherwise. Additional columns cannot contain several unique values due to summarizing process.
#' @param convert_unit List or \code{NULL}. Should the function convert supplied abundances using the \code{\link{convert_abundace}} function, a list giving \code{from} and \code{to} units have to be supplied. If conversion should not be made, use \code{NULL} (default). See details for alternatives.
#' @param summarise_cores Logical. Should core sections be summarized to a continuous core? If \code{TRUE} (default), all sections are summarized, \code{start_col} and \code{end_col} removed from the output, and a \code{$core_type} column added. If \code{FALSE} core sections are returned as they are, but duplicate species names are summed up.
#' @param return_summary Logical. Should summary statistics be returned?
#' @param round_digits Number of digits \code{start_col}, \code{end_col} and \code{ice_thick_col} values should be rounded to. Set to \code{NULL} to avoid rounding. See \code{\link[base]{round}} for details.
#' @details Built for ice-algae taxonomy data, but could be modified to work with any taxonomy data including plankton nets. The function is currently broken for wider use.
#' 
#' Implemented \strong{convert_unit} argument alternatives:
#' \itemize{
#'   \item \strong{\code{NULL}}: no conversion. Adundances summed up as they are (see \code{summarise_cores} argument).
#'   \item \strong{\code{list(from = "per", to = "per")}}: percentage abundances are summed up according to the \code{summarise_cores} argument and scaled to 100 \% (sum for an ice core or a section adds up to 100 \%).
#'   \item \strong{\code{list(from = "rel", to = "rel")}}: relative abundances are summed up by taking a mean for duplicate species or entire ice cores (if \code{summarise_cores = TRUE}).
#'   \item \strong{\code{list(from = "rel", to = "per")}}: relative abundances are first summed up and a percentage of their contribution is calculated. Abundances add up to 100 \% for an ice core or a section depending on the \code{summarise_cores} argument.
#'   \item \strong{\code{list(from = "1/L", to = "1/m2")}} (or any other variations passed to \code{\link{convert_abundace}}): Individuals per litre values converted to individuals per square metre before summing up abundances (see \code{summarise_cores} argument)
#'   }
#' 
#' @return Returns a data frame with columns specified in arguments. Drops any unspecified column.
#' @importFrom plyr mapvalues
#' @importFrom lazyeval interp
#' @import dplyr
#' @author Mikko Vihtakari
#' @encoding UTF-8
#' @export
# dat <- y[[2]]

# sp_col = "species"; core_cols = c("expedition", "station", "core.id"); unit_col = "unit"; convert_unit = list(from = "rel", to = "rel"); start_col = "from"; end_col = "to"; ice_thick_col = "ice"; ab_col = "abundance"; summarise_cores = TRUE; add_cols = c("gear", "longitude", "latitude", "date", "bottom_depth", "ice.type", "snow"); return_summary = TRUE; round_digits = 0

compile_species_data <- function(dat, splist, core_cols = c("expedition", "station", "core.id"), sp_col = "species", ab_col = "abundance", unit_col = "unit", start_col = "from", end_col = "to", ice_thick_col = "ice", add_cols = c("gear", "longitude", "latitude", "date", "bottom_depth", "ice.type", "snow"),  convert_unit = list(from = "1/L", to = "1/m2"), summarise_cores = TRUE, return_summary = TRUE, round_digits = 0) {

### Tests ####

req_splist_cols <- c("or_name", "use_name", "drop", "type", "certainty")

if(length(unique(dat[unit_col])) != 1) stop("Only one unit type is allowed. Split ", unit_col, " column")
if(!all(req_splist_cols %in% colnames(splist))) stop("splist (species list) must contain ", paste(req_splist_cols[!req_splist_cols %in% colnames(splist)], collapse = ", "), " columns. Easiest way to get these is to use confirm_species_list function.")

## Split to unique ice cores

dat$temp <- apply(dat, 1, function(k) paste(k[core_cols], collapse = "_"))

dat <- dat[c("temp", core_cols, start_col, end_col, ice_thick_col, sp_col, unit_col, ab_col, add_cols)]

x <- split(dat, dat$temp, drop = TRUE)

## Loop ####

# k <- x[["N-ICE15_Main Coring Site_8"]]
# k <- x[[2]]
out <- lapply(x, function(k) {

## Definitions for IDs
id <- unique(k$temp)

 #print(id)  

k_add <- unique(k[c(core_cols, add_cols)])
if(nrow(k_add) > 1) stop(">= 1 unique add_col entries for ", id, ". Not allowed")

k <- k[!names(k) %in% c(add_cols, "temp")]

k <- droplevels(k)

## Definitions for ice cores

### Test whether the ice core contains missing pieces

if(!is.null(round_digits)) {
if(!is.numeric(round_digits)) stop("round argument must be an integer")
  k[[start_col]] <- base::round(k[[start_col]], round_digits)
  k[[end_col]] <- base::round(k[[end_col]], round_digits)
  k[[ice_thick_col]] <- base::round(k[[ice_thick_col]], round_digits)
  
  start <- min(k[start_col])
  end <- max(k[end_col])
  icethick <- unique(k[[ice_thick_col]])
  
} else {
  start <- min(k[start_col])
  end <- max(k[end_col])
  icethick <- unique(k[[ice_thick_col]])
}


sections <- unique(k[c(start_col, end_col)])
cont_sec <- interleave(sections[[1]], sections[[2]])
cont_sec <- all(duplicated(cont_sec[c(-1, -length(cont_sec))]) == rep(c(FALSE, TRUE), (length(cont_sec)-2)/2))

### Parameters


if(summarise_cores) {
bottom.code <- bottom_sec(start, end, icethick, cont_sec)  
}

## Fix instances with empty cores

z <- split(k, k[[start_col]])

z <- lapply(z, function(j) {
  if(nrow(j) == 1) {
    if(is.na(j[[sp_col]]) & is.na(j[[ab_col]])) {
    j[[sp_col]] <- "Empty"
    j[[ab_col]] <- 0
    j
    } else {
      j
    }} else{
    j
  }
})

k <- do.call(rbind, z)

## Remove high abundances of symbionts (M. rubrum)

symbs <- splist[splist$type %in% "symbiont" & splist$use_name %in% "Mesodinium rubrum", "or_name"]

if(any(levels(k[[sp_col]]) %in% symbs)) {
  k[k[[sp_col]] %in% symbs, ab_col] <- 1
}

## Rename species
k_og <- k
k[[sp_col]] <- factor(plyr::mapvalues(as.character(k[[sp_col]]), splist$or_name, splist$use_name, warn_missing = FALSE))

k <- droplevels(k)

## Convert units

if(!is.null(convert_unit) & ((convert_unit$from != "per" & convert_unit$to != "per") & (convert_unit$from != "rel" & convert_unit$to != "rel"))) {
  k <- suppressMessages(convert_abundance(data = k, ab_col = ab_col, ab_from = convert_unit$from, ab_to = convert_unit$to, filtered = TRUE))
}

######### MEGA MIND-FUCK #################
## Sum up abundances for duplicate species

if(convert_unit$from == "per" & convert_unit$to == "per") {
  
  if(summarise_cores) {
    k <- k %>% dplyr::group_by_(.dots = names(k)[!names(k) %in% c(ab_col, start_col, end_col)]) %>% dplyr::summarise_(abundance = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(ab_col)))
    k <- data.frame(k)
    if(ab_col != "abundance") {
      names(k)[names(k) == "abundance"] <- ab_col
    }
    k[ab_col] <- 100*k[ab_col]/sum(k[[ab_col]])
    k$core_type <- bottom.code
    
    if(nrow(k) > 1) k <- k[!k[[sp_col]] %in% "Empty",]
    
  } else {
    k <- k %>% dplyr::group_by_(.dots = names(k)[!names(k) %in% c(ab_col)]) %>% dplyr::summarise_(abundance = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(ab_col)))
    k <- data.frame(k)
    if(ab_col != "abundance") {
      names(k)[names(k) == "abundance"] <- ab_col
    }
    k[ab_col] <- 100*k[ab_col]/sum(k[[ab_col]])
}
  
if(round(sum(k[[ab_col]]), 0) != 100) stop("Abundace sum for ", id, " is not 100%")
    
} else if(convert_unit$from == "rel") {
  
  if(summarise_cores) {
     k <- k %>% dplyr::group_by_(.dots = names(k)[!names(k) %in% c(ab_col, start_col, end_col)]) %>% dplyr::summarise_(abundance = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(ab_col)), occurences = lazyeval::interp(~length(var), var = as.name(sp_col)))
  } else {
    k <- k %>% dplyr::group_by_(.dots = names(k)[!names(k) %in% c(ab_col)]) %>% dplyr::summarise_(abundance = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(ab_col)), occurences = lazyeval::interp(~length(var), var = as.name(sp_col)))
  }
 
  k <- data.frame(k)
  if(nrow(k) > 1 & summarise_cores) k <- k[!k[[sp_col]] %in% "Empty",]
  
  if(convert_unit$to == "rel") {
  temp_ab <- ifelse(k$occurences <= 1, k$abundance, ifelse(k$abundance == 0, 0, round(k$abundance/k$occurences, 0)))  
  k[unit_col] <- "rel"
  } else if(convert_unit$to == "per") {
  temp_ab <- ifelse(k$abundance == 1, 0.1, 100*k$abundance/sum(k$abundance))
  temp_ab <- 100*temp_ab/sum(temp_ab)
  k[unit_col] <- "per"
  } else {
    stop("Abundace conversion for ", id, " not implemented")
  }
  
  if(ab_col != "abundance") {
      names(k)[names(k) == "abundance"] <- ab_col
  }
  
  k[ab_col] <- temp_ab
  
  k <- k[!names(k) %in% "occurences"]
  
  if(summarise_cores) {
     if(nrow(k) > 1) k <- k[!k[[sp_col]] %in% "Empty",]
     k$core_type <- bottom.code
  }
  
} else {
   if(summarise_cores) {
    k <- k %>% dplyr::group_by_(.dots = names(k)[!names(k) %in% c(ab_col, start_col, end_col)]) %>% dplyr::summarise_(abundance = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(ab_col)))
    k <- data.frame(k)
    if(ab_col != "abundance") {
      names(k)[names(k) == "abundance"] <- ab_col
    }
    
    ## Remove empty entries
    if(nrow(k) > 1) k <- k[!k[[sp_col]] %in% "Empty",]
    
    k$core_type <- bottom.code

  } else {
    k <- k %>% dplyr::group_by_(.dots = names(k)[!names(k) %in% c(ab_col)]) %>% dplyr::summarise_(abundance = lazyeval::interp(~sum(var, na.rm = TRUE), var = as.name(ab_col)))
    k <- data.frame(k)
    
    ## Remove empty entries
    #(not implemented)
    
    if(ab_col != "abundance") {
      names(k)[names(k) == "abundance"] <- ab_col
    }
  }
}
  
### Return

if(summarise_cores) {
  k <- merge(k, k_add, by = core_cols, all = TRUE, sort = FALSE)
  k <- k[c(core_cols, add_cols, ice_thick_col, "core_type", sp_col, unit_col, ab_col)]
  list(data = k, sumInfo = sum_info(ID = id, dt_st = k_og, dt_ed = k, sum_cor = summarise_cores, secs = sections))
} else {
  k <- merge(k, k_add, by = core_cols, all = TRUE, sort = FALSE)
  k <- k[c(core_cols, add_cols, ice_thick_col, start_col, end_col, sp_col, unit_col, ab_col)]
  list(data = k, sumInfo = sum_info(ID = id, dt_st = k_og, dt_ed = k, sum_cor = summarise_cores, secs = sections))
}


})
### End loop ###
### Return

out_dat <- do.call(rbind, lapply(out, function(k) k$data))
rownames(out_dat) <- 1:nrow(out_dat)

if(return_summary) {
  list(data = out_dat, sumInfo = lapply(out, function(k) k$sumInfo))
} else {
  out_dat
}

}
#### Function end ####

### Summary info function ####
sum_info <- function(ID = id, dt_st = k_og, dt_ed = k, sum_cor = summarise_cores, secs = sections) {
  paste0(ID, ": summarized from ", length(dt_st$species), " species to ", length(dt_ed$species), 
  ". ", length(unique(dt_st$species)), " unique species at the beginning and ", length(unique(dt_st$species)), " at the end. ",
  if(sum_cor) {
  paste0("Cores summed. Core type: ", unique(dt_ed$core_type), ".")
  } else {
  paste("Cores kept separate. Sections:", paste(secs$from, secs$to, sep = "-", collapse = ", "))     
    } 
  )
}

### Define core type ####
bottom_sec <- function(start, end, icethick, cont_sec) {
  ifelse(all(start == 0, is.na(icethick)),
    paste("bottom", end, "cm"),
  ifelse(all(start != 0, is.na(icethick)),
    "bottom sec missing",
  ifelse(end > icethick,
    "wrong ice thinkness",
  ifelse(all(start == 0, end == icethick, cont_sec), 
    "whole core",
  ifelse(all(start == 0, end == icethick, !cont_sec),
    "whole core, gap",
  ifelse(start == 0 & cont_sec,
    paste("bottom", end, "cm"),
  ifelse(start == 0 & !cont_sec,  
    paste("bottom", end, "cm, gap"),
  ifelse(start != 0,
    "bottom sec missing",
    "ERROR"
  ))))))))
}

### Scrap functions
# sum_rel <- function(occ, abb) {
#   ifelse(occ == abb, 1, abb/occ)
# }
# convert_unit_names <- function(x) {
#   switch(x,
#     `1/L` = c("L", "cells/L"),
#     stop("unit has not been implemented"))
# }
