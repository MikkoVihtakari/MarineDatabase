#' @title Summarize ZooplanktonData species abundances based on information from meta-data or species list
#' @description Averages \link[=read_zooplankton_data]{ZooplanktonData} objects by grouping varaibles from meta-data (\code{$meta}) or species list (\code{$splist}). Can be used to convert abundances to biomasses, to sum up species abundances to remove stages and to summarize species data over the entire water column or regions. 
#' @param obj \link[=read_zooplankton_data]{ZooplanktonData} object
#' @param biomass logical indicating whether abundances should be converted to biomass. Requires the \code{biomass_conv} column in \code{obj$splist}. 
#' @param sp_group character indicating the new names of species matrix names. \strong{Sums up} species names based on the new name. Can be used to collate species to remove stages ("species") or to collate species based on origin ("origin"; Atlantic vs Arctic). Use \code{NULL} if you do not want to change species matrix names.
#' @param meta_group character vector of column names in \code{$meta}, which should be used to summarize the species data. Typically use c("expedition", "station", "lon", "lat", "lon.utm", "lat.utm", "date") for multinet data to summarize over the entire water column. Use \code{NULL} if you do not want to summarize species data based on meta data. 
#' @param meta_group_method character giving the method which should be used to summarize species data. Options: 
#' \itemize{
#'   \item \strong{"mean"} takes a simple mean of based on values listed in \code{meta_group} argument.
#'   \item \strong{"depth_mean"} takes a depth averaged abundance/biomass using the values listed in \code{meta_group} argument. Requires \code{from} and \code{to} columns in \code{$meta}. The values for each species are multiplied by depth interval, summed up by \code{meta_group} and divided by summed depth interval. The unit is the original unit (n/m3 or weight/m3). 
#'   \item \strong{"depth_sum"} same than above, but values are not divided by summed depth interval. Represents depth "integrated" values. The unit is n/m2 if the original unit was n/m3.
#'   \item \strong{"sum"} sums up ("integrates") abundances/biomasses for each species by \code{meta_group}
#'   \item \strong{"total_sum"} ignores species and sums up all abundance/biomass by \code{meta_group}. 
#' }
#' @param remove_noncont logical indicating whether non-continuous casts should be removed if \code{meta_group_method} = "depth_mean". 
#' @param warnings logical indicating whether the function should print warnings if it detects problems with data
#' @return Returns a list of class \code{ZooplanktonData}. The list contains 3 data frames: \code{$data} (abundance data), \code{$meta} (meta-data), and \code{$splist} (species information).
#' @import dplyr
#' @export
#' 
#' 

## Test parameters
# obj = dt; biomass = FALSE; sp_group = "origin"; meta_group = c("expedition" ,"station", "lon", "lat", "lon.utm", "lat.utm", "date"); meta_group_method = "depth_mean"; remove_noncont = FALSE; warnings = TRUE
# obj = dt; sp_group = "species"; meta_group = NULL; meta_group_method = "depth_mean"; biomass = FALSE; remove_noncont = FALSE; warnings = TRUE
# obj = tp; sp_group = NULL; meta_group = c("expedition" , "station", "area", "bottom_depth", "lon", "lat", "lon.utm", "lat.utm", "date");  meta_group_method = "total_sum"; biomass = TRUE; remove_noncont = TRUE; warnings = TRUE
# obj = dt; biomass = TRUE; sp_group = "species"; meta_group = NULL; meta_group_method = "none"; remove_noncont = FALSE; warnings = TRUE
# obj  = tmp; biomass = FALSE; sp_group = "group"; meta_group = c("expedition", "station", "area", "season", "bottom_depth", "lon", "lat", "lon.utm", "lat.utm", "date"); meta_group_method = "depth_mean"; remove_noncont = TRUE; warnings = TRUE
# obj = dt; sp_group = "species"; meta_group = "id"; biomass = FALSE; remove_noncont = TRUE; warnings = TRUE; meta_group_method = "depth_mean"
summarize_zooplankton_data <- function(obj, sp_group = NULL, meta_group = NULL, meta_group_method = "depth_mean", biomass = FALSE, remove_noncont = FALSE, warnings = TRUE) {

  ## Tests ####
  
  if(class(obj) != "ZooplanktonData") stop("The function requires a ZooplanktonData object")
  if(nrow(obj$data) != nrow(obj$meta)) stop("Invalid ZooplanktonData object: Number of rows in $meta and $data differ.")
  if(any(rownames(obj$data) != obj$meta$id)) stop("Invalid ZooplanktonData object: rownames of $data and the id column in $meta do not match.")
  
  if(!is.null(meta_group)) {
    if(meta_group == "id") {
      if(!any(duplicated(obj$meta$id))) {
        stop("Using 'id' as meta_group: All ids in $meta are unique. There is no need to summarize using 'id'. Use meta_group = NULL instead.")
      } else {
        stop("$meta entires are not unique (they should be). Something is wrong with the ZooplanktonData object.")
      }
    }  
  }
  
  
  ## Objects 
  
  dat <- obj$data
  dat$id <- rownames(dat)
  dat <- reshape2::melt(dat, id = "id")
  names(dat)[names(dat) == "variable"] <- "sp_id"
  
  meta <- obj$meta
  sp <- obj$splist
  
  ## Convert to biomass

  if(biomass) {
 
    if(is.null(sp$biomass_conv)) stop("Biomass conversion requires biomass_conv column in $splist. Run read_zooplankton_data again with lookup_cols = 'biomass_conv'")
   
    dat <- merge(dat, sp[c("id", "biomass_conv")], by.x = "sp_id", by.y = "id", all.x = TRUE, sort = FALSE)
    dat$value <- dat$value * dat$biomass_conv
    dat <- dat[c("id", "sp_id", "value")]
  }
  
  ## Collate species
  
  if(!is.null(sp_group)) {
    
    if(!sp_group %in% names(sp)) stop("sp_group must be one of the column names in $splist")
    
    dat <- merge(dat, sp[c("id", sp_group)], by.x = "sp_id", by.y = "id", all.x = TRUE, sort = FALSE)
    
    if(any(make.names(dat[[sp_group]]) != dat[[sp_group]])) {
      
      dat$sp_abbr <- make_spabbr(as.character(dat[[sp_group]]))
      dat[[sp_group]] <- factor(dat[[sp_group]], levels = unique(dat[[sp_group]]))
      dat$sp_abbr <- factor(dat$sp_abbr, levels = unique(dat$sp_abbr))
      dat$sp_id <- dat$sp_abbr
    
      tmp <- unique(dat[c(sp_group, "sp_abbr")])
      rownames(tmp) <- 1:nrow(tmp)
    
      if(any(names(sp) %in% c("origin")) & sp_group == "species") {
        tmp <- merge(tmp, unique(sp[c(sp_group, "origin")]), by = sp_group, all.x = TRUE)
      }
      
      if(any(duplicated(tmp$sp_abbr))) stop("duplicated names in species list as a result of grouping.")
      
      names(tmp)[names(tmp) == "sp_abbr"] <- "id"
      
      sp <- tmp
      
    } else {
      
      dat[[sp_group]] <- factor(dat[[sp_group]], levels = unique(dat[[sp_group]]))
      dat$sp_id <- dat[[sp_group]]
      
      sp <- data.frame(id = unique(dat[[sp_group]]))
      
    }
    
    dat <- dat[c("id", "sp_id", "value")]
    
    dat <- dat %>% group_by(id, sp_id) %>% summarise(value = sum(value, na.rm = TRUE))  
  }
  
  ## Summarize species data ###
  
  if(!is.null(meta_group)) {
    
    if(any(!meta_group %in% names(meta))) stop("All meta_group match the column names in $meta")
  
    if(meta_group_method == "mean") {
      dat <- merge(meta[c("id", meta_group)], dat, by = "id", all = TRUE, sort = FALSE)
      dat <- dat %>% group_by_(.dots = c(meta_group, "sp_id")) %>% summarise(value = mean(value, na.rm = TRUE))
    } else if(meta_group_method == "sum") {
      dat <- merge(meta[c("id", meta_group)], dat, by = "id", all = TRUE, sort = FALSE)
      dat <- dat %>% group_by_(.dots = c(meta_group, "sp_id")) %>% summarise(value = sum(value, na.rm = TRUE))
    } else if(meta_group_method == "total_sum") {
      dat <- merge(meta[c("id", meta_group)], dat, by = "id", all = TRUE, sort = FALSE)
      dat <- dat %>% dplyr::group_by_(.dots = meta_group) %>% dplyr::summarise(value = sum(value, na.rm = TRUE))
    } else if(meta_group_method %in% c("depth_mean", "depth_sum")) {
      
      if(any(meta_group %in% c("from", "to"))) stop("'from' and 'to' should not be included in meta_group for the 'depth_mean'|'depth_sum' method as they are used to calculate depth averaged/summed values")
      
      TMP <- meta[c("id", meta_group, "from", "to")]
      if(any(names(TMP) %in% "id.1")) {
        TMP <- TMP[!names(TMP) %in% "id.1"]
      }
      
      dat <- base::merge(TMP, dat, by = "id", all = TRUE, sort = FALSE)
      dat$diff <- dat$from - dat$to
      
    if(remove_noncont | warnings) {
        
      tmp <- dat[c("id", meta_group, "from", "to", "diff")]
      tmp <- unique(tmp)
      
      tmp <- tmp %>% group_by_(.dots = c(meta_group, "to")) %>% arrange(.by_group = TRUE)
      
      tmp <- tmp %>% group_by_(.dots = meta_group) %>% mutate(gap = from - lead(to))
      
      noncont <- tmp[!is.na(tmp$gap) & tmp$gap != 0,]$id
      
      if(remove_noncont) {
        
      dat$tmp_id <- apply(dat[,meta_group], 1, paste, collapse = "-")
      tmp$tmp_id <- apply(tmp[,meta_group], 1, paste, collapse = "-")
      
      drop_ids <- tmp[tmp$id %in% noncont, ]$tmp_id
      
      dat <- dat[!dat$tmp_id %in% drop_ids,]
      dat <- dat[!names(dat) %in% "tmp_id"]
      
        } else {
      warning("IDs ", paste(noncont, collapse=", "), " contain gaps.")  
      }
    }
      
      if(meta_group_method ==  "depth_mean") {
        dat <- dat %>% group_by_(.dots = c(meta_group, "sp_id")) %>% summarise(value = sum(value * diff, na.rm = TRUE)/sum(diff))
      } else {
        dat <- dat %>% group_by_(.dots = c(meta_group, "sp_id")) %>% summarise(value = sum(value * diff, na.rm = TRUE))  
        }
      
    } else {
      stop(meta_group_method, " is not a valid meta_group_method")
    }
    
    
    if(meta_group_method == "total_sum") {
      meta <- as.data.frame(dat[!names(dat) %in% "value"])
      dat <- as.data.frame(dat["value"])
      meta$id <- rownames(dat)
      sp <- data.frame(value = "total sum")
    } else {
      dat <- reshape2::dcast(dat, ... ~ sp_id, value.var = "value") 
      meta <- dat[meta_group] 
      meta$id <- rownames(dat)
      dat <- dat[!names(dat) %in% meta_group]
    }
    
  ## If is.null(meta_group):  
  } else {
    
    
  }
  

## Final manipulation ####
  
if(all(c("id", "sp_id", "value") %in% names(dat))) {
  dat <- reshape2::dcast(dat, ... ~ sp_id, value.var = "value") 
  rownames(dat) <- dat$id
  dat <- dat[!names(dat) %in% "id"]
}

## Make sure that the order of meta and dat matches:
if(any(meta$id != rownames(dat))) dat <- dat[match(meta$id, rownames(dat)),]

  if(any(is.na(dat))) dat[is.na(dat)] <- 0
  if(nrow(meta) != nrow(dat)) stop("nrow between data and meta differ.")
  if(ncol(dat) != nrow(sp)) stop("ncol for data and nrow for sp differ.")
  if(any(rownames(dat) != meta$id)) stop("data rownames and meta data ids do not match")
  
  out <- list(data = dat, meta = meta, splist = sp)
  class(out) <- "ZooplanktonData"

  return(out)

}
