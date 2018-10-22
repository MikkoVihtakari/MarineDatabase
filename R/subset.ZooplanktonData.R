#' @title Subset a ZooplanktonData object
#' @description Generic subset method for ZooplanktonData objects. Returns a subset of a Zooplankton object based on meta-data or species information defined by the \code{type} argument.
#' @param x a \link[=read_zooplankton_data]{ZooplanktonData} object.
#' @param type Character defining whether meta-data ("meta") or species list ("splist") should be used in subsetting.
#' @param ... Arguments passed to the \code{\link[base]{subset}} function.
#' @param remove_zeros Logical indicating whether columns/species (if \code{type = "meta"}) or rows/samples (if \code{type = "splist"}) with zero sums should be removed from the dataset.
#' @param drop_levels Logical indicating whether unused levels should be dropped from \code{$meta} and \code{$splist} data frames. See \code{\link[base]{droplevels}}.
#' @return Returns a subsetted \link[=read_zooplankton_data]{ZooplanktonData} object.
#' @details See the the \code{\link[base]{subset}} function for details.
#' @method subset ZooplanktonData
#' @family ZooplanktonData
#' @author Mikko Vihtakari
#' @export

## Examples
# subset(dt, type = "meta", season == "winter") # Select samples collected during winter
# subset(dt, type = "meta", season == "winter", remove_zeros = TRUE) # Remove all species which did not occur during winter in addition to subsetting
# subset(dt, type = "meta", year %in% 2010:2012) # Select samples collected in 2010, 2011 or 2012
#
# subset(dt, type = "splist", species == "Calanus glacialis", remove_zeros = TRUE) # Select only C. glacialis. Remove all samples which do not contain them
# 
# Subset a range of species entries using a logical vector
# specs <- grep("Pseudo", dt$splis$species, value = TRUE)
# subset(dt, type = "splist", species %in% specs)

subset.ZooplanktonData <- function(x, type, ..., remove_zeros = FALSE, drop_levels = TRUE) { 
  
  if(missing(type)) stop("type argument has to be defined. Use type = 'meta' if you want to subset using meta-data (samples) and type = 'splist' if you want to subset by species.")
  
  ## Meta data subset
  if(type == "meta") {
    
    new.meta <- base::subset.data.frame(x$meta, ...)
    removed.ids <- x$meta[!x$meta$id %in% new.meta$id, "id"]
    new.data <- x$data[!rownames(x$data) %in% removed.ids,]

    if(remove_zeros) {

    removed.sps <- sort(names(new.data)[colSums(new.data) == 0])
    new.data <- new.data[!names(new.data) %in% removed.sps]
    new.sp <- x$splist[!x$splist$id %in% removed.sps,]

    message(length(removed.sps), " species entries have been removed from the dataset since their colSum was 0.")

    } else {
    
     new.sp <- x$splist 
    
     }
  
  ## Species data subset    
  } else if(type == "splist") {
    
    new.sp <- base::subset.data.frame(x$splist, ...)
    removed.sps <- x$splist[!x$splist %in% new.sp$id, "id"]
    new.data <- x$data[names(x$data) %in% new.sp$id]
    
    if(remove_zeros) {
      
    removed.ids <- rownames(new.data)[rowSums(new.data) == 0]
    new.data <- subset(new.data, !rownames(new.data) %in% removed.ids)
    new.meta <- x$meta[!x$meta$id %in% removed.ids,]
    
    message(length(removed.ids), " samples have been removed from the dataset since their rowSum was 0.")
      
    } else {
      
      new.meta <- x$meta
    
    }
    
  ## Not accepted type arguments
    } else {
      stop("type ", type, " not defined.")
  }
  
  ## Final tests
  
  #if(any(new.meta$id != rownames(new.data))) new.data <- new.data[match(new.meta$id, rownames(new.data)),]
    #if(any(is.na(dat))) dat[is.na(dat)] <- 0
    if(nrow(new.meta) != nrow(new.data)) stop("nrow between new data and meta differ.")
    if(ncol(new.data) != nrow(new.sp)) stop("ncol for data and nrow for sp differ.")
    if(!all(new.meta$id == rownames(new.data))) stop("New meta and data ids do not match.")
  
  ## Drop levels
  
    if(drop_levels) {
      new.data <- droplevels(new.data)
      new.meta <- droplevels(new.meta)
      new.sp <- droplevels(new.sp)
    }
  
  ## Output
  
    out <- list(data = new.data, meta = new.meta, splist = new.sp)
    class(out) <- "ZooplanktonData"

    return(out)

}