#' @title Merge two ZooplanktonData objects
#' @description Merges two \link[=read_zooplankton_data]{ZooplanktonData} objects
#' @param x \link[=read_zooplankton_data]{ZooplanktonData} object
#' @param y \link[=read_zooplankton_data]{ZooplanktonData} object
#' @seealso \code{\link{read_zooplankton_data}}
#' @author Mikko Vihtakari
#' @export

merge_zooplankton_data <- function(x, y) {
  
  if(!all(c(class(x), class(y)) == "ZooplanktonData")) stop("x and y have to be ZooplanktonData objects. See ?read_zooplankton_data")
  
  ## Meta-data
  
  if(any(duplicated(c(x$meta$id, y$meta$id)))) stop("Meta-data IDs duplicted. Add a feature to fix this.")
  
  if(!all.equal(names(x$meta), names(y$meta))) message("Meta-data column names were not identical. Columns added.")
  
  meta <- rbind.fill(x$meta, y$meta)
  
  ## Species list
  
  sp <- merge(x$splist, y$splist, all = TRUE)
  
    if(nrow(sp) != max(c(nrow(x$splist), nrow(y$splist)))) message(paste(nrow(sp) - max(c(nrow(x$splist), nrow(y$splist)))), " rows added to splist")
  
  if(ncol(sp) != max(c(ncol(x$splist), ncol(y$splist)))) message(paste(ncol(sp) - max(c(ncol(x$splist), ncol(y$splist)))), " columns added to splist")
  
  ## Data
  
  dat <- rbind.fill(x$data, y$data)
  dat[is.na(dat)] <- 0
    
  if(nrow(meta) != nrow(dat)) stop("Number of meta-data rows does not match with number of data rows. The error may be caused by the function not being able to define data columns correctly. Try using a numeric index in dataCols argument")

  if(nrow(sp) != ncol(dat)) stop("Number of species list rows and data does not match. No idea why this happens. Try debugging the function section by section.")

  ## Compile 
  
  out <- list(data = dat, meta = meta, splist = sp)
  class(out) <- "ZooplanktonData"
  
  return(out)

}
