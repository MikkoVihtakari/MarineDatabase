#' @title Merge two ZooplanktonData objects
#' @description Merges two \link[=read_zooplankton_data]{ZooplanktonData} objects
#' @param x \link[=read_zooplankton_data]{ZooplanktonData} object
#' @param y \link[=read_zooplankton_data]{ZooplanktonData} object
#' @seealso \code{\link{read_zooplankton_data}}
#' @family ZooplanktonData
#' @author Mikko Vihtakari
#' @export

# x = dt1; y = dt2011
merge_zooplankton_data <- function(x, y) {
  
  if(!all(c(class(x), class(y)) == "ZooplanktonData")) stop("x and y have to be ZooplanktonData objects. See ?read_zooplankton_data")
  
  ## Meta-data ####
  
  if(any(duplicated(c(x$meta$id, y$meta$id)))) stop("Meta-data IDs duplicted. Add a feature to fix this.")
  
  if(!all.equal(names(x$meta), names(y$meta))) message("Meta-data column names were not identical. Columns added.")
  
  meta <- rbind.fill(x$meta, y$meta)
  
  meta <- meta[order(meta$id),]
  rownames(meta) <- 1:nrow(meta)
  
  ## Species list ####
  
  xsp <- x$splist[!names(x$splist) %in% c("size_op", "length")]
  ysp <- y$splist[!names(y$splist) %in% c("size_op", "length")]
  
  sp <- merge(xsp, ysp, all = TRUE, sort = TRUE)
  sp <- sp[order(sp$id),]
  
  tmp <- ZOOPL[c("species_ID", "length")]
  names(tmp)[names(tmp) == "species_ID"] <- "id"
  
  sp <- merge(sp, tmp, by = "id", all.x = TRUE, sort = FALSE)
  
  rownames(sp) <- 1:nrow(sp)
  
  # test_equality(sp, xsp)
  
    if(nrow(sp) != max(c(nrow(x$splist), nrow(y$splist)))) message(paste(nrow(sp) - max(c(nrow(x$splist), nrow(y$splist)))), " rows added to splist")
  
  if(ncol(sp) != max(c(ncol(x$splist), ncol(y$splist)))) message(paste(ncol(sp) - max(c(ncol(x$splist), ncol(y$splist)))), " columns added to splist")
  
  ## Data ####
  
  xdat <- x$data
  ydat <- y$data
  
  xdat$id <- rownames(xdat)
  ydat$id <- rownames(ydat)
  
  dat <- rbind.fill(xdat, ydat)
  dat[is.na(dat)] <- 0
  
  rownames(dat) <- dat$id
  
  if(nrow(meta) != nrow(dat)) stop("Number of meta-data rows does not match with number of data rows. The error may be caused by the function not being able to define data columns correctly. Try using a numeric index in dataCols argument")
  
  dat <- dat[order(dat$id),]
  
  if(!all.equal(dat$id, meta$id)) stop("Meta-data ids and row names of data are not equal. Something went wrong.")
  
  dat <- dat[names(dat) != "id"]
  
  
  if(nrow(sp) != ncol(dat)) stop("Number of species list rows and data does not match most likely because of differing values in splists for x and y.")

  ## Compile ####
  
  out <- list(data = dat, meta = meta, splist = sp)
  class(out) <- "ZooplanktonData"
  
  return(out)

}
