#' @title Define water type from temparature and salinity for Kongsfjorden
#' @description Defines water type from temperature and salinity data for Kongsfjorden
#' @param dt Dataframe containing temperature and salinity information
#' @param temp_col Character string giving the column name containing temperature data
#' @param sal_col Character string giving the column name containing salinity data
#' @param bind Should the water type information be \code{\link{cbind}} with \code{dt} (\code{TRUE}) or should a character vector containing water type abbreviations returned instead (\code{FALSE}; default)
#' @param return_water_type_list Should dataframe containing water type definitions be returned instead of defining water types (\code{TRUE}; default is \code{FALSE})? 
#' @details Water types are returned after the definition in Cottier et al. (2005) with slight modifications to avoid overlaps of water types. Use \code{return_water_type_list = TRUE} to see the boundary values for water masses. \code{y} means temperature and \code{x} salinity. These values are used in associated TS diagrams. 
#' @references Cottier, F., Tverberg, V., Inall, M., Svendsen, H., Nilsen, F., Griffiths, C., 2005. Water mass modification in an Arctic fjord through cross-shelf exchange: The seasonal hydrography of Kongsfjorden, Svalbard. J. Geophys. Res. 110, C12005. doi:10.1029/2004JC002757
#' @author Mikko Vihtakari
#' @export

# dt <- ctd; temp_col <- "temp"; sal_col <- "sal"
define_water_type <- function(dt, temp_col = "temp", sal_col = "sal", bind = FALSE, return_water_type_list = FALSE) {

waters <- c("Atlantic Water", "Arctic Water", "Winter Cooled Water", "Local Water", "Surface Water", "Transformed Atlantic Water", "Intermediate Water")
abbrs <- c("AW", "ArW", "WCW", "LW", "SW", "TAW", "IW")

WM <- data.frame(Type = waters, Abb = abbrs, y.min = c(3, -0.5, -Inf, -0.5, 1, 1, 1), y.max = c(Inf, 1, -0.5, 1, Inf, 3, Inf), x.min = c(34.65, 34.30, 34.40, 34.30, -Inf, 34.65, 34), x.max = c(Inf, 35.00, 35.00, 34.85, 34, Inf, 34.65), x = c(34.95, 34.6, 34.7, 34.3, 28.3, 35.0, 34.25), y = c(8.5, 0.8, -0.7, -0.5, 8.5, 2.8, 8.5), stringsAsFactors = FALSE) 
WM <- WM[!WM$Abb %in% c("LW"),]

if(return_water_type_list) {
  WM
} else {

watertype <- sapply(1:nrow(dt), function(i) {
  index <- dt[[i, sal_col]] > WM$x.min & dt[[i, sal_col]] <= WM$x.max & dt[[i, temp_col]] > WM$y.min & dt[[i, temp_col]] <= WM$y.max
  out <- WM[index, "Abb"]
  
  if(length(out) > 1) stop("Several water types matched")
  if(length(out) == 0) out <- "Other"
  
  out
})

watertype <- factor(watertype, levels = c("AW", "TAW", "IW", "ArW", "WCW", "SW", "Other"))

 if(bind) {
    cbind(dt, watertype)
    } else {
    watertype 
  }
}
}