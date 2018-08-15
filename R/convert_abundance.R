#' @title Calculate and convert between abundance values for ice core and planton net data
#' @description Not ready conversion function
#' @param data Data.frame containing the data to be converted
#' @param ab_col Character vector specifying the columns containing abundance data
#' @param ab_from The initial abundance unit. Options: "n" (number of individuals), "nsub" (number of individuals in a subsample), "1/L" (number of individuals per litre), "1/m3" (number of individuals per cubic metre), "1/m2" (number of individuals per square metre).
#' @param ab_to The desired abundance unit. Options: "1/L" (number of individuals per litre), "1/m3" (number of individuals per cubic metre), "1/m2" (number of individuals per square metre).
#' @param filtered Logical indicating whether the sample was filtered (\code{filtered = TRUE}) or subsampled (\code{filtered = FALSE}). See details.
#' @param type Character argument specifying either the gear column (see \code{\link{read_metadata}}) or either of following: "icecore" or "net".
#' @param cf Numeric giving the concentration factor connected to the volume difference when transforming water to ice (melted water volume/ice volume). Used only, if \code{type} indicates ice core. To remove the effect, set \code{cf = 1}.
#' @param vol_col Character argument specifying the volume column to be used in calculations. Should be supplied in \strong{liters}.
#' @param start_col Character argument specifying the name of the column in \code{data} for the section start for the ice core or plankton net in \strong{meters}.
#' @param end_col Character argument specifying the name of the column in \code{data} for the section end for the ice core or plankton net in \strong{meters}.
#' @param unit_col Character argument specifying the unit column name in \code{data}.
#' @details The function to calculate ind m^-2 values from ind is (ind*cf*1000*h)/V, where ind is number of individuals in a sample, cf the concentration factor, h height of ice-core section in m, and V melted volume in L.
#' 
#' The function is not ready and works only for "1/L" to "1/m2" conversions with very limited options. 
#' 
#' \itemize{
#'   \item \strong{n} 
#'   }
#' 
#' @author Mikko Vihtakari
#' @export

#data = k; ab_col <- c("abundance"); ab_from = "L"; ab_to = "m2"; filtered = TRUE; type = "icecore"; cf = 1; vol_col = NULL; start_col = "from"; end_col = "to"; unit_col = "unit"
convert_abundance <- function(data, ab_col, ab_from = NULL, ab_to = NULL, filtered = NULL, type = "icecore", cf = 1, vol_col = NULL, start_col = "from", end_col = "to", unit_col = "unit") {

  if(is.null(ab_from)) stop("Specify from argument. I.e the initial abundance unit.")    
  if(is.null(ab_to)) stop("Specify to argument. I.e the desided abundance unit.")
  if(is.null(filtered)) stop("Specify filtered argument. I.e whether samples were filtered or subsampled")
  if(!is.null(vol_col)) if(type == "icecore" & any(data[vol_col] > 10)) warning("Volume needs to be supplied as liters. Very high volumes for an ice core. Are you sure?")
  
  ## Switches
  
  cm.switch <- FALSE
  
  ### Conversions ####
  
  ### Filtered samples ####
  if(filtered) {
    
    if(ab_from == "1/L" & ab_to == "1/m2") {
      if(type == "icecore" & (any(data[[start_col]] > 10) | any(data[[end_col]] > 10))) {
        cm.switch <- TRUE
        message(paste0("start_col and end_col should be supplied as meters. Too high values (up to ", max(c(data[[start_col]], data[[end_col]])), "). Converted assuming that core distances are given as cm."))
      data[[start_col]] <- data[[start_col]]/100
      data[[end_col]] <- data[[end_col]]/100
      }
        
      data[[ab_col]] <- indL2indm2(data = data, ab.cols = ab_col, sec.start = start_col, sec.end = end_col, cf. = cf)
      data[[unit_col]] <- "1/m2"
      
      if(cm.switch) {
      data[[start_col]] <- data[[start_col]]*100
      data[[end_col]] <- data[[end_col]]*100
      data
      } else {
      data  
      }
      
    } else {
      stop("Conversion from ", ab_from, " to ", ab_to, " has not been implemented yet")
      }
      
  } else {
  ### Nonfiltered samples ####  
    stop("Nonfiltered samples have not been implemented yet")
    
    
  }
}

## To calculate Ind/m2 values from Ind
ind2indm2 <- function(data, ab.cols, V = "Volume", sec.start = "Sec.start", sec.end = "Sec.end", cf. = cf) { 
  return(ind2indL(data, ab.cols, V, cf)*((data[[sec.end]]-data[[sec.start]])/100))
}


## To calculate Ind/m2 values from Ind/L
indL2indm2 <- function(data, ab.cols, sec.start, sec.end, cf.) { 
  return((1000*data[[c(ab.cols)]]*cf.*((data[[sec.end]]-data[[sec.start]]))))
  }

## To calcule Ind/L values from Ind
ind2indL <- function(data, ab.cols = ab_col, V = vol_col, cf. = cf) {
  return((cf.*data[ab.cols])/(data[[V]]))
  }
