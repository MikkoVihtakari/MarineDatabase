#' @name GEAR
#' @title Gear types
#' @description A dataframe containing allowed gear types.
#' @docType data
#' @usage data("gear_types")
#' @format a data.frame 
NULL


#' @name TYPES
#' @title Data types
#' @description A dataframe containing allowed sample codes.
#' @docType data
#' @usage data("sample_types")
#' @format A data.frame 
NULL

#' @name STATIONS
#' @title Standardized station names
#' @description A dataframe containing standardized station names.
#' @docType data
#' @usage data("stations")
#' @format A data.frame 
NULL

#' @name ZOOPL
#' @title Standardized zooplankton species names
#' @description A dataframe containing standardized zooplankton species names. 
#' 
#' \itemize{
#' \item The "species", "stage", "length", "size_op" and "length_old" columns are used in species name matching (\code{\link{read_zooplankton_data}}). 
#' \item The "species_ID" column contains unique standardized species names: 
#'  \itemize{
#'  \item Capital letters in the beginning of the string refer to genus and higher ranks, while lower case letters indicate species names. 
#'  \item Numbers refer to either stage or length. character "t" means range (from Y to X, where Y and X are lengths in millimetres.). 
#'  \item Lower case strings "zoe", "meg", "naup", "calyp", "med", "pil", "vel", and "lar" are used for various larval forms. 
#'  \item Lower case "juv" is used for juveniles. 
#'  \item "FM" and "AM" refer to adult females and males, respectively. 
#'  }
#' }
#' @docType data
#' @usage data("ZOOPL")
#' @format A data.frame 
NULL