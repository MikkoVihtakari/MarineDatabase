% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset-docs.R
\docType{data}
\name{ZOOPL}
\alias{ZOOPL}
\title{Standardized zooplankton species names}
\format{
data.frame
}
\usage{
data("ZOOPL")
}
\description{
A dataframe containing standardized zooplankton species names. 

\itemize{
\item The "species", "stage", "length", "size_op" and "length_old" columns are used in species name matching (\code{\link{read_zooplankton_data}}). 
\item The "species_ID" column contains unique standardized species names: 
 \itemize{
 \item Capital letters in the beginning of the string refer to genus and higher ranks, while lower case letters indicate species names. 
 \item Numbers refer to either stage or length. character "t" means range (from Y to X, where Y and X are lengths in millimetres.). 
 \item Lower case strings "zoe", "meg", "naup", "calyp", "med", "pil", "vel", and "lar" are used for various larval forms. 
 \item Lower case "juv" is used for juveniles. 
 \item "FM" and "AM" refer to adult females and males, respectively. 
 }
}
}
