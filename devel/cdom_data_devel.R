source("runfirst.R")
library(MarineDatabase)

x <- process_cdom_data(paste0(devel, "GlacierFront_2017_CDOM.xlsx"), sheet = "all", blank_correction = "One milliQ")

x

plot(x)

plot(x, "model_fit")

if(length(sheet > 1)) {
    .combine_cdom_data(data_file, sheet, blank_correction)
  } else {
  if(sheet == "all") {
  sheets <- getSheetNames(data_file)

.combine_cdom_data(data_file, sheets, blank_correction)
} else {
  
   .cdom_data(data_file, sheet, blank_correction)  
  }}



#' \item \strong{\code{sl650}} Spectral slope for wavelengths between 600 and 650 nm. The slope is calculated by fitting a nonlinear exponetial model on absorbation coefficients using \code{\link{nls}} function in fitting and \code{\link{SSasymp}} function for defining the start parameters. The equation is from Stedmon et al. (2000) and can be expressed as follows:
#' \deqn{a_{\lambda} = a_{0} \times e^{-sl650 \times wavelength} + K}{a(\lambda) = a(0) * exp(-sl650 * wavelength) + K}
#' where \eqn{a_{\lambda}}{a(\lambda)} are the absorption coefficient values over the spectrum 300 - 650 nm, \eqn{a_{0}}{a(0)} the intercept, \code{sl650} the slope and \code{K}  the asymptote allowing \eqn{a_{\lambda}}{a(\lambda)} values to be negative. The formula is taken directly from Stedmon et al. (2000), and excluding the asymptote might make scientifically more sense as negative absorption values indicate that the instrument and standardization procedures did not work as intended, since negative or zero absorption coefficient values are not possible. 
