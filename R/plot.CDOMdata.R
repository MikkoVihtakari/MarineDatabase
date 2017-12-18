##' @title Plot CDOM data
##' @description Plot method for \code{\link[=process_cdom_data]{CDOMdata}} objects.
##' @param x \code{CDOMdata} object from \code{\link{process_cdom_data}} function.
##' @param type Character specifying the type of the plot. Options: \code{"spectra"}, \code{"models"} or \code{"model_fit"}. See Details
##' @param ... Further arguments that do not work.
##' @method plot CDOMdata
##' @details Three plot types have been implemented:
##' \itemize{
##' \item \strong{\code{"spectra"}} Plots the absorbtion coefficient of all samples against wavelength. Flags samples that had negative values in the range between 350 and 400 nm. 
##' \item \strong{\code{"models"}} Plots exponential decay models fitted using the \code{\link{nls}} function. See Value in \code{\link{process_cdom_data}} for details.
##' \item \strong{\code{"model_fit"}} Plots both the absorbition coefficients and fitted nonlinear models. 
##' }
##' 
##' Note that especially the \code{"model_fit"} option might be slow for large datasets.
##' @seealso \code{\link{process_cdom_data}}
##' @author Mikko Vihtakari
##' @import ggplot2 reshape2
##' @export

plot.CDOMdata <- function(x, type = "spectra", ...) {

  if(class(x) != "CDOMdata") stop("x must be a CDOMdata object")

  switch(type,
    spectra = {
      
      mx <- melt(x$spectra, id = 1, variable.name = "sample_name")
      mx <- merge(mx, x$data[c("sample_name", "sl295", "sl400", "slope_ratio", "sl650")], by = "sample_name")
      mx$negative400nm <- ifelse(is.na(mx$sl400), "Fail", "Pass")
  
      ggplot(mx, aes(x = wavelength, y = value, group = sample_name, color = negative400nm)) + 
        geom_line(size = 0.2, alpha = 0.5) + 
        theme_classic() + 
        ylab(expression(paste("Absorption coefficient (", m^-1, ")"))) + 
        xlab("Wavelength (nm)") +
        scale_color_manual(name = "Negative at\n400 nm", values = c("red", "black"))
      },
    
    models = {
      
      fx <- merge(x$model.data, x$data[c("sample_name", "sl400")], by = "sample_name", all = TRUE)
      fx$negative400nm <- ifelse(is.na(fx$sl400), "Fail", "Pass")
  
      ggplot(fx, aes(x = wavelength, y = fitted, group = sample_name, color = negative400nm)) + 
        geom_line(size = 0.2, alpha = 0.5) + 
        theme_classic() + 
        ylab(expression(paste("Fitted absorption coefficient (", m^-1, ")"))) + 
        xlab("Wavelength (nm)") + 
        scale_color_manual(name = "Negative at\n400 nm", values = c("red", "black"))
      },
    
    model_fit = {
      
      mx <- melt(x$spectra, id = 1, variable.name = "sample_name")
      mx <- merge(mx, x$data[c("sample_name", "sl295", "sl400", "slope_ratio", "sl650")], by = "sample_name")
      mx$negative400nm <- ifelse(is.na(mx$sl400), "Fail", "Pass")
  
      fx <- merge(x$model.data, x$data[c("sample_name", "sl400")], by = "sample_name", all = TRUE)
      fx$negative400nm <- ifelse(is.na(fx$sl400), "Fail", "Pass")
        
      ggplot() + 
        geom_point(data = mx, aes(x = wavelength, y = value, group = sample_name, fill = negative400nm), size = 0.1, alpha = 0.1, shape = 21) + 
        geom_line(data = fx, aes(x = wavelength, y = fitted, group = sample_name, color = negative400nm), size = 0.3, alpha = 0.5) + 
        theme_classic() + 
        ylab(expression(paste("Absorption coefficient (", m^-1, ")"))) + 
        xlab("Wavelength (nm)") + 
        scale_color_manual(name = "Negative at\n400 nm", values = c("red", "black")) + 
        scale_fill_manual(name = "Negative at\n400 nm", values = c("red", "grey90"))
      },
    
    stop(paste(type, "is not implemented"))
    
    )
}

