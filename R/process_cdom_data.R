#' @title Process CDOM data from Excel sheets
#' @description Reads CDOM data from Excel sheets and calculates a range of parameters that can be exported to the database.  
#' @param data_file Name of the excel file containing the dataset to be exported.
#' @param sheet sheet number or name where data are located. If \code{"all"} (default), all sheets in the Excel file are opened and processed. See Details and \code{\link[openxlsx]{read.xlsx}} for further information.
#' @param blank_correction Character specifying the type of blank correction that should be used. Options: \code{"One milliQ"} (default) or \code{"Moving average"}. See Details for requirements for each type.
#'
#' @details The function runs following steps for individual data sheets:
#' \enumerate{
#' \item Read in data
#' 
#' Reads data from a MS Excel file using the \link[openxlsx]{openxlsx} package. The Excel sheets have to be formatted in a specific way explained in Figure 1. Each data sheet (i.e. tab) is allowed to contain only three types of columns: one wavelength column, blank columns and sample columns. In other words, meta-data should not be included in the sheet, but these can be merged with data afterwards using the \code{\link{compile_data}} function.
#' \itemize{
#'   \item \strong{Wavelength column} Only one is allowed, and this column should contain the word "wavelength" as column name. Case does not matter and the column name is allowed to contain other letters such as unit. Wave lengths should be organized row-wise. 
#'   \item \strong{Blank columns} (milliQ samples) should contain the letters "MQ" in capital letters, but are allowed to contain other characters too. Any remaining columns that do not contain the letters "MQ" or "wavelength" are assumed as sample columns.
#'   \item \strong{Sample columns} should contain either the unique sample code (CDO-001 for example) or three numbers containing the unique numeric label (001 for example) as a part of their column name. These codes are used to merge CDOM data with meta-data in \code{\link{compile_data}} function. 
#'}
#' \figure{cdom_sheet_example.png}{options: width=530}
#' 
#' Figure 1. Example how CDOM Excel sheets should be arranged. Note that commas (,) should not be used as decimal separators as in this example. Configure Excel to use periods (.) as decimal separators instead.
#' \item Blank correction
#' 
#' Two methods for blank correction (i.e. subtraction of milliQ values) are implemented:
#' \itemize{
#' \item \strong{\code{"One milliQ"}} One MQ is subtracted from all spectra in an Excel sheet. If this method is selected, only one MQ column per sheet is allowed. The order of sample columns does not matter.
#' \item \strong{\code{"Moving average"}} Blank correction is done using a classic instrument drift correction routine by calculating moving averages of milliQ samples on both sides of samples and subtracting these moving averages from sample values. If this method is selected, sample and MQ columns have to be ordered in the same order they were analyzed. 
#' }
#' \item Baseline correction
#' 
#' Baseline correction is done by substracting the average absorption in the range 600-650 nm from each spectra. This routine introduces the assumption that absororbance is equal to zero in the range 600-650 nm attributing any variation from zero to instrument baseline drift, temperature, scattering, and refractive effects (see Spectral corrections and S determination section in Material and Methods in Helms et al. 2008 and Green & Blough 1994).
#' 
#' \item Conversion from absorbance to absorption coefficients
#' 
#' Absorbances are converted to absorption coefficients using the following equation from Helms et al. (2008):
#' \deqn{2.303 * A_{\lambda} / l}{2.303 * A(\lambda) / l}
#' 
#' Where \eqn{A_{\lambda}}{A(\lambda)} is the absorbance for wavelength \eqn{\lambda} and \code{l} the path length in meters. 0.1 was used as path length.
#'    
#' \item Calculate spectral slopes for 275-295 and 350-400 nm using linear model fitting
#' 
#' See Value for details about spectal slope calculation
#' 
#' \item	Calculate spectral slopes for 300-650 nm using nonlinear model fitting.
#' 
#' See Value for details about spectal slope calculation.
#' 
#' \item Return a list of variables
#' 
#' A list of class \code{CDOMdata} is returned. See Value for details.
#' }
#' 
#' The routine above is conducted by a hidden function named \code{.cdom_data}. The \code{process_cdom_data} function loops the \code{.cdom_data} function over all tabs in an Excel file if \code{sheet = "all"} or over selected tabs, if a vector containing sheet numbers or names is supplied. Another hidden function \code{.combine_cdom_data} combines data provided by \code{.cdom_data} and generates a progress bar as running the routine over large files might take some time.
#' 
#' @return Returns a list of class \code{CDOMdata} containing a data frame of spectral absorption coefficients (\code{$spectra}) and a data frame of desired variables (\code{$data}) to be exported to the database. The element \code{data} contains following columns:
#' \itemize{
#' \item \strong{\code{sample_name}} The standardized sample name that can be linked to \link[=export_metadata]{meta-data}
#' \item \strong{\code{abs254}} Absorption coefficient (\eqn{m^{-1}}{m-1}) for 254 nm wavelength
#' \item \strong{\code{abs350}} Absorption coefficient (\eqn{m^{-1}}{m-1}) for 350 nm wavelength
#' \item \strong{\code{abs375}} Absorption coefficient (\eqn{m^{-1}}{m-1}) for 375 nm wavelength
#' \item \strong{\code{abs440}} Absorption coefficient (\eqn{m^{-1}}{m-1}) for 440 nm wavelength
#' \item \strong{\code{sl295}} Spectral slope for wavelengths between 275 and 295 nm. The slope is calculated using linear approximation after Helms et al. (2008). Absorption coefficients are logarithm transformed prior fitting using a linear model (see \code{\link{lm}}). The values are reported as positive exponential numbers following the mathematical convention of fitting to an exponential decay (see \code{sl650}), but are actually negative. The slopes are expressed in \eqn{µm^{-1}}{µm-1}. If the slope is \code{NA}, some of the absorption coefficients were negative over the spectral range (275-295 nm), making it impossible to logarithm transform the values.
#' \item \strong{\code{sl400}} Spectral slope (\eqn{µm^{-1}}{µm-1}) for wavelengths between 350 and 400 nm. See \code{sl295} for details.
#' \item \strong{\code{slope_ratio}} Slope ratio between \code{sl295} and \code{sl400} (\eqn{sl295/sl400}). The ratio is calculated using the exponential slopes.
#' \item \strong{\code{sl650}} Spectral slope (\eqn{µm^{-1}}{µm-1}) for wavelengths between 300 and 650 nm. The slope is calculated by fitting a nonlinear exponetial model on absorbation coefficients using the \code{\link{nls}} function. The equation is from Helms et al. (2008) and can be expressed as follows:
#' \deqn{a_{\lambda} = a_{0} \times e^{-sl650 \times wavelength}}{a(\lambda) = a(0) * exp(-sl650 * wavelength)}
#' where \eqn{a_{\lambda}}{a(\lambda)} are the absorption coefficient values over the spectrum 300 - 650 nm, \eqn{a_{0}}{a(0)} the intercept and \code{sl650} the slope. The nonlinear estimation can handle negative absorption coefficient values, unlike the linear approximation, and produces very similar results to linear approximation (Helms et al. 2008). The downside with nonlinear estimation is that it might not manage to fit the function on data that contain heavy instrumental errors. Currently the function does not include a buffer against this and may return an error, when used on bad data.
#' }
#'
#' @references Green, S.A., Blough, N. V, 1994. Optical absorption and fluorescence properties of chromophoric dissolved organic matter in natural waters. Limnol. Oceanogr. 39, 1903–1916. doi:10.4319/lo.1994.39.8.1903
#' 
#' Helms, J.R., Stubbins, A., Ritchie, J.D., Minor, E.C., Kieber, D.J., Mopper, K., 2008. Absorption spectral slopes and slope ratios as indicators of molecular weight, source, and photobleaching of chromophoric dissolved organic matter. Limnol. Oceanogr. 53, 955–969. doi:10.4319/lo.2008.53.3.0955
#' 
#' Stedmon, C.A., Markager, S., Kaas, H., 2000. Optical Properties and Signatures of Chromophoric Dissolved Organic Matter (CDOM) in Danish Coastal Waters. Estuar. Coast. Shelf Sci. 51, 267–278. doi:10.1006/ecss.2000.0645
#' @import openxlsx
#' @example #make some
#' @author Mikko Vihtakari, Alexey Pavlov
#' @seealso \code{\link{plot.CDOMdata}} \code{\link{print.CDOMdata}}
#' @export

#data_file = paste0(devel, "GlacierFront_2017_CDOM.xlsx"); sheet = "all"; blank_correction = "One milliQ"
process_cdom_data <- function(data_file, sheet = "all", blank_correction = "One milliQ") {

file_ext <- get_file_ext(data_file)

if(!file_ext %in% c("xlsx", "xls")) stop("The function requires an Excel file")

if(length(sheet) == 0) stop("sheet argument must be defined")

if(sheet == "all") {
  sheets <- getSheetNames(data_file)

  .combine_cdom_data(data_file, sheets, blank_correction)
} else {
  .combine_cdom_data(data_file, sheet, blank_correction)
}

}


#dat = paste0(devel, "GlacierFront_2017_CDOM.xlsx"); sht = 1:2; blk = "One milliQ"
.combine_cdom_data <- function(dat, sht, blk) {

pb <- txtProgressBar(min = 0, max = length(sht), style = 3)  
  
tmp <- lapply(sht, function(k) {
  setTxtProgressBar(pb, which(sht == k))
  return(.cdom_data(dat, Sheet = k, Blank_correction = blk))
})  
  
  out <- do.call(rbind, lapply(tmp, function(k) k$data))
  proc_data <- Reduce(function(x, y) merge(x, y, by= "wavelength"), lapply(tmp, function(k) k$spectra))
  #mods <- do.call(c, lapply(tmp, function(k) k$models))
  mod.dat <- do.call(rbind, lapply(tmp, function(k) k$model.data))
  
  x <- list(spectra = proc_data, data = out, model.data = mod.dat)

  class(x) <- class(x) <- "CDOMdata"

  close(pb)
  
  return(x)
}

#Data_file = paste0(devel, "GlacierFront_2017_CDOM.xlsx"); Sheet = 1; Blank_correction = "One milliQ"
.cdom_data <- function(Data_file, Sheet, Blank_correction) {

dt <- read.xlsx(Data_file, Sheet)

## Column classes ####

dt <- data.frame(apply(dt, 2, function(k) as.numeric(as.character(k))))

## Column names ####

### Find the wavelength column

colnames(dt)[grep("wavelength", colnames(dt), ignore.case = TRUE)] <- "wavelength"

### Find MilliQ columns

mqs <- grep("MQ", colnames(dt), value = TRUE)

### Find the sample columns
sample_cols <- colnames(dt)[!colnames(dt) %in% c("wavelength", mqs)]
tmp <- strsplit(sample_cols, split = "[[:punct:]]")
tmp <- sapply(tmp, function(k) grep("^[[:digit:]]", k, value = TRUE))
colnames(dt)[colnames(dt) %in% sample_cols] <- paste("CDO", tmp, sep = "-")

if(length(mqs) == 0) stop(paste("The data sheet", sheet, "does not contain MilliQ samples. Make sure that they are labelled 'MQ'."))
if(length(mqs) != 1 & Blank_correction == "One milliQ") stop(paste("One milliQ method can handle only one milliQ sample / datasheet. Check sheet", sheet))

## Remove NAs ####

dt <- na.omit(dt)

## Blank correction ####

if(Blank_correction == "One milliQ") {
  dt[grep("CDO", colnames(dt))] <- dt[grep("CDO", colnames(dt))] - dt[[mqs]]
  dt <- dt[!colnames(dt) %in% mqs]
} else {
  stop("Other blank correction methods have not been implemented yet. Send an email to Mikko")
}

## Baseline correction with 600-650 nm average

x <- subset(dt, wavelength <= 650 & wavelength >= 600)
x <- colMeans(x[!colnames(x) %in% "wavelength"])
y <- dt[!colnames(dt) %in% "wavelength"]

dt[!colnames(dt) %in% "wavelength"] <- y - x[col(y)]

## Conversion from absorbance to absorption coefficient ####

x <- dt[!colnames(dt) %in% "wavelength"]
dt[!colnames(dt) %in% "wavelength"] <- (2.303 * x) / 0.1

proc_data <- dt 

## Output data ####

out <- lapply(grep("CDO", colnames(dt), value = TRUE), function(k) {
  tmp <- dt[c("wavelength", k)]
  data.frame(sample_name = k, 
    abs254 = tmp[tmp$wavelength == 254, k],
    abs350 = tmp[tmp$wavelength == 350, k],
    abs375 = tmp[tmp$wavelength == 375, k],
    abs440 = tmp[tmp$wavelength == 440, k],
    positive295 = min(tmp[tmp$wavelength >= 275 & tmp$wavelength <= 295, k]) >= 0,
    positive400 = min(tmp[tmp$wavelength >= 350 & tmp$wavelength <= 400, k]) >= 0)
})

out <- do.call(rbind, out)

## Slope ratio log(275-295)/log(350-400)

Sr <- lapply(grep("CDO", colnames(dt), value = TRUE), function(k) {
  
  tmp <- dt[c("wavelength", k)]
  colnames(tmp)[colnames(tmp) %in% k] <- "value"
  
  
  dat295 <- tmp[tmp$wavelength >= 275 & tmp$wavelength <= 295,]
  dat400 <- tmp[tmp$wavelength >= 350 & tmp$wavelength <= 400,]
  
  if(out[out$sample_name == k, "positive400"]) {
    
  mod295 <- lm(log(value) ~ wavelength, data = dat295)
  mod400 <- lm(log(value) ~ wavelength, data = dat400)
  
  data.frame(sample_name = k, sl295 = -1000*unname(coef(mod295)[2]), sl400 = -1000*unname(coef(mod400)[2]), slope_ratio = unname(coef(mod295)[2])/unname(coef(mod400)[2]))
  
  } else {
    if(out[out$sample_name == k, "positive295"]) {
      mod295 <- lm(log(value) ~ wavelength, data = dat295)
      
      data.frame(sample_name = k, sl295 = -1000*unname(coef(mod295)[2]), sl400 = NA, slope_ratio = NA)    
    } else {
      data.frame(sample_name = k, sl295 = NA, sl400 = NA, slope_ratio = NA)    
    }}
})

Sr <- do.call(rbind, Sr)

out <- merge(out, Sr)

## Spectral slope for 300-600 nm ####

#k <- grep("CDO", colnames(dt), value = TRUE)[1]
S650 <- lapply(grep("CDO", colnames(dt), value = TRUE), function(k) {
  tmp <- dt[dt$wavelength <= 650 & dt$wavelength >= 300, c("wavelength", k)]
  colnames(tmp)[colnames(tmp) %in% k] <- "value"
  
  mod <- nls(value ~ a * exp(-S * wavelength), data = tmp, start = list(a = 78, S = 0.02))
  #mod <- nls(value ~ SSasymp(wavelength, Asym = K, R0, lrc = S), data = tmp)
  mod.dat <- data.frame(sample_name = k, wavelength = tmp$wavelength, fitted = predict(mod))
  
  list(data = data.frame(sample_name = k, sl650 = 1000*coef(mod)[["S"]]), model.data = mod.dat)
})

S650dat <- do.call(rbind, lapply(S650, function(k) k$data))

mods <- do.call(rbind, lapply(S650, function(k) k$model.data))

out <- merge(out, S650dat)

## Export the data ####

out <- out[!colnames(out) %in% c("positive400", "positive295")]

x <- list(spectra = proc_data, data = out, model.data = mods)

class(x) <- "CDOMdata"

x
}
