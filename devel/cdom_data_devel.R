source("runfirst.R")
library(MarineDatabase)

meta <- export_metadata(paste0(twice, "GlacierFront_2017_Samplelog_20171211.xlsx"), sheet = "SAMPLELOG", guess_colnames = TRUE)

dat <- process_cdom_data(paste0(devel, "GlacierFront_2017_CDOM.xlsx"), sheet = "all", blank_correction = "One milliQ")

x <- compile_data(dat, meta)

dat2 <- process_cdom_data(paste0(devel, "MOSJ-2016 CDOM raw 1.xlsx"), sheet = "all", blank_correction = "Moving average")

plot(dat2, type = "model_fit")


tmp <- read.xlsx("devel/MOSJ-2016 CDOM raw 1.xlsx", 1)
tmp <- data.frame(apply(tmp, 2, function(k) as.numeric(as.character(k))))

colnames(tmp)[2:length(colnames(tmp))] <- gsub("\\.", "\\-", colnames(tmp)[2:length(colnames(tmp))])
colnames(tmp)[1] <- "Wavelength"
tmp <- tmp[!colnames(tmp) %in% c("MOSJ2016-009", "MOSJ2016-009-B", "MOSJ2016-MQ009")]
colnames(tmp)[colnames(tmp) == "MOSJ2016-MQ1"] <- "MOSJ2016-MQ009"
ord <- c("Wavelength", "MOSJ2016-MQ0", "MOSJ2016-001", "MOSJ2016-002", "MOSJ2016-003", "MOSJ2016-004", "MOSJ2016-005", "MOSJ2016-006", "MOSJ2016-007", "MOSJ2016-008", "MOSJ2016-MQ009", "MOSJ2016-010", "MOSJ2016-011", "MOSJ2016-012", "MOSJ2016-013", "MOSJ2016-014", "MOSJ2016-015", "MOSJ2016-016", "MOSJ2016-017", "MOSJ2016-018", "MOSJ2016-019", "MOSJ2016-MQ019", "MOSJ2016-020", "MOSJ2016-021", "MOSJ2016-022", "MOSJ2016-023", "MOSJ2016-024", "MOSJ2016-025", "MOSJ2016-026", "MOSJ2016-027", "MOSJ2016-028", "MOSJ2016-029", "MOSJ2016-030", "MOSJ2016-MQ030")


tmp <- tmp[ord]

write.xlsx(tmp, "devel/MOSJ-2016 CDOM raw 1.xlsx")

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
