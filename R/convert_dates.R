#' @title Convert Excel dates to consistent date formatting
#' @description Converts messy nonstandarized Excel dates to consistent date formatting.
#' @param dt Data.frame for which the date conversions should be made. The date column has to be named as \code{date}.
#' @param date_col Not implemented yet. Name of the column, which contains dates to be converted.
#' @param excel_file path to the Excel file where the dates originate from. Can be left empty, if the date conversion should be done for other type of files (for example .csv or .txt).
#' @param file_ext Extension of the data file. Can be left empty, if \code{dt} originates from another type file than Excel sheet.
#' @param add_time Hours to be added to the ISO 8601 \code{date}s. See Details. 
#' @param date_origin The origin for recorded dates in the Excel sheet in "YYYY-MM-DD" format. See Details.
#' @param output_format Character string specifying in which format the date information should be returned. Options: \code{"iso8601"} (default) returns the date column as a character string in ISO 8601 standard, \code{"POSIXct"} returns the column in \link[as.POSIXct]{UTC time format}, and \code{"as.Date"} returns the date column \link[as.Date]{in Date format ignoring hours, minutes and seconds}.
#' @return Returns a data.frame equal to \code{dt} with \code{date_col} as character representing ISO 8601 dates. 
#' @details Large (biological) datasets are often recorded on Excel sheets with the file going around several computers using different operating systems and locales. This often leads to dates being recorded in multiple formats from text strings, to various Excel date formats and numeric date codes for which the origin date may vary. This function attempts to fix such inconsistensies in date formats and returns the dates as a character column representing ISO 8601 dates. The function is still experimental and due to the many ways of recording dates in Excel, the outcome might differ from the desired outcome. Please check each date returned by the function and report any inconsistencies so that the function can be improved.
#' 
#' The \code{add_time} argument can be used to add or subtract hours from the output, if the times do not match with those in the Excel sheet. This can be helpful if your locale or operating system causes an offset between recorded dates. 
#' 
#' The function also works for other types of messy dates than those recorded in Excel sheets.
#' @import openxlsx
#' @author Mikko Vihtakari
#' @export 

convert_dates <- function(dt, excel_file = NULL, file_ext = NULL, add_time = 0, date_origin = "1899-12-30", output_format = "iso8601") {

if(is.numeric(dt$date) & file_ext %in% c("xlsx", "xls")) {
  dt$temp_date <- openxlsx::convertToDateTime(dt$date, tz = "UTC")
  dt$temp_date <- dt$temp_date + add_time*3600
  dt$date <- strftime(as.POSIXct(dt$temp_date, "UTC"), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  dt <- dt[!names(dt) %in% temp_date]
  #message(paste("Date converted to ISO 8601 format. Stored as", class(dt$date), "assuming", openxlsx::getDateOrigin(excel_file), "as origin date. Control that dates match with the Excel sheet. You can use add_time to adjust if there is offset."))
} else {
  if(is.numeric(dt$date)) {
  dt$temp_date <- as.POSIXct(as.numeric(dt$date) * (60*60*24), tz = "UTC", origin = date_origin)
  dt$temp_date <- dt$temp_date + add_time*3600
  dt$date <- strftime(as.POSIXct(dt$temp_date, "UTC"), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  #message(paste("Date converted to ISO 8601 format. Stored as", class(dt$date), "class assuming", date_origin, "as origin date. Control that dates match with the Excel sheet. You can use add_time to adjust if there is offset."))
  } else {
  if(class(dt$date) == "Date") {
  dt$temp_date <- dt$date + add_time*3600
  dt$date <- strftime(as.POSIXct(dt$temp_date, "UTC"), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  #message(paste("Date converted to ISO 8601 format. Stored as", class(dt$date), "class assuming", date_origin, "as origin date. Control that dates match with the Excel sheet. You can use add_time to adjust if there is offset."))
  } else {
    
  ## If date is character (meaning there are typos), try to fix them
  if(class(dt$date) == "character") {
  temp_date <- suppressWarnings(is.na(as.numeric(dt$date)))
  if(any(temp_date)) {
    
    temp_date <- lapply(dt$date, function(k) {
      if(grepl("UTC", k)) {
        out <- strptime(k, format = "%Y-%m-%d %H:%M", tz = "UTC")
        out <- out + add_time*3600
       
      } else {
        out <- strptime(k, format = "%d.%m.%Y %H:%M", tz = "UTC")
        out <- out + add_time*3600
      } 
      
        if(is.na(out)) { #last save
        out <- as.POSIXct(as.numeric(k) * (60*60*24), tz = "UTC", origin = date_origin)
        out <- out + add_time*3600
        }
          
       strftime(as.POSIXct(out, "UTC"), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
      
      })
    
  temp_date <- unlist(temp_date)
    
  if(any(is.na(temp_date))) { 
  warning("Typo in date format for records ", paste(unique(dt$date[is.na(temp_date)]), collapse = ", "), " on rows ", paste(which(is.na(temp_date)), collapse = ", "), ". NAs produced.")
  
  dt$date <- temp_date  
  } else {
    
    dt$date <- temp_date
    #message(paste("Date converted to ISO 8601 format. Stored as", class(dt$date), "class. Control that dates match with the Excel sheet. You can use add_time to adjust if there is offset."))
  
  }} else {
  dt$temp_date <- as.POSIXct(as.numeric(dt$date) * (60*60*24), tz = "UTC", origin = date_origin)
  dt$temp_date <- dt$temp_date + add_time*3600
  dt$date <- strftime(as.POSIXct(dt$temp_date, "UTC"), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  #message(paste("Date converted to ISO 8601 format. Stored as", class(dt$date), "class assuming", date_origin, "as origin date. Control that dates match with the Excel sheet. You can use add_time to adjust if there is offset."))  
  }
    
  } else {
  stop("Implement new date conversion. Does not work for these data.")  

  }}}}
  
  dt <- dt[!names(dt) %in% "temp_date",]

  dt$date <- switch(output_format,
    iso8601 = dt$date,
    POSIXct = as.POSIXct(dt$date, "UTC"),
    as.Date = as.Date(dt$date),
    stop("Output date format is not implemented."))
  
  return(dt)
}