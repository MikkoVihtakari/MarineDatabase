##' @title Compile marine biological data to Norwegian Polar Institute data format
##' @description The function compiles scientific cruise data from +emph{one station} to the format required by the Norwegian Polar Institute data base
##' @param dt data frame containing overview of the cruise data. See details for formatting instructions.
##' @param ctds list of \link[oce]{ctd-class} objects taken at the station. Required if \code{dt} contains CTD casts.
##' @param station.col Character. Column name specifying the station name in \code{dt}
##' @param date.col Character. Column name specifying the sampling time in \code{dt}. The \code{date.col} in \code{dt} be in \code{\link[=DateTimeClasses]{POSIXct}} format.
##' @param expedition Character string specifying the name of the expedition.
##' @param latitude.col Character. Column name specifying the latitude column in \code{dt}. The \code{latitude.col} in \code{dt} be given in decimal degrees.
##' @param longitude.col Character. Column name specifying the longitude column in \code{dt}. The \code{longitude.col} in \code{dt} be given in decimal degrees.
##' @param variable.col Character. Column name specifying the variable column. This is stupid programming and should be changed.
##' @param value.col Character. Column name specifying the value column. This is stupid programming and should be changed.
##' @param type.col Column name specifying the sample type column in \code{dt}. This will be used to place data on the same level than CTD data in the hierarchy. Important!
##' @param gear.col Column name specifying the sampling gear column in \code{dt}
##' @param unit.col Column name specifying the unit of \code{value.col} column in \code{dt}
##' @param sample.name.col Column name specifying the sample name column in \code{dt}. Important for binding meta-data and actual data together.
##' @param sampling.depth.col Column name specifying the sampling depth column in \code{dt}. Important parameter. Should be given in meters.
##' @param find.ctds Logical. Should the function find ctd casts from a list of \link[oce]{ctd-class} objects based on station name?
##' @details The data frame for \code{dt} should be formatted as Dr. Anette Wold's Excel sheets tend to be formatted.
##' @author Mikko Vihtakari
##' @import oce lubridate
##' @export

#dt = stn_data; station.col = "Station"; date.col = "Date"; expedition = "TWICE"; latitude.col = "lat"; longitude.col = "lon"; ctds = ctd.data; variable.col = "variable"; value.col = "value"; type.col = "Type"; gear.col = "Gear"; unit.col = "unit"; sample.name.col = "Name"; sampling.depth.col = "From"; find.ctds = TRUE
#
# make_station_data <- function(dt, station.col = "Station", date.col = "Date", expedition = "TWICE", latitude.col = "lat", longitude.col = "lon", ctds = NULL, variable.col = "variable", value.col = "value", type.col = "Type", gear.col = "Gear", unit.col = "unit", sample.name.col = "Name", sampling.depth.col = "From", find.ctds = TRUE) {
#
#   dt <- droplevels(dt)
#
#   ## Conditionals
#
#   if(class(ctds) == "ctd") ctds <- list(ctds)
#
#   ### Metadata
#   stn <- as.character(unique(dt[, station.col]))
#   if(length(stn) != 1) stop("Data contains several stations or no station information. Check station.col argument")
#   DATE <- min(dt[,date.col])
#   mnt <- ifelse(nchar(month(DATE)) == 1, paste0("0", month(DATE)), month(DATE))
#   DAY <- ifelse(nchar(day(DATE)) == 1, paste0("0", day(DATE)), day(DATE))
#   DT <- paste0(year(DATE), mnt, day(DATE))
#   name <- paste(DT, expedition, stn, sep = "_")
#   time <- min(slog[, date.col], na.rm = TRUE)
#   DEPTH <- max(c(slog$Depth, slog$From), na.rm = TRUE)#max(c(dt$Depth, dt$From, unlist(lapply(ctds, function(k) k@metadata$waterDepth))), na.rm = TRUE)
#
#   ### Metadata list
#
#   a <- list(list(metadata = list(
#     station = stn,
#     latitude = mean(slog[,latitude.col], na.rm = TRUE),
#     longitude = mean(slog[,longitude.col], na.rm = TRUE),
#     date = strftime(as.POSIXct(time, "UTC"), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
#     bottom_depth = DEPTH
# )))
#
#   names(a) <- name
#
# ### CTD data
#
# if(nrow(dt[dt$Type == "CTD",]) != 0) {
#  if(is.null(ctds)) stop("Give oce::ctd-class objects or remove CTDs from data")
#   if(is.list(ctds) & find.ctds) {
#     ctds <- ctds[dt[dt[,type.col] == "CTD", variable.col]]
#     message(paste("Finding CTD casts from a list of ctd objects.", names(ctds), "selected"))
#   } else {
#     NULL
#   }
#
#  a[[1]]$data$ctd <- lapply(ctds, function(g) list(metadata =
# g@metadata[!names(g@metadata) %in% c("header", "flags", "hexfilename", "filename")], data = g@data))
#
# }
#
# ### Exsisting data
#
# x <- dt[!is.na(dt[,value.col]),]
# x <- droplevels(x)
#
# datas <- lapply(unique(x[, variable.col]), function(g) {
# y <- subset(x, variable == g)
#
# if(any(c(length(unique(y[,latitude.col])) > 1, length(unique(y[,longitude.col])) > 1))) warning(paste("Several latitude and longitude values for", unique(stn)))
#
# ml <- list(latitude = unique(y[,latitude.col]), longitude = unique(y[,longitude.col]), gear = as.character(unique(y[,gear.col])), sample_type = as.character(unique(y[,type.col])), date = strftime(as.POSIXct(unique(y[,date.col]), "UTC"), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"), variable = as.character(unique(y[,variable.col])), unit = as.character(unique(y[,unit.col])))
#
# dl <- list(sample_name = as.character(y[,sample.name.col]), depth_from = y[,sampling.depth.col], value = y[,value.col])
#
# list(metadata = ml, data = dl)
# })
#
# names(datas) <- gsub(" ", "_", tolower(unique(x[,variable.col])))
#
# a[[1]]$data <- append(a[[1]]$data, datas)
#
# ### Missing data
#
# missings <- gsub(" ", "_", tolower(unique(dt[,type.col])))
# missings <- missings[!missings %in% select(strsplit(names(a[[1]]$data), "_"), 1)]
#
# miss <- lapply(missings, function(k) list(metadata = "missing", data = NA))
# names(miss) <- missings
#
# a[[1]]$data <- append(a[[1]]$data, miss)
#
# a[[1]]$missing_data <- missings
#
# class(a) <- "NPIMarData"
#
# a
# }
