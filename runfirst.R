### Run first script for inter-OS package development

#rm(list = ls()) ## Clear workspace

## Libraries, remember to add these as dependencies if needed

#library(PlotSvalbard)
#library(sp)
#library(rgdal)
#library(maptools)
#library(rgeos) #gIntersect
library(MarineDatabase)

## Define paths

if(Sys.info()["sysname"] == "Windows") {
  devel <- "devel/"
  twice <- "C:/Users/mikko/Dropbox/Workstuff/2017 TW-ICE/"
  } else {
 NULL
}
