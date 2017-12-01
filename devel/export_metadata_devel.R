
source("runfirst.R")
library("openxlsx")
library("MarineDatabase")

x <- export_metadata(paste0(devel, "Samplelog MOSJ 2015.xlsx"))

 ## Only one origin date. Nice!

 # Compare with the Excel sheet. Looks good!



switch (column,
  sample.name.col = "name",
  station.col = "station",
  latitude.col = c("latitude", "decimal"),
  longitude.col = c("longitude", "decimals"),
  bottom.depth.col = c("bottom", "depth"),
  date.col = "date",
  gear.col = "gear",
  from.col = "from",
  to.col = "to",
  filtered.vol.col = "volume",
  type.col = "type",
  responsible.col = "person",
  comment.col = "comment",
  stop(paste(fn, "column type not set"))
)

c("sample.name.col", "station.col", "latitude.col", "longitude.col", "bottom.depth.col", "date.col", "gear.col", "from.col", "to.col", "filtered.vol.col", "type.col", "responsible.col", "comment.col")

column <- "sample.name.col"
df <- dt

colnames(df)
x <- gsub(".col", "", column)



grep(x, colnames(df), ignore.case = TRUE)



if(guess.colnames) {


}

?read.xlsx
