
source("runfirst.R")
library("openxlsx")
library("MarineDatabase")

x <- export_metadata(paste0(devel, "Samplelog MOSJ 2015.xlsx"))

x <- export_metadata(paste0(twice, "GlacierFront_2017_Samplelog_20171024.xlsx"), sheet = "SAMPLELOG", filtered_volume = "Filtration.volume.(ml)", responsible = "Contact.person")


 ## Only one origin date. Nice!

 # Compare with the Excel sheet. Looks good!



coln_search_word <- function(column) {
  switch (column,
  expedition = "expedition",
  sample_name = "name",
  station = "station",
  latitude = c("latitude", "decimal"),
  longitude = "longitude decimals",
  bottom_depth = c("bottom", "depth"),
  date = "date",
  gear = "gear",
  from = "from",
  to = "to",
  filtered_volume = "volume",
  type = "type",
  responsible = "person",
  comment = "comment",
  stop(paste(fn, "column type not set"))
)
}



sapply(required_cols, function(k) {
  agrep(coln_search_word(k), colnames(dt), value = TRUE)
})

c("sample.name.col", "station.col", "latitude.col", "longitude.col", "bottom.depth.col", "date.col", "gear.col", "from.col", "to.col", "filtered.vol.col", "type.col", "responsible.col", "comment.col")

column <- "sample.name.col"
df <- dt

colnames(df)
x <- gsub(".col", "", column)



grep(x, colnames(df), ignore.case = TRUE)



if(guess.colnames) {


}

?read.xlsx
