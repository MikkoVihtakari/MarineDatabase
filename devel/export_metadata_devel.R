
source("runfirst.R")
library("openxlsx")
library("MarineDatabase")

x <- export_metadata(paste0(devel, "Samplelog MOSJ 2015.xlsx"), guess_colnames = TRUE)

x <- export_metadata(paste0(twice, "GlacierFront_2017_Samplelog_20171211.xlsx"), sheet = "SAMPLELOG", guess_colnames = TRUE)


 ## Only one origin date. Nice!

 # Compare with the Excel sheet. Looks good!




#library(stringdist)

ClosestMatch2 = function(string, stringVector){

  stringVector[amatch(string, stringVector, maxDist=Inf)]

}
k <- "to"
colnames(dt)[amatch(coln_search_word(k), tolower(colnames(dt)), maxDist=Inf, method = "lv")]

sapply(required_cols, function(k) {
  ClosestMatch2(coln_search_word(k), tolower(colnames(dt)))
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
