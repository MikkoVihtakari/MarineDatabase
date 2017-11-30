## MarineDatabase test & development

source("runfirst.R")

data("stn_data")

library(oce)

ctd.data <- read.ctd(list.files(system.file('extdata', package = 'MarineDatabase'), full.names = TRUE))

##

X <- make_station_data(dt = stn_data, ctds = ctd.data)
X$`20170727_TWICE_Kb5`$data


library(data.tree)

stn <- Node$new("Station ID")
  meta <- stn$AddChild("Meta-data")
    lon <- meta$AddChild("Longitude")
    lat <- meta$AddChild("Latitude")
    date <- meta$AddChild("Date-time")
    depth <- meta$AddChild("Bottom depth")


# Station ID is proposed in following format `YYYYMMDD_<Expedition name>_<Station name>_<Replicate number>`. Words between `< >` are unicode strings without whitespace. Expedition and station name are included in the station ID and do not need to be repeated in meta-data. Replicate number is intended for cases when same station was taken several times during a day on an expedition.


stn <- Node$new("Station ID")
  meta <- stn$AddChild("Meta-data")
  dat <- stn$AddChild("Data")
    CTD <- dat$AddChild("CTD data")
      CTD1 <- CTD$AddChild("CTD 1")
        CTD1.meta <- CTD1$AddChild("Meta-data")
        CTD1.data <- CTD1$AddChild("Data")
      CTD2 <- CTD$AddChild("CTD 2")
        CTD2.meta <- CTD2$AddChild("Meta-data")
        CTD2.data <- CTD2$AddChild("Data")
    type1 <- dat$AddChild("Type 1")
      type1.meta <- type1$AddChild("Meta-data")
      type1.data <- type1$AddChild("Data")
    type2 <- dat$AddChild("Type 2")
      type2.meta <- type2$AddChild("Meta-data")
      type2.data <- type2$AddChild("Data")

print(stn)

##  Sample types table

library(knitr)
library(kableExtra)

data("sample_types")

kable(TYPES, "html") %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

