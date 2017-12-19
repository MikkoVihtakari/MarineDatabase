### Dataset exporting

source("runfirst.R")

library(openxlsx)

## Gear type test

gear_test <- read.xlsx("devel/Type source files/GEAR_types.xlsx", 3)

gear_test <- gear_test$Arg 

save(gear_test, file = "devel/gear_test.rda")

## Sample types

TYPES <- read.xlsx("devel/Type source files/Sample_types.xlsx", 1)
TYPES <- TYPES[c("code", "sample_type", "gear_type", "parameters", "unit")]

save(TYPES, file = "data/sample_types.rda")

GEAR <- read.xlsx("devel/Type source files/GEAR_types.xlsx", 1)

save(GEAR, file = "data/gear_types.rda")
