## Load Library
source("code/0-packages.R")
## Load CSv from REF DB
REFDB <- read.csv("out/plandataREF2016.csv")

## Adding column category with value Refugee
REFDB$Category <- "Refugee"

## Load CSv from REF DB
RESDB <- read.csv("out/plandataRES2016.csv")

## Adding column category with value Resilience
RESDB$Category <- "Resilience"

## Merging Data frames
mergeDB <- rbind(REFDB, RESDB)
write.csv(mergeDB, file = "out/plan2016Merge.csv",na="")
rm(list =ls())