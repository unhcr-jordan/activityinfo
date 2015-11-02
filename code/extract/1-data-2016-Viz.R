## Load Library
source("code/0-packages.R")
## Load Viz CSv from REF DB
REFDB <- read.csv("out/plandataREF2016.csv")
## Adding column category with value Refugee
REFDB$Category <- "Refugee"
## Load Viz CSv from REF DB
RESDB <- read.csv("out/plandataRES2016.csv")
## Adding column category with value Resilience
RESDB$Category <- "Resilience"
## Merging Data frames
mergeDB <- rbind(REFDB, RESDB)
mergeDB.3rpVis <- subset(mergeDB, select = c(Category,governorate,activityCategory,indicatorName, activityName,partnerName,locationName,refugee.camps,month, value, objective, Sector,activity2,Implementation,RegionCODE,Area2,units))
setnames(mergeDB.3rpVis, old=c("Category","governorate","activityCategory","indicatorName", "activityName","partnerName","locationName","refugee.camps","month", "value", "objective", "Sector","activity2", "Implementation", "RegionCODE","Area2","units"), new=c("Category","Governorate", "sector","indicatorName","activity","Partner","Area","Refugee.Camps","End","Total","Objective","Sector","Output","Implementation","RegionCODE","Area2","units"))
## Subset of Data for Visualization
budget <- subset(mergeDB.3rpVis, indicatorName=="Budget Requirement for 2016")
#VizSubset <- subset(budget, select = c(Category, Partner,Area,Objective,output,Sector,Implementation,Total, RegionCODE,Area2))
VizSubset <- subset(budget, select = c(Category, Partner,Area,sector,activity,Sector,Implementation,Total, RegionCODE,Area2))
setnames (VizSubset, old=c("Category", "Partner","Area","sector","activity","Sector","Implementation","Total", "RegionCODE","Area2"), new=c("Category", "Partner","Area","Objective","Output","Sector","Implementation","Total", "RegionCODE","Area2"))
write.csv(VizSubset, file = "out/3rpviz.csv",na="")
rm(list =ls())