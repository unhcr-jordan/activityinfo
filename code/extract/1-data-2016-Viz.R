
## Load Viz CSv from REF DB
REFDB <- read.csv("out/plandataREF2016Viz.csv")
## Adding column category with value Refugee
REFDB$Category <- "Refugee"
## Load Viz CSv from REF DB
RESDB <- read.csv("out/plandataRES2016Viz.csv")
## Adding column category with value Resilience
RESDB$Category <- "Resilience"
## Merging Data frames
mergeDB <- rbind(REFDB, RESDB)
## Subset of Data for Visualization
budget <- subset(mergeDB, units=="USD $"  | units=="$")
VizSubset <- subset(budget, select = c(Category, Partner,Area,Objective,Output,Sector,Implementation,Total, RegionCODE,Area2))
write.csv(VizSubset, file = "out/3rpviz.csv",na="")
