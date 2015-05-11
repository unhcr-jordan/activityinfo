########## Script to match achivement to Target between planning & Monitoring database

###################################
### Step 1: Doing the aggregation of Monitoring figures


## We load the mapping table 
monitor2plan <- read.csv("data/config/monitor2plan2015.csv")

## We load dump from from monitor  
source("code/extract/1-data-monitor2015.R")
## Extract done at Line 957
monitor <- read.csv("out/monitordata.csv")


########################################
### Step 2: Doing the aggregation of palnning figures

## We load dump from from plan 
source("code/extract/1-data-monitor2015.R")
## Extract done at Line 212
plan <- read.csv("out/plandata.csv")



########################################
### Step 3: We merge the reformatted plan and monitor

## Mergin is done using indicator ID, Partner ID and location

### Various business rules needs to be taken in consideration

##  Fopr partner, if implementation through, the merge should be done with target of the partners who appealed with an unidrect implementation
## If locations can not be matched, aggregation shoudl be done at the closet level in the hierachic geographic three
