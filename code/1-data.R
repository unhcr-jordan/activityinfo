#####################################################
#  ActivityInfo  analysis script          #
#####################################################

## Log in plus some helper function for the activityinfo R module
source("code/0-activityinfo.R")


## Dump RRP 6 plan Review in a dataframe
source("code/extract/1-data-rrp6-review.R")
## db.1272.rrp6review

## Dump Refugee Response Monitoring in a dataframe
source("code/extract/1-data-monitor.R")
# db.1064.monitor

## Dump 3 RP plan in a dataframe
source("code/extract/1-data-3rp.R")
# db.1662.3rp

## Dump Refugee Services Mapping in a dataframe
source("code/extract/1-data-services.R")
# db.1100.services