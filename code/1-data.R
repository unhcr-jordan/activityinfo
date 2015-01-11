#####################################################
#  ActivityInfo  analysis script          #
#####################################################

## Log in plus some helper function
source("code/activityinfo.R")


## Dump RRP 6 plan Review in a dataframe
source("code/extract/1-rrp6-review.R")

## Dump Refugee Response Monitoring in a dataframe
source("code/extract/1-data-monitor.R")

## Dump 3 RP plan in a dataframe
source("code/extract/1-data-3rp.R")

## Dump Refugee Services Mapping in a dataframe
source("code/extract/1-data-services.R")