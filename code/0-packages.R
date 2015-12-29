

## install the packages that are not yet instaled

packages <- c("ggplot2",
              "httr",
             # "xlsx",  
              "rjson", "RCurl", "reshape2","plyr", "data.table")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

rm(packages)

#library(xlsx)
library(reshape2)
library(rjson)
library(RCurl)
library(plyr)
library(data.table)

library(httr)  # required to "warm-up" the beta API for ActivityInfo.org


# Function that will sum values even if we have NA
psum <- function(..., na.rm=FALSE) {
  x <- list(...)
  rowSums(matrix(unlist(x), ncol=length(x)), na.rm=na.rm)
}

