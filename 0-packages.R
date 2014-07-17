

## install the packages that are not yet instaled

packages <- c("ggplot2", 
             # "xlsx",  
              "rjson", "RCurl", "reshape")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

#library(xlsx)
library(reshape)
library(rjson)
library(RCurl)

## Activity Info R package
install.packages("devtools")
library(devtools)
install_github('bedatadriven/activityinfo-R')
library(activityinfo)
