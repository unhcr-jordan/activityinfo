

## install the packages that are not yet instaled

packages <- c("ggplot2", 
             # "xlsx",  
              "rjson", "RCurl", "reshape2")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

#library(xlsx)
library(reshape2)
library(rjson)
library(RCurl)




# Function that will sum values even if we have NA
psum <- function(..., na.rm=FALSE) {
  x <- list(...)
  rowSums(matrix(unlist(x), ncol=length(x)), na.rm=na.rm)
}

