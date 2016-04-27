
# Uncomment if needed or to update the package

## Activity Info R package
# Install packages if not present
if(!("devtools" %in% installed.packages())) {
  install.packages("devtools")
}
# Ensure that a recent version of the 'activityinfo' package is installed:
if(!"activityinfo" %in% installed.packages() || packageVersion("activityinfo") < "0.4.17") {
  library(devtools)
  install_github( "bedatadriven/activityinfo-R", ref = "release")
}

library(activityinfo)


### ActivityInfo Login

# required packages:
require("reshape2")

# authenticate
activityInfoLogin()

# a helper function to extract all attributes from an activity:
## added multipleAllowed for future cast
extractAttributes <- function(activity, includeMultiple = FALSE) {
  extractAttributes <- function(attr) {
    do.call(rbind, lapply(attr, function (attr) {
      data.frame(id = attr$id, name = attr$name, stringsAsFactors = FALSE)
    }))
  }
  attributes <-
    do.call(rbind,
            lapply(activity$attributeGroups, function(group) {
              if (group$multipleAllowed & includeMultiple | !group$multipleAllowed) {
                attributes <- extractAttributes(group$attributes)                
                attributes$multipleAllowed <- rep(group$multipleAllowed, times = nrow(attributes))
                attributes$group <- rep(group$name, times = nrow(attributes))
                return(attributes)
              } else {
                NULL
              }
            }))
  attributes
}


sanitizeNames <- function(s) {
  # convert strings to a format that's suitable for use as name
  gsub("\\s|-|_", ".", tolower(s))
}


extractAdminLevelEntities <- function(loc) {
  # helper function to extract a admin entities from a location. The return 
  # value is a vector with entity names and the names of the elements are the
  # names of the columns in 'value' where these entity names need to be stored.
  entities <- sapply(loc$adminEntities, function(e) e$name)
  ii <- match(names(entities), admin.levels.table$id, nomatch = 0)
  names(entities) <- admin.levels.table$column[ii]
  entities
}


#' asIndicatorDataFrame
#'
#' Creates a data.frame containing a list of indicators
#' and their properties from the given database schema
#'

asIndicatorDataFrame <- function(databaseSchema) {
  tables <- lapply(databaseSchema$activities, function(activity) {
    indicators <- activity$indicators
    data.frame(
      databaseId= rep(databaseSchema$id, length.out=length(indicators)),
      activityId = rep(activity$id, length.out=length(indicators)),
      indicatorId = extractField(indicators, "id"),
      indicatorName = extractField(indicators, "name"),
      indicatorCategory = extractField(indicators, "category"),
      indicatorCode = extractField(indicators, "code"),
      aggregation = extractField(indicators, "aggregation"),
      units = extractField(indicators, "units"),
      mandatory = extractField(indicators, "mandatory"),
      listHeader = extractField(indicators, "listHeader"),
      stringsAsFactors = FALSE)
  })
  do.call("rbind", tables)
}


# helper functions to make other functions to extract
# the data from the list for us
extractField <- function(sites, fieldName)
  sapply(sites, function(site) {
    x <- site[[fieldName]]
    if(!is.null(x) && is.atomic(x)) {
      x
    } else {
      NA
    }
  })



