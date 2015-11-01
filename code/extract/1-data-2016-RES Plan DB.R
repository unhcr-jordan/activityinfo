################################################################################
# The MIT License (MIT)
#
# Copyright (c) 2015 Maarten-Jan Kallen
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
################################################################################

opt <- options(warn = 1)
rm(list = ls())
source("code/0-packages.R")
library("activityinfo")
library("httr")

# Replace 'NA' with the numeric identifier of your database (e.g. 1234):
database.id <- 4514

# Set the following to 'TRUE' if you also want to export the comment field of each site:
include.comments <- FALSE

# Uncomment the following command if you want to log in manually, leave commented
# out if you have stored your login credentials on your local machine.

activityInfoLogin()

#-------------------------------------------------------------------------------
# Function definitions
#-------------------------------------------------------------------------------

na.if.null <- function(x) {
  if (is.null(x)) NA else x
}

sanitizeNames <- function(s) {
  # convert strings to a format that's suitable for use as name
  gsub("\\s|-|_", ".", tolower(s))
}

translateFieldType <- function(typeClass) {
  switch(toupper(typeClass),
         REFERENCE  = "reference",
         LOCAL_DATE = "date",
         QUANTITY   = "indicator",
         CALCULATED = "calculated indicator",
         ENUMERATED = "attribute",
         NARRATIVE  =,
         FREE_TEXT  = "text",
         GEOAREA    = "geographic entity",
         stop("unknown field type '", typeClass, "'"))
}

getFormElements <- function(form, tree, name.prefix = NULL) {
  
  if (is.null(form$elements)) {
    NULL
  } else {
    do.call(rbind, lapply(form$elements, function(e) {
      fieldType <- translateFieldType(e$type$typeClass)
      if (fieldType == "reference") {
        # This form refers to one or more other forms
        do.call(rbind, lapply(e$type$parameters$range, function(refform) {
          getFormElements(tree$forms[[refform]],
                          tree,
                          ifelse(is.null(name.prefix),
                                 e$code,
                                 paste(name.prefix, e$code, sep = ".")))
        }))
      } else {
        fieldName <- ifelse(is.null(e$code), e$label, e$code)
        fieldLabel <- ifelse(is.null(e$label), e$code, e$label)
        fieldType <- if (fieldType == "attribute") {
          switch(e$type$parameters$cardinality,
                 SINGLE="single attribute",
                 MULTIPLE="multiple attribute",
                 stop("unknown cardinality"))
        } else {
          fieldType
        }
        data.frame(id = e$id,
                   name = ifelse(is.null(name.prefix),
                                 fieldName,
                                 paste(name.prefix, fieldName, sep = ".")),
                   label = fieldLabel,
                   type = fieldType,
                   stringsAsFactors = FALSE
        )
      }
    }))
  }
}

getFormTree <- function(activity) {
  
  prefix <- switch(as.character(activity$reportingFrequency),
                   "0"="a",
                   "1"="M",
                   stop("reporting frequency should be 0 (once) or 1 (monthly)")
  )
  
  tree <- getResource(sprintf("form/%s%s/tree", prefix, activity$id))
  
  form <- tree$forms[[tree$root]]
  
  elements <- getFormElements(form, tree)
  
  structure(elements, class = c("formtree", class(elements)), tree = tree)
}

queryForm <- function(form, queryType = c("rows", "columns"), ...) {
  
  formId <- if (inherits(form, "formtree")) {
    # query the root form of a tree contained in a formtree result
    attr(form, "tree")$root
  } else if (is.character(form)) {
    # query using a form identifier
    form
  } else {
    # query the root of a form tree
    form$root
  }
  
  getResource(sprintf("form/%s/query/%s", formId, match.arg(queryType)), ...)
}

extractOldId <- function(s) {
  if (all(grepl("^[[:alpha:]]0*", s))) {
    as.integer(sub("^[[:alpha:]]0*", "", s))
  } else {
    s
  }
}

determineMonth <- function(start, end) {
  start <- as.POSIXlt(start)
  end <- as.POSIXlt(end)
  if (start$year != end$year || start$mon != end$mon) {
    cat("Warning: found a start and end date in different months\n")
  }
  format(start, format = "%Y-%m")
}

getPartnersDataFrame <- function(formId) {
  partners <- getResource(sprintf("form/%s/query/rows", formId), id = "_id", name = "name")
  do.call(rbind, lapply(partners, function(p) {
    data.frame(id = p$id,
               name = p$name,
               oldId = extractOldId(p$id),
               stringsAsFactors = FALSE)
  }))
}

getLocationsDataFrame <- function(formId) {
  locations <- getResource(sprintf("form/%s/query/rows", formId), id = "_id", name = "name", code = "axe")
  do.call(rbind, lapply(locations, function(p) {
    data.frame(id = p$id,
               name = p$name,
               code = na.if.null(p$code), # alternative name ("axe")
               oldId = extractOldId(p$id),
               stringsAsFactors = FALSE)
  }))
}

lookupName <- function(x, table, lookupCol = "oldId", outputCol = "name") {
  
  if (is.character(x)) return(x)
  
  tableName <- deparse(substitute(table))
  
  if(is.null(table[[lookupCol]]) || is.null(table[[outputCol]])) {
    stop("'", tableName, "' must have columns '", lookupCol, "' and '", outputCol, "'")
  }
  
  row <- match(x, table[[lookupCol]])
  if (any(is.na(row))) {
    cat("Warning: no record(s) found with (old) identifier(s) ",
        paste(x[is.na(row)], collapse = ", "), " in '", tableName,
        "'\n", sep ="")
  }
  table[[outputCol]][row]
}

extractAdminLevelEntities <- function(loc) {
  # helper function to extract a admin entities from a location. The return 
  # value is a vector with entity names and the names of the elements are the
  # names of the columns in 'value' where these entity names need to be stored.
  # Returns NULL is any admin level entity in 'loc' can not be indentified.
  
  res <- structure(.Data = rep(NA_character_, times = nrow(admin.levels.table)),
                   .Names = admin.levels.table$column)
  entities <- sapply(loc$adminEntities, function(e) e$name)
  ii <- match(names(entities), admin.levels.table$id, nomatch = 0)
  res[ii] <- entities
  as.data.frame(as.list(res), stringsAsFactors = FALSE)
}

is.monthly <- function(formTree) {
  grepl("^M\\d*$", attr(formTree, "tree")$root)
}

# Send a "curl -I" request to the beta API to warm up the server:
invisible(HEAD("https://beta-api-dot-activityinfoeu.appspot.com/login"))

#-------------------------------------------------------------------------------
# Script body
#-------------------------------------------------------------------------------

if (is.na(database.id)) {
  stop("you forgot to set the database identifier at the top of this script!")
}

# Use the new API (in beta)
activityInfoRootUrl("https://beta-api-dot-activityinfoeu.appspot.com")
#activityInfoRootUrl("https://www.activityinfo.org")

# Get the schema and retry a few times to allow the beta-api instance to warm up:
cat("Retrieving schema for database ", database.id, "...\n", sep ="")
retry <- 5
while (retry) {
  success <- TRUE
  tryCatch(schema <- getDatabaseSchema(database.id),
           error = function(e) {
             cat("Failed to retrieve the schema for database ", database.id,
                 ". Retrying...\n", sep = "")
             retry <<- retry - 1
             if (retry == 0) stop("Failed with the following error: ", e$message)
             success <<- FALSE
           },
           finally = if (success) {
             cat("Retrieved schema for database ", database.id,
                 ": ", schema$name, "\n", sep = "")
             retry <- 0
           }
  )
}

attributeGroups <- unique(
  do.call(c, lapply(schema$activities, function(form) {
    sapply(form$attributeGroups, function(group) {
      group$name
    })
  }))
)

values <- NULL
# Loop over all forms in the database:
for (formIndex in seq(length(schema$activities))) {
  
  activity <- schema$activities[[formIndex]] # "activity" is the old name for a form
  indicator.metadata <- do.call(rbind, lapply(activity$indicators, function(indicator) {
    data.frame(oldId = indicator$id,
               units = na.if.null(indicator$units),
               category = na.if.null(indicator$category),
               stringsAsFactors = FALSE)
  }))
  
  cat("Processing activity ", activity$id, " (", activity$name, ")...\n", sep = "")
  formTree <- getFormTree(activity)
  
  partnerFormId <- grep("^P\\d*$", names(attr(formTree, "tree")$forms), value = TRUE)
  cat("Retrieving partners...\n")
  partners <- getPartnersDataFrame(partnerFormId)
  
  locationFormId <- grep("^L\\d*$", names(attr(formTree, "tree")$forms), value = TRUE)
  if (length(locationFormId) == 0L) {
    cat("Warning: no locations for form ", activity$id, ", skipping...\n", sep = "")
    next
  }
  cat("Retrieving locations...\n")
  locations <- getLocationsDataFrame(locationFormId)
  
  cat("Retrieving reported values...\n")
  success <- TRUE
  tryCatch(reports <- queryForm(formTree),
           error = function(e) {
             cat("Error: failed to retrieve reported values for form ", activity$id,
                 ", skipping...\n", sep = "")
             success <<- FALSE
           },
           finally = if (!success) next)
  
  if (include.comments) {
    cat("Retrieving comments...\n")
    success <- TRUE
    tryCatch(comments <- queryForm(formTree, queryParams = list(id = "_id", comment = "comments")),
             error = function(e) {
               cat("Error: failed to retrieve comments for form ", activity$id,
                   ", skipping...\n", sep = "")
               success <<- FALSE
             },
             finally = if (!success) next)
    
    # Merge/fuse the two lists together:
    reports <- mapply(c, reports, comments, SIMPLIFY = FALSE)
  }
  
  cat("Converting values to a tabular format...\n")
  values <- rbind(values, do.call(rbind, lapply(reports, function(report) {
    if (is.monthly(formTree)) {
      if (!all(c("date1",
                 "date2",
                 "site.partner.label",
                 "site.location.label") %in% names(report)))
        stop("report is missing one of the mandatory fields")
    } else {
      if (!all(c("date1",
                 "date2",
                 "partner.label",
                 "location.label") %in% names(report)))
        stop("report is missing one of the mandatory fields")
    }
    
    # Convert report to a data frame so we can merge with the form tree:
    reportTable <- data.frame(name = names(report),
                              values = unlist(report), stringsAsFactors = FALSE)
    reportTable <- merge(reportTable, formTree, by = "name")
    
    if (is.monthly(formTree)) {
      partnerId <- extractOldId(report$site.partner.label)
      locationId <- extractOldId(report$site.location.label)
    } else {
      #       partnerId <- extractOldId(report$partner.label)
      #       locationId <- extractOldId(report$location.label)
      partnerId <- partners$oldId[match(report$partner.label, partners$name)]
      locationId <- locations$oldId[match(report$location.label, locations$name)]
    }
    is.indicator <- grepl("indicator", reportTable$type)
    n <- sum(is.indicator)
    
    if (n == 0L) {
      return(NULL)
    } else {
      oldIndicatorId <- extractOldId(reportTable$id[is.indicator])
      values <- data.frame(
        indicatorId   = oldIndicatorId,
        indicatorName = reportTable$label[is.indicator],
        units         = lookupName(oldIndicatorId, indicator.metadata, outputCol = "units"),
        indicatorCategory = lookupName(oldIndicatorId, indicator.metadata, outputCol = "category"),
        value         = as.numeric(reportTable$values[is.indicator]),
        stringsAsFactors = FALSE)
      
    }
    values$activityId   <- activity$id
    values$activityName <- activity$name
    values$activityCategory <- na.if.null(activity$category)
    values$month        <- determineMonth(report$date1, report$date2)
    if (is.monthly(formTree)) {
      values$locationId   <- locationId
      values$locationName <- lookupName(locationId, locations)
      values$locationCode <- lookupName(locationId, locations, outputCol = "code")
      values$partnerId    <- partnerId
      values$partnerName  <- lookupName(partnerId, partners)
    } else {
      values$locationId   <- locationId
      values$locationName <- report$location.label
      values$locationCode <- na.if.null(report$location.axe)
      values$partnerId    <- partnerId
      values$partnerName  <- report$partner.label
    }
    for (col in attributeGroups) {
      if (is.monthly(formTree)) {
        values[[make.names(col)]] <- na.if.null(report[[paste("site", col, sep = ".")]])
      } else {
        values[[make.names(col)]] <- na.if.null(report[[col]])
      }
    }
    if (include.comments) {
      values$comment <- na.if.null(report$comment)
    }
    values
  })))
} # end of loop over forms

# Add the full geographic tree to the data:

country.id <- schema$country$id

location.types <- getLocationTypes(country.id)
location.types.table <-
  do.call(rbind, lapply(location.types, function(type) {
    data.frame(id = type$id,
               name = type$name,
               stringsAsFactors = FALSE)
  }))

admin.levels <- getAdminLevels(country.id)
admin.levels.table <-
  do.call(rbind, lapply(admin.levels, function(level) {
    data.frame(id = level$id,
               name = level$name,
               parent.id = if(!is.null(level$parentId)) level$parentId else NA,
               stringsAsFactors = FALSE)
  }))
admin.levels.table$column <- sanitizeNames(admin.levels.table$name)

# create a table with the location types for each activity:
activity.locs <-
  do.call(rbind,
          lapply(schema$activities, function(x) {
            loc <- x$locationType
            data.frame(activityId = x$id,
                       id = loc$id,
                       name = loc$name,
                       adminLevelId = na.if.null(loc$adminLevelId),
                       stringsAsFactors = FALSE)
          }))

# at this point, the administrative levels are determined in one of two ways:
# 1) the information in the 'locations' list if the activity uses a location 
#    type which is not an admin level, or
# 2) the name of the location in the site if the activity uses admin levels as 
#    locations.

# retrieve all locations for each type present in the activities but not for
# those that are administrative levels:
locations <- list()
for (type in unique(activity.locs$id[is.na(activity.locs$adminLevelId)])) {
  cat("Getting all location entities of type", type, "\n")
  locations <- c(locations, getLocations(type))
}
# add locations of type 50517 if looking at the Education database for Lebanon:
if (database.id == 1889) {
  cat("Getting all location entities of type", 50517, "\n")
  locations <- c(locations, getLocations(50517))
}
# create a data frame with the admin levels for each location:
location.admin.levels <- do.call(rbind, lapply(locations, function(loc) {
  loc.details <- data.frame(locationId = loc$id,
                            locationx = na.if.null(loc$longitude),
                            locationy = na.if.null(loc$latitude),
                            stringsAsFactors = FALSE)
  data.frame(loc.details, extractAdminLevelEntities(loc), stringsAsFactors = FALSE)
})
)

if (is.null(location.admin.levels)) {
  # add empty columns for the admin levels:
  for (col in admin.levels.table$column) values[[col]] <- NA_character_
} else {
  # merge in the admin levels for type 1 locations:
  values <- merge(values, location.admin.levels, all.x = TRUE, by = "locationId", incomparables = NA)
}

for (activity.id in unique(activity.locs$activityId[!is.na(activity.locs$adminLevelId)])) {
  idx <- which(values$activityId == activity.id)
  row <- which(activity.locs$activityId == activity.id)
  admin.name <- sanitizeNames(activity.locs$name[row])
  if (match(admin.name, names(values), nomatch = 0)) {
    values[[admin.name]][idx] <- values$locationName[idx]
  } else {
    warning("could not set the administrative levels for activity ",
            activity.id)
  }
}

###

values$databaseId <- database.id
values$databaseName <- schema$name

cat("Done. The results are in a data frame called 'values'.\n")


###

values$databaseId <- database.id
values$databaseName <- schema$name

cat("Done. The results are in a data frame called 'values'.\n")

## Adding Columns Sector, Activity2

values$Sector <- ""
values$activity2 <- ""

## Updating Sector column

sector <- substr(values$activityCategory ,1, (regexpr("[", values$activityCategory , ignore.case=FALSE, fixed=TRUE))-1)
values$Sector <- sector
values$Sector[values$Sector=="BN"] <-"BASIC NEEDS"
values$Sector[values$Sector=="EDU"] <-"EDUCATION"
values$Sector[values$Sector=="FOOD/LIV"] <-"FOOD/LIVELIHOOD"
values$Sector[values$Sector=="PROT"] <-"PROTECTION"
values$Sector[values$Sector=="SHLT"] <-"SHELTER"
values$Sector[values$Sector=="TRAN"] <-"TRANSPORT"
values$Sector[values$Sector=="MUN"] <-"MUNICIPALITY"
values$Sector[values$Sector=="JUS"] <-"JUSTICE"
values$Sector[values$Sector=="ENV"] <-"ENVIRONMENT"
values$Sector[values$Sector=="ENG"] <-"ENERGY"
values$Sector[values$Sector=="HLTH"] <-"HEALTH"



values$activity2 <- substr(values$activityName , (regexpr("]", values$activityName , ignore.case=FALSE, fixed=TRUE))+1,50)
values$objective <- substr(values$activityCategory , (regexpr("]", values$activityCategory , ignore.case=FALSE, fixed=TRUE))+1,50)

setnames(values, old=c("Implementation..direct.indirect."), new=c("Implementation"))
location <- read.csv("data/config/loc.csv")
db.4514.3rp <- merge (x=values, y=location, by="locationName", all.x=TRUE)
db.4514.3rpVis <- subset(db.4514.3rp, select = c(governorate,activityCategory,activityName,indicatorName,partnerName,locationName,refugee.camps,month, value, objective, Sector,activity2,Implementation,RegionCODE,Area2,units))
setnames(db.4514.3rpVis, old=c("governorate","activityCategory","activityName","indicatorName","partnerName","locationName","refugee.camps","month", "value", "objective", "Sector","activity2", "Implementation", "RegionCODE","Area2","units"), new=c("Governorate", "sector","activity","indicatorName","Partner","Area","Refugee.Camps","End","Total","Objective","Sector","Output","Implementation","RegionCODE","Area2","units"))
db.4514.3rpFinal <- subset(db.4514.3rp, select = c(locationName, locationId, indicatorId, indicatorName, units, indicatorCategory, value, activityId, activityName, activityCategory, month, locationCode, partnerId, partnerName, Implementation, implementation..direct.indirect., locationx, locationy, governorate, region, district, subdistrict, refugee.camps, camp.districts, databaseId, databaseName, Sector, activity2, objective, RegionCODE, Area2))
write.csv(db.4514.3rpVis, file = "out/plandataRES2016Viz.csv",na="")
write.csv(db.4514.3rpFinal, file = "out/plandataRES2016.csv",na="")
write.csv(db.4514.3rp, file = "out/plandataRES201611.csv",na="")
rm(list =ls())