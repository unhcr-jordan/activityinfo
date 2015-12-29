
#####################################################
#  ActivityInfo Monitoring analysis script          #
#####################################################

#  This script retrieves a selection of information for all activities in a
#  database which have reporting frequency "once".

# authenticate
#activityInfoLogin()

# Uncomment when you run for the first time during yout session
source("code/0-activityinfo.R")
#
source("code/0-packages.R")

### JOR Monitoring Database Jordan db 1064
database.id <- 2300

# Set the following to 'TRUE' if you also want to export the comment field of each site:
include.comments <- TRUE

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
    paste("a", form, sep = "")
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

  cat("Retrieving site identifiers...\n")
  tryCatch(ids <- queryForm(formTree, queryParams = list(siteId = "_id")),
           error = function(e) {
             cat("Error: failed to retrieve the site identifiers for form ", activity$id,
                 ", skipping...\n", sep = "")
             success <<- FALSE
           },
           finally = if (!success) next)
  # merge the identifiers with the reports:
  reports <- mapply(c, reports, ids, SIMPLIFY = FALSE)

  if (include.comments) {
    cat("Retrieving comments...\n")
    success <- TRUE
    tryCatch(comments <- queryForm(formTree, queryParams = list(comment = "comments")),
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
      partnerLabel <- report$site.partner.label
      locationLabel <- report$site.location.label
    } else {
      partnerLabel <- report$partner.label
      locationLabel <- report$location.label
    }
    partnerId <- partners$oldId[match(partnerLabel, partners$name)]
    locationId <- locations$oldId[match(locationLabel, locations$name)]
    
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
    values$siteId       <- extractOldId(report$siteId)
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

values$database <- database.id
values$databaseName <- schema$name

values.unique.attribute <- values

# Create a separate column for each attribute (single or multiple selection)
# and copy the actual value where it is selected:
attribute.columns <- grep("^X\\d", names(values), value = TRUE)
for (col in attribute.columns) {
    choices <- setdiff(unique(values.unique.attribute[[col]]), NA)
    for (option in choices) {
        values.unique.attribute[[option]] <-
            ifelse(values.unique.attribute[[col]] == option, option, NA_character_)
    }
}

# Rename the attribute columns (from sanitized names to the actual attribute group names):
values.unique.attribute <- rename(values.unique.attribute,
       replace = c("X1.Location.type" = "1-Location type",
                   "X2.3RP.Implementation.Type" = "2-3RP Implementation Type",
                   "X3.3RP.appeal.through" = "3-3RP appeal through",
                   "X4.Allocation.according.to.3RP" = "4-Allocation according to 3RP"))

values.unique.attribute$objective <- substr(values.unique.attribute$activityCategory , (regexpr("]", values.unique.attribute$activityCategory , ignore.case=FALSE, fixed=TRUE))+1,50)
values.unique.attribute$sector <- substr(values.unique.attribute$activityCategory ,1, (regexpr("[", values.unique.attribute$activityCategory , ignore.case=FALSE, fixed=TRUE))-1)

values.unique.attribute$sector[values.unique.attribute$sector=="EDU"] <-"EDUCATION"
values.unique.attribute$sector[values.unique.attribute$sector=="FOOD/LIV"] <-"FOOD/LIVELIHOOD"
values.unique.attribute$sector[values.unique.attribute$sector=="PROT"] <-"PROTECTION"
values.unique.attribute$sector[values.unique.attribute$sector=="SHLT"] <-"SHELTER"
values.unique.attribute$sector[values.unique.attribute$sector=="HLTH"] <-"HEALTH"
#unique(values.unique.attribute$sector)

values.unique.attribute$Category <- substr(values.unique.attribute$activityName ,(regexpr("[", values.unique.attribute$activityName , ignore.case=FALSE, fixed=TRUE))+1, (regexpr("]", values.unique.attribute$activityName , ignore.case=FALSE, fixed=TRUE))-4)
values.unique.attribute$Category[values.unique.attribute$Category=="RES "] <- "Resilience"
values.unique.attribute$Category[values.unique.attribute$Category=="RES"] <- "Resilience"
values.unique.attribute$Category[values.unique.attribute$Category=="REF"] <- "Refugee"
values.unique.attribute$Category <- as.factor(values.unique.attribute$Category)
levels(values.unique.attribute$Category)

values.unique.attribute$activity2 <- substr(values.unique.attribute$activityName , (regexpr("]", values.unique.attribute$activityName , ignore.case=FALSE, fixed=TRUE))+1,50)


db.2300.monitor <- values.unique.attribute

#################################################################################################
###  merge with the right code for the map
regionactivityinfo <- read.csv("data/regionactivityinfocode.csv")


values.unique.attribute <- merge(x=values.unique.attribute, y=regionactivityinfo, by="governorate", all.x=TRUE)

# Distinguish camps
values.unique.attribute$gcode <- as.character(values.unique.attribute$gcode)
values.unique.attribute$rcode <- as.character(values.unique.attribute$rcode)
values.unique.attribute$gov <- as.character(values.unique.attribute$gov)
values.unique.attribute$region <- as.character(values.unique.attribute$region.y)

values.unique.attribute$rcode[values.unique.attribute$locationName=="Camps"] <- "5"
values.unique.attribute$gcode[values.unique.attribute$locationName=="Camps"] <- "2"
values.unique.attribute$gov[values.unique.attribute$locationName=="Camps"] <- "Camps"
values.unique.attribute$region[values.unique.attribute$locationName=="Camps"] <- "2"

values.unique.attribute$rcode[values.unique.attribute$locationName=="Camps"] <- "5"
values.unique.attribute$gcode[values.unique.attribute$locationName=="Camps"] <- "2"
values.unique.attribute$gov[values.unique.attribute$locationName=="Camps"] <- "Camps"
values.unique.attribute$region[values.unique.attribute$locationName=="Camps"] <- "2"

#Camp Names ZaatariCamp
values.unique.attribute$gov[values.unique.attribute$locationName=="Zaatari District 9"] <- "ZAATARICAMP"
values.unique.attribute$gov[values.unique.attribute$locationName=="Zaatari District 6"] <- "ZAATARICAMP"
values.unique.attribute$gov[values.unique.attribute$locationName=="Zaatari District 5"] <- "ZAATARICAMP"
values.unique.attribute$gov[values.unique.attribute$locationName=="Zaatari District 3"] <- "ZAATARICAMP"
values.unique.attribute$gov[values.unique.attribute$locationName=="Zaatari District 10"] <- "ZAATARICAMP"
values.unique.attribute$gov[values.unique.attribute$locationName=="Zaatari District 2"] <- "ZAATARICAMP"
values.unique.attribute$gov[values.unique.attribute$locationName=="Zaatari District 4"] <- "ZAATARICAMP"
values.unique.attribute$gov[values.unique.attribute$locationName=="Zaatari District 11"] <- "ZAATARICAMP"
values.unique.attribute$gov[values.unique.attribute$locationName=="Zaatari District 7"] <- "ZAATARICAMP"
values.unique.attribute$gov[values.unique.attribute$locationName=="Zaatari District 12"] <- "ZAATARICAMP"
values.unique.attribute$gov[values.unique.attribute$locationName=="Zaatari District 1"] <- "ZAATARICAMP"
values.unique.attribute$gov[values.unique.attribute$locationName=="Zaatari District 8"] <- "ZAATARICAMP"
values.unique.attribute$gov[values.unique.attribute$locationName=="Zaatari Camp (all district)"] <- "ZAATARICAMP"


#Camp Names AzraqCamp
values.unique.attribute$gov[values.unique.attribute$locationName=="Azraq Camp Village 1"] <- "AZRAQCAMP"
values.unique.attribute$gov[values.unique.attribute$locationName=="Azraq Camp Village 6"] <- "AZRAQCAMP"
values.unique.attribute$gov[values.unique.attribute$locationName=="Azraq Camp Village 2"] <- "AZRAQCAMP"
values.unique.attribute$gov[values.unique.attribute$locationName=="Azraq Camp"] <- "AZRAQCAMP"
values.unique.attribute$gov[values.unique.attribute$locationName=="Azraq Camp Village 3"] <- "AZRAQCAMP"

#Camp Names EJC

values.unique.attribute$gov[values.unique.attribute$locationName=="EJ Camp"] <- "Emirati Jordanian Camp (EJC)"




#unique(values.unique.attribute$gov)
#unique(values.unique.attribute$locationName)

# Distinguish Country wide intervention
values.unique.attribute$rcode[values.unique.attribute$locationName=="Country Wide"] <- "3"
values.unique.attribute$gcode[values.unique.attribute$locationName=="Country Wide"] <- "1"
values.unique.attribute$gov[values.unique.attribute$locationName=="Country Wide"] <- "Countrywide"
values.unique.attribute$region[values.unique.attribute$locationName=="Country Wide"] <- "Countrywide"

values.unique.attribute$rcode[is.na(values.unique.attribute$locationName)] <- "3"
values.unique.attribute$gcode[is.na(values.unique.attribute$locationName)] <- "1"
values.unique.attribute$gov[is.na(values.unique.attribute$locationName)] <- "Countrywide"
values.unique.attribute$region[is.na(values.unique.attribute$locationName)] <- "Countrywide"


#################################################################################################
###  Convert month in full date format
values.unique.attribute$startDate <- as.Date(paste(values.unique.attribute$month,"-01",sep=""),"%Y-%m-%d" )
values.unique.attribute$startDate <- format(values.unique.attribute$startDate, "%d/%m/%Y")

values.unique.attribute$endDate <- as.Date(paste(values.unique.attribute$month,"-01",sep=""),"%Y-%m-%d" )
values.unique.attribute$endDate <- format(values.unique.attribute$endDate, "%d/%m/%Y")

#################################################################################################
###  Selection of indicators that have gender disaggregation


#values.unique.attribute$indicatorName <- as.factor(values.unique.attribute$indicatorName)
#levels(values.unique.attribute$indicatorName)


#################################################################################################
### Merge site type into one through concatenation

values.unique.attribute <- rename (values.unique.attribute, c("Informal Tented Settlement" = "ITS" ,  "Urban areas"="Urban"))

## Replace NA with NULL -- df[is.na(df)] <- " "
values.unique.attribute$Camp[is.na(values.unique.attribute$Camp)] <- " "
values.unique.attribute$ITS[is.na(values.unique.attribute$ITS)] <- " " 
values.unique.attribute$Other[is.na(values.unique.attribute$Other)] <- " "
values.unique.attribute$Urban[is.na(values.unique.attribute$Urban)] <- " "

values.unique.attribute$sitetype <- paste0(values.unique.attribute$Camp, values.unique.attribute$ITS, 
                                           values.unique.attribute$Other ,  values.unique.attribute$Urban, sep=" - ")                
#names(values.unique.attribute)



##Let's summarise the indicators that are disaggregated
## If the indicator contain the string - then 3 variable are filled
# 1. Gender
# 2. PopulationType
# 3. Aggregated indicator name
### a bit of cleaning might be needed on the Aggregated indicator name

values.unique.attribute$gender <- ""
values.unique.attribute$poptype <- ""
values.unique.attribute$indic <- ""

values.unique.attribute.bcp <- values.unique.attribute


#values.unique.attribute <- values.unique.attribute.bcp

#### Assigning Gender/Indicator/poptype -  for # caregivers/ mothers reached with IYCF services


values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("# caregivers/ mothers reached with IYCF services", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Women"),values.unique.attribute$gender )
)

values.unique.attribute$poptype <- with(values.unique.attribute,
                                        
                                        ifelse(grepl("Urban", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$sitetype) &
                                                 grepl("# caregivers/ mothers reached with IYCF services", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) ,                                        
                                               paste0("Urban"), values.unique.attribute$poptype)
)

values.unique.attribute$poptype <- with(values.unique.attribute,
                                        
                                        ifelse(grepl("Camp", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$sitetype) &
                                                 grepl("# caregivers/ mothers reached with IYCF services", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) ,                                        
                                               paste0("Camp"), values.unique.attribute$poptype)
)



#################Urban/Rural Syrian Women (Age 18 and above)
values.unique.attribute$gender <- with(values.unique.attribute,
                                     ifelse(grepl("Urban/Rural Syrian Women", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                            paste0("Women"),values.unique.attribute$gender )
                                      )
values.unique.attribute$poptype <- with(values.unique.attribute,
                                       ifelse(grepl("Urban/Rural Syrian Women", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Urban"), values.unique.attribute$poptype)
                                      )
values.unique.attribute$indic <- with(values.unique.attribute,
                                       ifelse(grepl("Urban/Rural Syrian Women", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                              grepl("Age 18 and above", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName)  ,
                                              paste0(
                                                substr(values.unique.attribute$indicatorName ,
                                                       (regexpr("Urban/Rural Syrian Women (Age 18 and above)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+44,250)
                                                ), values.unique.attribute$indic)
                                      )

values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Urban/Rural Syrian Women", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                             grepl("Age 18-24", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) ,
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Urban/Rural Syrian Women (Age 18-24)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+37,250)
                                             ), values.unique.attribute$indic)
)

#################Camps Syrian Women (Age 18 and above)
values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Camps Syrian Women", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Women"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Camps Syrian Women", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Camp"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Camps Syrian Women", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 18 and above", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Camps Syrian Women (Age 18 and above)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+38,250)
                                             ), values.unique.attribute$indic)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Camps Syrian Women", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 18-24", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Camps Syrian Women (Age 18-24)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+31,250)
                                             ), values.unique.attribute$indic)
)



#################Urban/Rural Syrian Men (Age 18 and Above)
values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Urban/Rural Syrian Men", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Men"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Urban/Rural Syrian Men", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Urban"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Urban/Rural Syrian Men", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 18 and Above", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Urban/Rural Syrian Men (Age 18 and Above)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+44,250)
                                             ), values.unique.attribute$indic)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Urban/Rural Syrian Men", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 18-24", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Urban/Rural Syrian Men (Age 18-24)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+37,250)
                                             ), values.unique.attribute$indic)
)

#################Camps Syrian Men (Age 18 and above)
values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Camps Syrian Men", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Men"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Camps Syrian Men", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Camp"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Camps Syrian Men", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 18 and above", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Camps Syrian Men (Age 18 and above)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+36,250)
                                             ), values.unique.attribute$indic)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Camps Syrian Men", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 18-24", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Camps Syrian Men (Age 18-24)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+29,250)
                                             ), values.unique.attribute$indic)
)

#################Urban/Rural Syrian Girls (Age 0-17)
values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Urban/Rural Syrian Girls", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Girls"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Urban/Rural Syrian Girls", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Urban"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Urban/Rural Syrian Girls", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 0-17", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Urban/Rural Syrian Girls (Age 0-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+36,250)
                                             ), values.unique.attribute$indic)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Urban/Rural Syrian Girls", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 15-17", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Urban/Rural Syrian Girls (Age 15-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+37,250)
                                             ), values.unique.attribute$indic)
)

#################Camps Syrian Girls (Age 0-17)
values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Camps Syrian Girls", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Girls"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Camps Syrian Girls", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Camp"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Camps Syrian Girls", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 0-17", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Camps Syrian Girls (Age 0-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+30,250)
                                             ), values.unique.attribute$indic)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Camps Syrian Girls", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 15-17", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Camps Syrian Girls (Age 15-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+31,250)
                                             ), values.unique.attribute$indic)
)


#################Existing Camps Syrian Boys (Age 0-17)
values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Existing Camps Syrian Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Boys"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Existing Camps Syrian Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Urban"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Existing Camps Syrian Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 0-17", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Existing Camps Syrian Boys (Age 0-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+37,250)
                                             ), values.unique.attribute$indic)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Existing Camps Syrian Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 0-17", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Existing Camps Syrian Boys (Age 0-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+38,250)
                                             ), values.unique.attribute$indic)
)

##Newly Camps Syrian Boys (Age 0-17)

#################Newly Camps Syrian Boys(Age 0-17)
values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Newly Camps Syrian Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Boys"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Newly Camps Syrian Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Urban"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Newly Camps Syrian Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 0-17", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Newly Camps Syrian Boys (Age 0-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+34,250)
                                             ), values.unique.attribute$indic)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Newly Camps Syrian Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 0-17", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Newly Camps Syrian Boys (Age 0-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+35,250)
                                             ), values.unique.attribute$indic)
)

#################Urban/Rural Syrian Boys (Age 0-17)
values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Urban/Rural Syrian Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Boys"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Urban/Rural Syrian Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Urban"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Urban/Rural Syrian Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 0-17", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Urban/Rural Syrian Boys (Age 0-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+35,250)
                                             ), values.unique.attribute$indic)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Urban/Rural Syrian Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 15-17", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Urban/Rural Syrian Boys (Age 15-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+36,250)
                                             ), values.unique.attribute$indic)
)


#################Camps Syrian Boys (Age 0-17)
values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Camps Syrian Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Boys"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Camps Syrian Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Camp"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Camps Syrian Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 0-17", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Camps Syrian Boys (Age 0-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+29,250)
                                             ), values.unique.attribute$indic)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Camps Syrian Boys (Age 15-17)", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 18-24", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Camps Syrian Boys (Age 15-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+30,250)
                                             ), values.unique.attribute$indic)
)


#################Host Community Men (Age 18 and above) Host Community Men (Age 18 and above)
values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Host Community Men", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Men"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Host Community Men", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Host Community"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Host Community Men", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 18 and above", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Host Community Men (Age 18 and above)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+38,250)
                                             ), values.unique.attribute$indic)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Host Community Men", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 18-24", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Host Community Men (Age 18-24)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+31,250)
                                             ), values.unique.attribute$indic)
)


#################Host Community Girls (Age 0-17)
values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Host Community Girls", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Girls"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Host Community Girls", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Host Community"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Host Community Girls", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 0-17", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Host Community Girls (Age 0-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+32,250)
                                             ), values.unique.attribute$indic)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Host Community Girls", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 15-17", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Host Community Girls (Age 15-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+33,250)
                                             ), values.unique.attribute$indic)
)


#################Host Community Boys (Age 0-17)
values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Host Community Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Boys"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Host Community Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Host Community"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Host Community Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 0-17", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Host Community Boys (Age 0-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+31,250)
                                             ), values.unique.attribute$indic)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Host Community Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 15-17", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Host Community Boys (Age 15-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+32,250)
                                             ), values.unique.attribute$indic)
)


#################Host Community Women (Age 18 and above)
values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Host Community Women", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Women"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Host Community Women", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Host Community"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Host Community Women", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 18 and above", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Host Community Women (Age 18 and above)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+40,250)
                                             ), values.unique.attribute$indic)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Host Community Women", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName) &
                                               grepl("Age 18-24", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Host Community Women (Age 18-24)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+33,250)
                                             ), values.unique.attribute$indic)
)


#################Host Community Men # active community health volunteers

values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Host Community Men # active community health volunteers", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Men"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Host Community Men # active community health volunteers", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Host Community"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Host Community Men # active community health volunteers", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Host Community Men # active community health volunteers", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+19,250)
                                             ), values.unique.attribute$indic)
)



#################Host Community Women # active community health volunteers

values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Host Community Women # active community health volunteers", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Men"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Host Community Women # active community health volunteers", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Host Community"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Host Community Women # active community health volunteers", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Host Community Women # active community health volunteers", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+21,250)
                                             ), values.unique.attribute$indic)
)



#################Host Community Men # of trainings provided to CHVs

values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Host Community Men # of trainings provided to CHVs", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Men"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Host Community Men # of trainings provided to CHVs", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Host Community"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Host Community Men # of trainings provided to CHVs", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Host Community Men # of trainings provided to CHVs", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+19,250)
                                             ), values.unique.attribute$indic)
)



#################Host Community Women # of trainings provided to CHVs

values.unique.attribute$gender <- with(values.unique.attribute,
                                       ifelse(grepl("Host Community Women # of trainings provided to CHVs", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0("Women"),values.unique.attribute$gender )
)
values.unique.attribute$poptype <- with(values.unique.attribute,
                                        ifelse(grepl("Host Community Women # of trainings provided to CHVs", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                               paste0("Host Community"), values.unique.attribute$poptype)
)
values.unique.attribute$indic <- with(values.unique.attribute,
                                      ifelse(grepl("Host Community Women # of trainings provided to CHVs", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Host Community Women # of trainings provided to CHVs", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+21,250)
                                             ), values.unique.attribute$indic)
)



#### Copy the activityname for indicator with ref to benef --

values.unique.attribute$new2 <- with(values.unique.attribute,
                                    ifelse((is.na(values.unique.attribute$indic) || !is.na(values.unique.attribute$poptype)),
                                           paste0(values.unique.attribute$indicatorName) , values.unique.attribute$indic))



### Now some manual cleaning
values.unique.attribute$indic2 <- as.factor(values.unique.attribute$indic)
indicbreak <- as.data.frame(levels(values.unique.attribute$indic2))
indicbreak <- rename(indicbreak, c("levels(values.unique.attribute$indic2)"="old"))
indicbreak$new <- indicbreak$old
write.csv(indicbreak, file = "data/config/indicbreak2015.csv",na="")
#indicbreak <- read.csv("data/config/indicbreak2015-2.csv")

#values.unique.attribute <- merge(x=values.unique.attribute, y=indicbreak, by="indic", all.x=TRUE)



################################################
###Add indicator that are not breakdown
values.unique.attribute$new <- ""

#str(values.unique.attribute)

values.unique.attribute$new <- with(values.unique.attribute,
                                    ifelse((is.na(values.unique.attribute$indic)),
                                           paste0(values.unique.attribute$indicatorName) , values.unique.attribute$indic))



#values.unique.attribute$new[values.unique.attribute$new==""] <- as.vector(values.unique.attribute$indicatorName)
#values.unique.attribute <- within(values.unique.attribute, new[b==""] <- indicatorName[b==0])

#################################################################################################################
#################
#########
##
names(values.unique.attribute)

output <- rename (values.unique.attribute, c(
  # "siteId"= "siteid" ,
  #"startDate"= "StartDate" ,
   "endDate"=  "StartDate",
  # ""=  "Year",
  # ""=  "Month" ,
  "objective"= "Category",
  "activityName"=  "activity",
  #"indicatorName"= "Indicator",
  "indic"= "Indicator",
  "indicatorName"= "Indicator2",
  "gov"=  "Governorate" ,
  "gender"=  "Gender",
  "partnerName"=  "Partner" ,  
  "sitetype"=  "SiteType",
  "2-3RP Implementation Type"= "appeal",
  "3-3RP appeal through"=  "Fundedby",
  "4-Allocation according to 3RP"=  "allocation",
  "rcode"=  "rcode" ,
  "gcode"=  "gcode" ,
  "value"= "Value" ,
  "units"=  "Units"  ,
  "locationName"= "location",
  "region.y"= "region",
  "poptype"="poptype"))


output <- output[,c("sector","StartDate" ,"Category",
                    "activity","Indicator",
                    "Indicator2", "Governorate" ,
                    "Gender","Partner" ,   "SiteType", "appeal",
                    "Fundedby",  "allocation",  "rcode" , "gcode" ,
                    "Value" , "Units"  ,"location", "region","poptype")] 



output$Indicator <- with(values.unique.attribute,
                                        
                                        ifelse(grepl("", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  output$Indicator) &
                                                 grepl("# caregivers/ mothers reached with IYCF services", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  output$Indicator2) ,                                        
                                               paste0("# caregivers/ mothers reached with IYCF services"), output$Indicator)
)
#names(output)

#output$Indicator <- as.factor(output$Indicator)
#levels(output$Indicator)



#Changing Start Date to End date
output$StartDate[output$StartDate=="01/01/2015"] <- "30/1/2015"
output$StartDate[output$StartDate=="01/02/2015"] <- "28/2/2015"
output$StartDate[output$StartDate=="01/03/2015"] <- "30/3/2015"
output$StartDate[output$StartDate=="01/04/2015"] <- "30/4/2015"
output$StartDate[output$StartDate=="01/05/2015"] <- "30/5/2015"
output$StartDate[output$StartDate=="01/06/2015"] <- "30/6/2015"
output$StartDate[output$StartDate=="01/07/2015"] <- "30/7/2015"
output$StartDate[output$StartDate=="01/08/2015"] <- "30/8/2015"
output$StartDate[output$StartDate=="01/09/2015"] <- "30/9/2015"
output$StartDate[output$StartDate=="01/10/2015"] <- "30/10/2015"
output$StartDate[output$StartDate=="01/11/2015"] <- "30/11/2015"
output$StartDate[output$StartDate=="01/12/2015"] <- "30/12/2015"
output$Indicator[output$Indicator=="bove) # benefiting from basic learning (literacy and numeracy)"] <- "# benefiting from basic learning (literacy and numeracy)"
output$Indicator[output$Indicator=="bove) # benefiting from life skills activities"] <- "# benefiting from life skills activities"
output$Indicator[output$Indicator=="d above) # benefiting from basic learning (literacy and numeracy)"] <- "# benefiting from basic learning (literacy and numeracy)"
output$Indicator[output$Indicator=="d above) # benefiting from life skills activities"] <- "# benefiting from life skills activities"



##Tweaking for blank data. It adds All the govenorates for all the sector with 0 Value entry for Dec 2014


tweakdata <- read.csv("data/config/tweak.csv",header=T,sep=",")
output <- rbind(output,tweakdata)

## Reorder resulting dataframe

output <- output[with(output, order(activity, Indicator,location, Partner)), ]


##################################################################################
######### Writing output for Dashbaord dataviz @ https://github.com/unhcr-jordan/sectors 
#"BASIC NEEDS"     "PROTECTION"      "HEALTH"          "EDUCATION"       "WASH"            "SHELTER"         "FOOD/LIVELIHOOD"

output.education <-  subset(output, output$sector == "EDUCATION")
output.education.benef <-  subset(output.education, output.education$Indicator != "")
output.education.benef$Indicator[output.education.benef$Indicator=="From Script"] <- ""
write.csv(output.education.benef, file = "out/monitor/2015/education/data.csv",na="")
output.education.oth <-  subset(output.education, output.education$Indicator == "")
output.education.oth$Indicator <- output.education.oth$Indicator2
write.csv(output.education.oth, file = "out/monitor/2015/education/dataother.csv",na="")
OE <- rbind(output.education.oth,output.education.benef)
write.csv(OE, file = "out/monitor/2015/education/dataind.csv",na="")

output.health <-  subset(output, output$sector == "HEALTH")
output.health.benef <-  subset(output.health, output.health$Indicator != "")
output.health.benef$Indicator[output.health.benef$Indicator=="From Script"] <- ""
write.csv(output.health.benef, file = "out/monitor/2015/health/data.csv",na="")
output.health.oth <-  subset(output.health, output.health$Indicator == "")
output.health.oth$Indicator <- output.health.oth$Indicator2
write.csv(output.health.oth, file = "out/monitor/2015/health/dataother.csv",na="")
HE <- rbind(output.health.oth,output.health.benef)
write.csv(HE, file = "out/monitor/2015/health/dataind.csv",na="")


output.food <-  subset(output, output$sector == "FOOD/LIVELIHOOD")
output.food.benef <-  subset(output.food, output.food$Indicator != "")
output.food.benef$Indicator[output.food.benef$Indicator=="From Script"] <- ""
write.csv(output.food.benef, file = "out/monitor/2015/food/data.csv",na="")
output.food.oth <-  subset(output.food, output.food$Indicator == "")
output.food.oth$Indicator <- output.food.oth$Indicator2
write.csv(output.food.oth, file = "out/monitor/2015/food/dataother.csv",na="")
FE <- rbind(output.food.oth,output.food.benef)
write.csv(FE, file = "out/monitor/2015/food/dataind.csv",na="")

output.basicneeds <-  subset(output, output$sector == "BASIC NEEDS")
output.basicneedscase <- subset(output.basicneeds, output.basicneeds$Units == "# of case")
output.basicneedscase$Indicator <- output.basicneedscase$Indicator2
output.basicneeds.benef <-  subset(output.basicneeds, output.basicneeds$Indicator != "")
output.basicneeds.benef$Indicator[output.basicneeds.benef$Indicator=="From Script"] <- ""
output.basicneeds.benefFinal <- rbind(output.basicneeds.benef,output.basicneedscase)
write.csv(output.basicneeds.benefFinal, file = "out/monitor/2015/basicneeds/data.csv")
output.basicneeds.oth <-  subset(output.basicneeds, output.basicneeds$Indicator == "" & output.basicneeds$Units !="# of case")
output.basicneeds.oth$Indicator <- output.basicneeds.oth$Indicator2
write.csv(output.basicneeds.oth, file = "out/monitor/2015/basicneeds/dataother.csv",na="")
BE <- rbind(output.basicneeds.oth,output.basicneeds.benef)
write.csv(BE, file = "out/monitor/2015/basicneeds/dataind.csv",na="")


output.protection <-  subset(output, output$sector == "PROTECTION")
#  of individuals submitted for resettlement 
# of women, girls, boys and men SGBV survivors benefiting from case management services 
# of girls & boys benefiting from multi-sectoral services
# of women, girls, boys and men with specific needs receiving special support
# of women, girls, boys & men receiving legal information, counseling and/or representation 
# of women, girls, boys & men benefiting from psychosocial support services (level 2 & 3) 

output.protection.benef <-  subset(output.protection, output.protection$Indicator != "")
output.protection.benef$Indicator[output.protection.benef$Indicator=="From Script"] <- ""
write.csv(output.protection.benef, file = "out/monitor/2015/protection/data.csv",na="")
output.protection.oth <-  subset(output.protection, output.protection$Indicator == "")
output.protection.oth$Indicator <- output.protection.oth$Indicator2
write.csv(output.protection.oth, file = "out/monitor/2015/protection/dataother.csv",na="")
PE <- rbind(output.protection.oth,output.protection.benef)
write.csv(PE, file = "out/monitor/2015/protection/dataind.csv",na="")

output.shelter <-  subset(output, output$sector == "SHELTER")
# of dwelling units upgraded to minimum standards
# Increased housing units provided in unfinished buildings
# of HH receiving rental support
# of home adaptation kits distributed
# of people receiving information messaging on housing (HLP)

output.shelter.benef <-  subset(output.shelter, output.shelter$Indicator != "")
output.shelter.benef$Indicator[output.shelter.benef$Indicator=="From Script"] <- ""
write.csv(output.shelter.benef, file = "out/monitor/2015/shelter/data.csv",na="")
output.shelter.oth <-  subset(output.shelter, output.shelter$Indicator == "")
output.shelter.oth$Indicator <- output.shelter.oth$Indicator2
write.csv(output.shelter.oth, file = "out/monitor/2015/shelter/dataother.csv",na="")
SE <- rbind(output.shelter.oth,output.shelter.benef)
write.csv(SE, file = "out/monitor/2015/shelter/dataind.csv",na="")

output.wash <-  subset(output, output$sector == "WASH")
output.wash.benef <-  subset(output.wash, output.wash$Indicator != "")
output.wash.benef$Indicator[output.wash.benef$Indicator=="From Script"] <- ""
write.csv(output.wash.benef, file = "out/monitor/2015/wash/data.csv",na="")
output.wash.oth <-  subset(output.wash, output.wash$Indicator == "")
output.wash.oth$Indicator <- output.wash.oth$Indicator2
write.csv(output.wash.oth, file = "out/monitor/2015/wash/dataother.csv",na="")
WE <- rbind(output.wash.oth,output.wash.benef)
write.csv(WE, file = "out/monitor/2015/wash/dataind.csv",na="")

########################################################

### Multisectorial Dashboard - ZaatariCamp
output.zaatari <-  subset(output, output$Governorate == "ZAATARICAMP")
output.zaatari.benef <-  subset(output.zaatari, output.zaatari$Indicator != "")
output.zaatari.benef$sector[output.zaatari.benef$sector=="BASIC NEEDS"] <- "BASICNEEDS"
output.zaatari.benef$sector[output.zaatari.benef$sector=="FOOD/LIVELIHOOD"] <- "FOOD"
output.zaatari.benef$location[output.zaatari.benef$location=="Zaatari District 9"] <- "District 9"
output.zaatari.benef$location[output.zaatari.benef$location=="Zaatari District 6"] <- "District 6"
output.zaatari.benef$location[output.zaatari.benef$location=="Zaatari District 5"] <- "District 5"
output.zaatari.benef$location[output.zaatari.benef$location=="Zaatari District 3"] <- "District 3"
output.zaatari.benef$location[output.zaatari.benef$location=="Zaatari District 10"] <- "District 10"
output.zaatari.benef$location[output.zaatari.benef$location=="Zaatari District 2"] <- "District 2"
output.zaatari.benef$location[output.zaatari.benef$location=="Zaatari District 4"] <- "District 4"
output.zaatari.benef$location[output.zaatari.benef$location=="Zaatari District 11"] <- "District 11"
output.zaatari.benef$location[output.zaatari.benef$location=="Zaatari District 7"] <- "District 7"
output.zaatari.benef$location[output.zaatari.benef$location=="Zaatari District 12"] <- "District 12"
output.zaatari.benef$location[output.zaatari.benef$location=="Zaatari District 1"] <- "District 1"
output.zaatari.benef$location[output.zaatari.benef$location=="Zaatari District 8"] <- "District 8"
output.zaatari.benef$location[output.zaatari.benef$location=="Zaatari Camp (all district)"] <- "Campwide"
tweakdata1 <- read.csv("data/config/tweakzaatari.csv",header=T,sep=",")
output.zaatari.benefFinal <- rbind(output.zaatari.benef,tweakdata1)

write.csv(output.zaatari.benefFinal, file = "out/monitor/2015/zaatari/data.csv",na="")


### Multisectorial Dashboard - AzraqCamp
output.azraq <-  subset(output, output$Governorate == "AZRAQCAMP")
output.azraq.benef <-  subset(output.azraq, output.azraq$Indicator != "")
output.azraq.benef$sector[output.azraq.benef$sector=="BASIC NEEDS"] <- "BASICNEEDS"
output.azraq.benef$sector[output.azraq.benef$sector=="FOOD/LIVELIHOOD"] <- "FOOD"
output.azraq.benef$location[output.azraq.benef$location=="Azraq Camp Village 1"] <- "Village 1"
output.azraq.benef$location[output.azraq.benef$location=="Azraq Camp Village 6"] <- "Village 6"
output.azraq.benef$location[output.azraq.benef$location=="Azraq Camp Village 2"] <- "Village 2"
output.azraq.benef$location[output.azraq.benef$location=="Azraq Camp"] <- "Campwide"
output.azraq.benef$location[output.azraq.benef$location=="Azraq Camp Village 3"] <- "Village 3"
tweakdata2 <- read.csv("data/config/tweakazraq.csv",header=T,sep=",")
output.azraq.benefFinal <- rbind(output.azraq.benef,tweakdata2)
write.csv(output.azraq.benefFinal, file = "out/monitor/2015/azraq/data.csv",na="")

### Multisectorial Dashboard - Countrywide

countrywide <- rbind(output.education.benef, output.health.benef, output.food.benef, output.basicneeds.benef, output.protection.benef, output.shelter.benef, output.wash.benef)
countrywide$sector[countrywide$sector=="BASIC NEEDS"] <- "BASICNEEDS"
countrywide$sector[countrywide$sector=="FOOD/LIVELIHOOD"] <- "FOOD"
write.csv(countrywide, file = "out/monitor/2015/countrywide/data.csv",na="")


db.1064.monitor <- values.unique.attribute
write.csv(db.2300.monitor, file = "out/monitordata.csv",na="")




### Clean unused elements

rm(activities.table)
rm(admin.levels.table)
#rm(attributes.single)
rm(location.types.table)
rm(sites)
#rm(sites.wide)
rm(values)
# rm(activities.reported.once)
rm(activities.reported.monthly)
rm(admin.levels)
rm(col)
rm(column.name)
rm(country.id)
rm(database.id)
rm(id)
rm(j)
rm(location.ids)
rm(location.types)
rm(locations)
rm(rows)
rm(schema)
rm(BE)
rm(FE)
rm(HE)
rm(OE)
rm(PE)
rm(SE)
rm(WE)
rm(countrywide)
rm(db.2300.monitor)
rm(db.1064.monitor)
rm(indicbreak)
rm(output)
rm(output.azraq)
rm(output.azraq.benef)
rm(output.basicneeds)
rm(output.basicneeds.oth)
rm(output.basicneeds.benef)
rm(output.basicneeds.benefFinal)
rm(output.basicneedscase)
rm(output.education)
rm(output.education.benef)
rm(output.education.oth)
rm(output.food)
rm(output.food.benef)
rm(output.food.oth)
rm(output.health)
rm(output.health.benef)
rm(output.health.oth)
rm(output.protection)
rm(output.protection.benef)
rm(output.protection.oth)
rm(output.shelter)
rm(output.shelter.benef)
rm(output.shelter.oth)
rm(output.wash)
rm(output.wash.benef)
rm(output.wash.oth)
rm(output.zaatari)
rm(output.zaatari.benef)
rm(output.zaatari.benefFinal)
rm(regionactivityinfo)
rm(sites.attribute.multiple.wide)
rm(tweakdata)
rm(tweakdata1)
rm(tweakdata2)
rm(values.unique.attribute)
rm(values.unique.attribute.bcp)
rm(type)

rm(attributes)
rm(include.multiple.selection)
rm(sites.attribute.multiple)
rm(sites.attribute.single)
rm(sites.attribute.single.dup)
rm(sites.attribute.single.wide)
rm(sites.unique)
rm(sites.unique.attr)
rm(values.unique)
#rm(values.unique.attribute)


