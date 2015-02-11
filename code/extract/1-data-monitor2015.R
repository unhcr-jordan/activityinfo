# required packages:
#require("activityinfo")
#require("reshape2")

#####################################################
#  ActivityInfo Monitoring analysis script          #
#####################################################

#  This script retrieves a selection of information for all activities in a
#  database which have reporting frequency "once".

# authenticate
#activityInfoLogin()

#
source("code/0-activityinfo.R")
#
source("code/0-packages.R")

### JOR Monitoring Database Jordan db 1064
database.id <- 2300

values <- getIndicatorValueTable(database.id)
# this gives us almost all the information we need. Missing are the start and 
# end date of the reporting period as well as the value of the attributes for
# each site.



#################################################################################################
### Step 1: create a lookup table for single attributes (i.e. those attributes
### that are in the activities and which allow only a single selection)

# retrieve the database schema as a list:
schema <- getDatabaseSchema(database.id)

# convert list of activities to a data frame
activities.table <-asActivityDataFrame(schema)


# extract attributes from the current database:
include.multiple.selection <- TRUE
attributes <-
  do.call(rbind, lapply(schema$activities,
                        function(activity) {
                          extractAttributes(activity, include.multiple.selection)
                        }))

### Step 2: extract start/end date and attributes from all sites:

# select the identifiers of activities that have reporting frequency "monthly":
activities.reported.monthly <-
  activities.table$activityId[activities.table$reportingFrequency == 1]

# retrieve a data frame with all sites linked to all indicators in the database
# and which contains the information missing in the 'data' object:
sites <- do.call(rbind, lapply(activities.reported.monthly, function(id) {
  cat("Getting sites for activity", id, "\n")
  sites <- getSites(id)
  do.call(rbind, lapply(sites, function(site) {
    n <- length(site$attributes)
    if (n) {
      df <- data.frame(siteId = rep(site$id, n),
                       activityId = rep(site$activity, n),
                     #  startDate = rep(site$startDate, n),
                     #  endDate = rep(site$endDate, n),
                       comments = ifelse(is.null(site$comments), "", site$comments),
                       stringsAsFactors = FALSE)
      # site$attributes is a vector with attribute identifiers. Some of these
      # may be multiple-selection attributes, which we currently ignore.
      i <- match(site$attributes, attributes$id, nomatch = 0)
      df$attributeGroup <- attributes$group[i]
      df$attributeValue <- attributes$name[i]
      df$multipleAllowed <- attributes$multipleAllowed[i]
    } else {
      return(NULL)
    }
    return(df)
  }))
}))

# Now we have:
# - sites$attributeGroup contains the name of the attribute
# - sites$attributeValue contains the actual value (i.e. selection) of the 
#   attribute
# Create a wide-format data frame with a column for each attribute group:
if (!include.multiple.selection) {
  sites.wide <- dcast(sites,
                      siteId + activityId + startDate + endDate ~ attributeGroup)


#################################################################################################
###### Step 3: merge missing information into the 'values' data frame:
  values <- merge(values, sites.wide, by = c("siteId", "activityId"), all.x = TRUE)
} else {
  values <- merge(values, sites, by = c("siteId", "activityId"), all.x = TRUE)
  warning("attribute values are not stored in separate columns!")
}
# 'values' should now have a separate column for every single-selection
# attribute found in all indicators that exist in the given database.


#################################################################################################
### Step 4: add the full geographic tree to the data.
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

# add a column to 'values' for each administrative level in the country:
admin.levels.table$column <- sanitizeNames(admin.levels.table$name)
for (column.name in admin.levels.table$column) {
  values[[column.name]] <- NA_character_
}

# add location type identifier to the activities table as having only the name
# is not very useful:
activities.table$locationTypeId <-
  location.types.table$id[match(activities.table$locationTypeName,
                                location.types.table$name)]

# retrieve all locations for each type present in the activities table:
locations <- list()
for (type in unique(activities.table$locationTypeId)) {
  cat("Getting all location entities of type", type, "\n")
  locations <- c(locations, getLocations(type))
}

# store the names of administrative entities for each record (i.e. site) in the
# final result:
location.ids <- sapply(locations, function(loc) loc$id)
for (id in unique(values$locationId)) {
  rows <- which(values$locationId == id)
  j <- which(location.ids == id)
  if (length(j) == 1L) {
    admin.levels <- extractAdminLevelEntities(locations[[j]])
    for (col in names(admin.levels)) {
      values[[col]][rows] <- admin.levels[[col]]
    }
  } else {
    warning("found zero or more locations with identifier ",
            values$locationId[id], ". Skipping row(s) ", paste(rows, collapse = ", "), ".")
  }
}


#################################################################################################
### Step 5: Let's cast attributes and merge them back to unique indicators
# reformat attributes
## First unique values for sites;

#names(values)
values.unique <- unique(values[,c("siteId" , "activityId" , "locationId" , "locationName"  ,
                                  "partnerId"  , "partnerName" ,  "activityName" ,
                                  "activityCategory","indicatorId"  , "value", "indicatorName",
                                  "month" , "database",  "indicatorCategory","units" , 
                                  #"startDate" , "endDate" , 
                                  #"attributeGroup" , "attributeValue" , "multipleAllowed"
                                  "governorate" ,  "region", "district" ,  "subdistrict", "refugee.camps", "camp.districts","comments"  )])

## Let's cast attributes
# We have single and multiple attributes -- multipleAllowed
#names(values)

sites.unique.attr <- unique(values[,c("siteId" , "attributeGroup" , "attributeValue" , "multipleAllowed" )])
sites.unique <- as.data.frame(values[,c("siteId"  )])
sites.unique <- unique(sites.unique)

sites.attribute.single <- sites.unique.attr[sites.unique.attr$multipleAllowed == "FALSE",c("siteId", "attributeGroup" , "attributeValue")]
sites.attribute.single.wide <- dcast(sites.attribute.single, siteId ~ attributeGroup, value.var="attributeValue")


sites.attribute.multiple <- sites.unique.attr[sites.unique.attr$multipleAllowed == "TRUE",c("siteId", "attributeGroup" , "attributeValue")]
sites.attribute.multiple.wide <- dcast(sites.attribute.multiple, siteId  ~ attributeValue)

## Merge back
#rm(values.unique.attribute)
values.unique.attribute <- merge (x=values.unique, y=sites.attribute.single.wide, by="siteId", all.x=TRUE)
values.unique.attribute <- merge (x=values.unique.attribute, y=sites.attribute.multiple.wide, by="siteId", all.x=TRUE)


values.unique.attribute$objective <- substr(values.unique.attribute$activityCategory , (regexpr("]", values.unique.attribute$activityCategory , ignore.case=FALSE, fixed=TRUE))+1,50)
values.unique.attribute$sector <- substr(values.unique.attribute$activityCategory ,1, (regexpr("[", values.unique.attribute$activityCategory , ignore.case=FALSE, fixed=TRUE))-1)



values.unique.attribute$sector[values.unique.attribute$sector=="EDU"] <-"EDUCATION"
values.unique.attribute$sector[values.unique.attribute$sector=="FOOD/LIV"] <-"FOOD/LIVELIHOOD"
values.unique.attribute$sector[values.unique.attribute$sector=="PROT"] <-"PROTECTION"
values.unique.attribute$sector[values.unique.attribute$sector=="SHLT"] <-"SHELTER"
values.unique.attribute$sector[values.unique.attribute$sector=="HLTH"] <-"HEALTH"

#unique(values.unique.attribute$sector)

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

values.unique.attribute$rcode[grepl("Zaatari", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$locationName)] <- "5"
values.unique.attribute$gcode[grepl("Zaatari", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$locationName)] <- "2"
values.unique.attribute$gov[grepl("Zaatari", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$locationName)] <- "Camps"
values.unique.attribute$region[grepl("Zaatari", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$locationName)] <- "2"

values.unique.attribute$rcode[grepl("Azraq Camp", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$locationName)] <- "5"
values.unique.attribute$gcode[grepl("Azraq Camp", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$locationName)] <- "2"
values.unique.attribute$gov[grepl("Azraq Camp", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$locationName)] <- "Camps"
values.unique.attribute$region[grepl("Azraq Camp", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$locationName)] <- "2"


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
                                       ifelse(grepl("Urban/Rural Syrian Women", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                              paste0(
                                                substr(values.unique.attribute$indicatorName ,
                                                       (regexpr("Urban/Rural Syrian Women (Age 18 and above)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+44,250)
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
                                      ifelse(grepl("Camps Syrian Women", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Camps Syrian Women (Age 18 and above)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+38,250)
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
                                      ifelse(grepl("Urban/Rural Syrian Men", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Urban/Rural Syrian Men (Age 18 and Above)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+44,250)
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
                                      ifelse(grepl("Camps Syrian Men", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Camps Syrian Men (Age 18 and above)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+36,250)
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
                                      ifelse(grepl("Urban/Rural Syrian Girls", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Urban/Rural Syrian Girls (Age 0-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+36,250)
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
                                      ifelse(grepl("Camps Syrian Girls", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Camps Syrian Girls (Age 0-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+30,250)
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
                                      ifelse(grepl("Urban/Rural Syrian Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Urban/Rural Syrian Boys (Age 0-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+35,250)
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
                                      ifelse(grepl("Camps Syrian Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Camps Syrian Boys (Age 0-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+29,250)
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
                                      ifelse(grepl("Host Community Men", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Host Community Men (Age 18 and above)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+38,250)
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
                                      ifelse(grepl("Host Community Girls", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Host Community Girls (Age 0-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+32,250)
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
                                      ifelse(grepl("Host Community Boys", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Host Community Boys (Age 0-17)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+31,250)
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
                                      ifelse(grepl("Host Community Women", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  values.unique.attribute$indicatorName),
                                             paste0(
                                               substr(values.unique.attribute$indicatorName ,
                                                      (regexpr("Host Community Women (Age 18 and above)", values.unique.attribute$indicatorName , ignore.case=FALSE, fixed=TRUE))+40,250)
                                             ), values.unique.attribute$indic)
)






### Now some manual cleaning
#values.unique.attribute$indic2 <- as.factor(values.unique.attribute$indic)
#indicbreak <- as.data.frame(levels(values.unique.attribute$indic2))
#indicbreak <- rename(indicbreak, c("levels(values.unique.attribute$indic2)"="old"))
#indicbreak$new <- indicbreak$old
#write.csv(indicbreak, file = "data/config/indicbreak.csv",na="")
#indicbreak <- read.csv("data/config/indicbreak.csv")

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
  "startDate"= "StartDate" ,
  # ""=  "EndDate",
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


output <- output[,c("sector","StartDate" ,"Category", "activity","Indicator","Indicator2", "Governorate" , "Gender","Partner" ,   "SiteType", "appeal",
                    "Fundedby",  "allocation",  "rcode" , "gcode" ,"Value" , "Units"  ,"location", "region","poptype")] 


#names(output)

#output$Indicator <- as.factor(output$Indicator)
#levels(output$Indicator)



))
##################################################################################
######### Writing output for Dashbaord dataviz @ https://github.com/unhcr-jordan/sectors 
#"BASIC NEEDS"     "PROTECTION"      "HEALTH"          "EDUCATION"       "WASH"            "SHELTER"         "FOOD/LIVELIHOOD"

output.education <-  subset(output, output$sector == "EDUCATION")
output.education.benef <-  subset(output.education, output.education$Indicator != "")
write.csv(output.education.benef, file = "out/monitor/2015/education/data.csv",na="")
output.education.oth <-  subset(output.education, output.education$Indicator == "")
output.education.oth$Indicator <- output.education.oth$Indicator2
write.csv(output.education.oth, file = "out/monitor/2015/education/dataother.csv",na="")

output.health <-  subset(output, output$sector == "HEALTH")
output.health.benef <-  subset(output.health, output.health$Indicator != "")
write.csv(output.health.benef, file = "out/monitor/2015/health/data.csv",na="")
output.health.oth <-  subset(output.health, output.health$Indicator == "")
output.health.oth$Indicator <- output.health.oth$Indicator2
write.csv(output.health.oth, file = "out/monitor/2015/health/dataother.csv",na="")

output.food <-  subset(output, output$sector == "FOOD/LIVELIHOOD")
output.food.benef <-  subset(output.food, output.food$Indicator != "")
write.csv(output.food.benef, file = "out/monitor/2015/food/data.csv",na="")
output.food.oth <-  subset(output.food, output.food$Indicator == "")
output.food.oth$Indicator <- output.food.oth$Indicator2
write.csv(output.food.oth, file = "out/monitor/2015/food/dataother.csv",na="")

output.basicneeds <-  subset(output, output$sector == "BASIC NEEDS")
output.basicneeds.benef <-  subset(output.basicneeds, output.basicneeds$Indicator != "")
write.csv(output.basicneeds.benef, file = "out/monitor/2015/basicneeds/data.csv",na="")
output.basicneeds.oth <-  subset(output.basicneeds, output.basicneeds$Indicator == "")
output.basicneeds.oth$Indicator <- output.basicneeds.oth$Indicator2
write.csv(output.basicneeds.oth, file = "out/monitor/2015/basicneeds/dataother.csv",na="")


output.protection <-  subset(output, output$sector == "PROTECTION")
#  of individuals submitted for resettlement 
# of women, girls, boys and men SGBV survivors benefiting from case management services 
# of girls & boys benefiting from multi-sectoral services
# of women, girls, boys and men with specific needs receiving special support
# of women, girls, boys & men receiving legal information, counseling and/or representation 
# of women, girls, boys & men benefiting from psychosocial support services (level 2 & 3) 

output.protection.benef <-  subset(output.protection, output.protection$Indicator != "")
write.csv(output.protection.benef, file = "out/monitor/2015/protection/data.csv",na="")
output.protection.oth <-  subset(output.protection, output.protection$Indicator == "")
output.protection.oth$Indicator <- output.protection.oth$Indicator2
write.csv(output.protection.oth, file = "out/monitor/2015/protection/dataother.csv",na="")

output.shelter <-  subset(output, output$sector == "SHELTER")
# of dwelling units upgraded to minimum standards
# Increased housing units provided in unfinished buildings
# of HH receiving rental support
# of home adaptation kits distributed
# of people receiving information messaging on housing (HLP)

output.shelter.benef <-  subset(output.shelter, output.protection$Indicator != "")
write.csv(output.shelter, file = "out/monitor/2015/shelter/data.csv",na="")
output.shelter.oth <-  subset(output.shelter, output.shelter$Indicator == "")
output.shelter.oth$Indicator <- output.shelter.oth$Indicator2
write.csv(output.shelter.oth, file = "out/monitor/2015/shelter/dataother.csv",na="")

output.wash <-  subset(output, output$sector == "WASH")
output.wash.benef <-  subset(output.wash, output.wash$Indicator != "")
write.csv(output.wash.benef, file = "out/monitor/2015/wash/data.csv",na="")
output.wash.oth <-  subset(output.wash, output.wash$Indicator == "")
output.wash.oth$Indicator <- output.wash.oth$Indicator2
write.csv(output.wash.oth, file = "out/monitor/2015/wash/dataother.csv",na="")

########################################################


#db.1064.monitor <- values.unique.attribute
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
rm(values.unique.attribute)