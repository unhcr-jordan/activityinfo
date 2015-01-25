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

# source("code/0-activityinfo.R")

### JOR-Services Mapping Database Jordan db 1100
database.id <- 1100

values <- getIndicatorValueTable(database.id)
# this gives us almost all the information we need. Missing are the start and 
# end date of the reporting period as well as the value of the attributes for
# each site.

### Step 1: create a lookup table for single attributes (i.e. those attributes
### that are in the activities and which allow only a single selection)

# retrieve the database schema as a list:
schema <- getDatabaseSchema(database.id)

# convert list of activities to a data frame
activities.table <-asActivityDataFrame(schema)


# extract attributes from the current database:
include.multiple.selection <- TRUE

#rm(attributes)
attributes <-
  do.call(rbind, lapply(schema$activities,
                        function(activity) {
                          extractAttributes(activity, include.multiple.selection)
                        }))

### Step 2: extract start/end date and attributes from all sites:

# select the identifiers of activities that have reporting frequency "once":
activities.reported.once <-
  activities.table$activityId[activities.table$reportingFrequency == 0]

# retrieve a data frame with all sites linked to all indicators in the database
# and which contains the information missing in the 'data' object:
sites <- do.call(rbind, lapply(activities.reported.once, function(id) {
  cat("Getting sites for activity", id, "\n")
  sites <- getSites(id)
  do.call(rbind, lapply(sites, function(site) {
    n <- length(site$attributes)
    if (n) {
      df <- data.frame(siteId = rep(site$id, n),
                       activityId = rep(site$activity, n),
                       startDate = rep(site$startDate, n),
                       endDate = rep(site$endDate, n),
                     #  comments = rep(site$comments, n),
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
### Step 3: merge missing information into the 'values' data frame:
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
            values$locationId[i], ". Skipping row(s) ", paste(rows, collapse = ", "), ".")
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
                                                      "month" , "database",  "indicatorCategory","units" , "startDate" , "endDate" , 
                                                      #"attributeGroup" , "attributeValue" , "multipleAllowed"
                                                      "governorate" ,  "region", "district" ,  "subdistrict", "refugee.camps", "camp.districts"  )])

## Let's cast attributes
# We have single and multiple attributes -- multipleAllowed
names(values)
values.attribute.single <- values[values$multipleAllowed == "FALSE",c("siteId", "indicatorId","attributeGroup" , "attributeValue")]
values.attribute.single.wide <- dcast(values.attribute.single, siteId + indicatorId ~ attributeGroup, value.var="attributeValue" )

values.attribute.multiple <- values[values$multipleAllowed == "TRUE",c("siteId", "indicatorId","attributeGroup" , "attributeValue")]
values.attribute.multiple.wide <- dcast(values.attribute.multiple, siteId + indicatorId ~ attributeValue)

## Merge back
#rm(values.unique.attribute)
values.unique.attribute <- merge (x=values.unique, y=values.attribute.single.wide, by=c("siteId", "indicatorId"), all.x=TRUE)
values.unique.attribute <- merge (x=values.unique, y=values.attribute.multiple.wide, by=c("siteId", "indicatorId"), all.x=TRUE)

### Rename Column for the desired output

output <- rename (values.unique.attribute, c(
                "SiteID" , "Governorate " , "District" , "Partner Name" , "Sector" , "Activity Name" , "Activity Indicator Name" ,
                "Start Date" , "End Date" , "Registration Type Requirement" , "Nationality" ,  "Intake Criteria- Open to all" ,
                "Intake Criteria- Specific Vulnerability Calculation Scoring" , "Intake Criteria- Child at risk" ,
                "Intake Criteria- Unaccompanied or separated child" , "Intake Criteria- Woman at risk" ,
                "Intake Criteria- Older person at risk" , "Intake Criteria- Single parent or caregiver" ,
                "Intake Criteria- Disability" , "Intake Criteria- Serious medical condition" , "Intake Criteria- Family unity" ,
                "Intake Criteria- Specific legal and physical protection needs" , "Intake Criteria- Torture" ,
                "Intake Criteria- SGBV", "Accessibility" , "Coverage" ,
                "Availability" , "Availability Day" , "Office Open at" ,
                "Office close at" , "Referral Method" , "Immediate Next step",
                "Response delay after referrals" , "Feedback Mechanism" , 
                "Feedback delay" , "Referral Contact" , "Comments" 		)])							


db.1100.services <- values.unique.attribute



### Clean unused elements

rm(values.unique.attribute)
rm(values.attribute.single)
rm(values.attribute.multiple)
rm(values.unique)


rm(activities.table)
rm(admin.levels.table)
#rm(attributes.single)
rm(attributes)
rm(include.multiple.selection)
rm(location.types.table)
rm(sites)
#rm(sites.wide)
rm(values)
rm(activities.reported.once)
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
