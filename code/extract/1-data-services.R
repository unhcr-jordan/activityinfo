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
                                                      "governorate" ,  "region", "district" ,  "subdistrict", "refugee.camps", "camp.districts","comments"  )])

## Let's cast attributes
# We have single and multiple attributes -- multipleAllowed
#names(values)

sites.unique.attr <- unique(values[,c("siteId" , "attributeGroup" , "attributeValue" , "multipleAllowed" )])
sites.unique <- as.data.frame(values[,c("siteId"  )])
sites.unique <- unique(sites.unique)
sites.attribute.single <- sites.unique.attr[sites.unique.attr$multipleAllowed == "FALSE",c("siteId", "attributeGroup" , "attributeValue")]

## does not work
#sites.attribute.single.wide <- dcast(sites.attribute.single, siteId ~ attributeGroup, value.var="attributeValue")

## identify duplicate -- potential bug
sites.attribute.single.dup <- paste(sites.attribute.single$siteId, sites.attribute.single$attributeGroup, sep = " ", collapse = NULL)
sites.attribute.single.dup <- as.data.frame(sites.attribute.single.dup[duplicated(sites.attribute.single.dup)])

rm(sites.attribute.single.wide)
                       
sites.attribute.single.1 <- subset(sites.attribute.single, attributeGroup == "1. Registration Type Requirement")
sites.attribute.single.1.wide <- dcast(sites.attribute.single.1, siteId ~ attributeGroup, value.var="attributeValue")
sites.attribute.single.wide <- merge(x=sites.unique, y=sites.attribute.single.1.wide, all.x=TRUE)

sites.attribute.single.2 <- subset(sites.attribute.single, attributeGroup == "2. Nationality")
sites.attribute.single.2.wide <- dcast(sites.attribute.single.2, siteId ~ attributeGroup, value.var="attributeValue")
sites.attribute.single.wide <- merge(x=sites.attribute.single.wide, y=sites.attribute.single.2.wide, all.x=TRUE)

sites.attribute.single.4 <- subset(sites.attribute.single, attributeGroup == "4. Accessibility")
sites.attribute.single.4.wide <- dcast(sites.attribute.single.4, siteId ~ attributeGroup, value.var="attributeValue")
sites.attribute.single.wide <- merge(x=sites.attribute.single.wide, y=sites.attribute.single.4.wide, all.x=TRUE)

sites.attribute.single.5 <- subset(sites.attribute.single, attributeGroup == "5. Coverage")
sites.attribute.single.5.wide <- dcast(sites.attribute.single.5, siteId ~ attributeGroup, value.var="attributeValue")
sites.attribute.single.wide <- merge(x=sites.attribute.single.wide, y=sites.attribute.single.5.wide, all.x=TRUE)

sites.attribute.single.6 <- subset(sites.attribute.single, attributeGroup == "6. Availability")
sites.attribute.single.6.wide <- dcast(sites.attribute.single.6, siteId ~ attributeGroup, value.var="attributeValue")
sites.attribute.single.wide <- merge(x=sites.attribute.single.wide, y=sites.attribute.single.6.wide, all.x=TRUE)

sites.attribute.single.7 <- subset(sites.attribute.single, attributeGroup == "7. Availability Day")
sites.attribute.single.7.wide <- dcast(sites.attribute.single.7, siteId ~ attributeGroup, value.var="attributeValue")
sites.attribute.single.wide <- merge(x=sites.attribute.single.wide, y=sites.attribute.single.7.wide, all.x=TRUE)

sites.attribute.single.8 <- subset(sites.attribute.single, attributeGroup == "8. Office Open at")
sites.attribute.single.8.wide <- dcast(sites.attribute.single.8, siteId ~ attributeGroup, value.var="attributeValue")
sites.attribute.single.wide <- merge(x=sites.attribute.single.wide, y=sites.attribute.single.8.wide, all.x=TRUE)

sites.attribute.single.9 <- subset(sites.attribute.single, attributeGroup == "9. Office close at" )
sites.attribute.single.9.wide <- dcast(sites.attribute.single.9, siteId ~ attributeGroup, value.var="attributeValue")
sites.attribute.single.wide <- merge(x=sites.attribute.single.wide, y=sites.attribute.single.9.wide, all.x=TRUE)

sites.attribute.single.10 <- subset(sites.attribute.single, attributeGroup == "10. Referral Method" )
sites.attribute.single.10.wide <- dcast(sites.attribute.single.10, siteId ~ attributeGroup, value.var="attributeValue")
sites.attribute.single.wide <- merge(x=sites.attribute.single.wide, y=sites.attribute.single.10.wide, all.x=TRUE)

sites.attribute.single.11 <- subset(sites.attribute.single, attributeGroup == "11. Immediate Next step  response after referal")
sites.attribute.single.11.wide <- dcast(sites.attribute.single.11, siteId ~ attributeGroup, value.var="attributeValue")
sites.attribute.single.wide <- merge(x=sites.attribute.single.wide, y=sites.attribute.single.11.wide, all.x=TRUE)

sites.attribute.single.12 <- subset(sites.attribute.single, attributeGroup == "12. Response delay after referrals" )
sites.attribute.single.12.wide <- dcast(sites.attribute.single.12, siteId ~ attributeGroup, value.var="attributeValue")
sites.attribute.single.wide <- merge(x=sites.attribute.single.wide, y=sites.attribute.single.12.wide, all.x=TRUE)

sites.attribute.single.13 <- subset(sites.attribute.single, attributeGroup == "13. Feedback Mechanism")
sites.attribute.single.13.wide <- dcast(sites.attribute.single.13, siteId ~ attributeGroup, value.var="attributeValue")
sites.attribute.single.wide <- merge(x=sites.attribute.single.wide, y=sites.attribute.single.13.wide, all.x=TRUE)

sites.attribute.single.14 <- subset(sites.attribute.single, attributeGroup == "14. Feedback delay")
sites.attribute.single.14.wide <- dcast(sites.attribute.single.14, siteId ~ attributeGroup, value.var="attributeValue")
sites.attribute.single.wide <- merge(x=sites.attribute.single.wide, y=sites.attribute.single.14.wide, all.x=TRUE)


sites.attribute.multiple <- sites.unique.attr[sites.unique.attr$multipleAllowed == "TRUE",c("siteId", "attributeGroup" , "attributeValue")]
sites.attribute.multiple.wide <- dcast(sites.attribute.multiple, siteId  ~ attributeValue)

## Merge back
#rm(values.unique.attribute)
values.unique.attribute <- merge (x=values.unique, y=sites.attribute.single.wide, by="siteId", all.x=TRUE)
values.unique.attribute <- merge (x=values.unique.attribute, y=sites.attribute.multiple.wide, by="siteId", all.x=TRUE)

names(values.unique.attribute)

### Rename Column for the desired output
#rm(output)
output <- rename (values.unique.attribute, c( "locationName" = "Reporting Site" ,
  "siteId"="SiteID" , "governorate"="Governorate", "district"="District" ,
  "partnerName"= "Partner Name" ,
  "activityCategory" = "Sector" ,
  "activityName" = "Activity Name" , "indicatorName"= "Activity Indicator Name" ,
  "startDate"=   "Start Date" , "endDate"="End Date" ,
#  "1. Registration Type Requirement"= "Registration Type Requirement" ,
#  "2. Nationality" = "Nationality" , 
  "Open to all"  = "3. Intake Criteria- Open to all" ,  
  "Specific Vulnerability Calculation/Scoring"= "3. Intake Criteria- Specific Vulnerability Calculation Scoring" ,
  "Child at risk" ="3. Intake Criteria- Child at risk" ,
  "Unaccompanied or separated child" = "3. Intake Criteria- Unaccompanied or separated child" ,
  "Woman at risk" = "3. Intake Criteria- Woman at risk" ,
  "Older person at risk"=   "3. Intake Criteria- Older person at risk" , 
  "Single parent or caregiver" = "3. Intake Criteria- Single parent or caregiver" ,
  "Disability" = "3. Intake Criteria- Disability" ,
  "Serious medical condition" = "3. Intake Criteria- Serious medical condition" , 
  "Family unity"  = "3. Intake Criteria- Family unity" ,
#  "3. Intake Criteria- Specific legal and physical protection needs" ,
# "3. Intake Criteria- Torture" ,
  "SGBV"  = "3. Intake Criteria- SGBV" , 
#  "4. Accessibility" = "Accessibility" , 
#  "5. Coverage" = "Coverage" ,
#  "6. Availability"= "Availability" , "7. Availability Day"="Availability Day" ,  "8. Office Open at" = "Office Open at" ,
#  "9. Office close at"= "Office close at" ,
#  "10. Referral Method" =   "Referral Method" , 
#  "11. Immediate Next step  response after referal"="Immediate Next step",
#  "12. Response delay after referrals"= "Response delay after referrals" ,
#  "13. Feedback Mechanism"= "Feedback Mechanism" , 
#  "14. Feedback delay" =  "Feedback delay" #,
#  "Referral Contact" ,
  "comments" 	= "Comments"
  ))							

names(output)

output1 <- output[ ,c("SiteID" ,"Reporting Site", "Governorate",
                    "District" , "Partner Name" , "Sector" ,
                     "Activity Name" ,  "Activity Indicator Name" ,
                      "Start Date" , "End Date",
                    "1. Registration Type Requirement" ,
                    "2. Nationality" , 
                    "3. Intake Criteria- Open to all" ,  
                    "3. Intake Criteria- Specific Vulnerability Calculation Scoring" ,
                    "3. Intake Criteria- Child at risk" ,
                    "3. Intake Criteria- Unaccompanied or separated child" ,
                    "3. Intake Criteria- Woman at risk" ,
                    "3. Intake Criteria- Older person at risk" , 
                    "3. Intake Criteria- Single parent or caregiver" ,
                    "3. Intake Criteria- Disability" ,
                    "3. Intake Criteria- Serious medical condition", 
                    "3. Intake Criteria- Family unity" ,
                  # "3. Intake Criteria- Specific legal and physical protection needs" ,
                  # "3. Intake Criteria- Torture" ,
                    "3. Intake Criteria- SGBV" , 
                    "4. Accessibility"  , 
                    "5. Coverage"  ,
                    "6. Availability" , 
                    "7. Availability Day" ,
                    "8. Office Open at"  ,
                    "9. Office close at" ,
                    "10. Referral Method"  , 
                    "11. Immediate Next step  response after referal",
                    "12. Response delay after referrals" ,
                    "13. Feedback Mechanism" , 
                    "14. Feedback delay",
                    #"Referral Contact" ,
                    "Comments" 
                    )]

write.csv(output1, file="out/JOR-Refugee-Services-Maping.csv",row.names=F, na="")



### Clean unused elements
rm(output)
rm(output1)
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
