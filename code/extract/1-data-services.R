#####################################################
#  ActivityInfo Service Advisor Extraction Script   #
#####################################################
### JOR-REF-Services -- Refugee Services Database Jordan db 1100

# Required Packages and authenticate

source("code/0-packages.R")
source("code/0-activityinfo.R")




##############################################################################
### Step 1: list all activities and put it in a data frame
activity.table <-asActivityDataFrame(getDatabaseSchema(1100))


##############################################################################
### Step 2: extract all sites in order to get the comments
activities.reported.once <- activity.table$activityId[activity.table$reportingFrequency == 0]

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
                       comments = ifelse(is.null(site$comments), "", site$comments),
                       stringsAsFactors = FALSE)
    } else {
      return(NULL)
    }
    return(df)
  }))
}))
## Now get unique sites
sites <- unique(sites)

##############################################################################
### Step 3: Extraction of all activities per site
#getCube
getMonthlyReportsCube <- function (activityId) 
  getCube(filters = list(form = activityId), dimensions = c("indicator", "site"))

# Creating function for data extraction based on databaseID 
getMonthlyReportsCubeForDatabase <- function (databaseId) 
{
  db <- getDatabaseSchema(databaseId)
  tables <- lapply(db$activities, function(activity) {
    cat(sprintf("Fetching %d %s %s...\n", activity$id, activity$category, 
                activity$name))
    sites <- getSitesDataFrame(activity)
    indicatorValues <- getMonthlyReportsCube(activityId = activity$id)
    merge(sites, indicatorValues)
  })
  columnNames <- unique(unlist(sapply(tables, function(table) names(table)), 
                               recursive = T))
  tables <- lapply(tables, function(table) {
    columns <- lapply(columnNames, function(columnName) {
      column <- table[[columnName]]
      if (is.null(column)) {
        logical(if (is.null(table)) 
          0
          else nrow(table))
      }
      else {
        column
      }
    })
    names(columns) <- columnNames
    data.frame(columns, stringsAsFactors = F)
  })
  table <- do.call("rbind", tables)
}

# Fetching all the records based on databaseID i.e, 1100 for REF Services DB
data <- getMonthlyReportsCubeForDatabase(1100)


#################################################################################################
### Step 4: add the full geographic tree to the data.

schema <- getDatabaseSchema(1100)
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
for (column.name in admin.levels.table$column) { data[[column.name]] <- NA_character_ }

# add location type identifier to the activities table as having only the name
# is not very useful:
activity.table$locationTypeId <- location.types.table$id[match(activity.table$locationTypeName,
                                                               location.types.table$name)]

# retrieve all locations for each type present in the activities table:
locations <- list()
for (type in unique(activity.table$locationTypeId)) {
  cat("Getting all location entities of type", type, "\n")
  locations <- c(locations, getLocations(type))
}

# store the names of administrative entities for each record (i.e. site) in the final result:
location.ids <- sapply(locations, function(loc) loc$id)
for (id in unique(data$locationId)) {
  rows <- which(data$locationId == id)
  j <- which(location.ids == id)
  if (length(j) == 1L) {
    admin.levels <- extractAdminLevelEntities(locations[[j]])
    for (col in names(admin.levels)) {
      data[[col]][rows] <- admin.levels[[col]]
    }
  } else {
    warning("found zero or more locations with identifier ",
            data$locationId[id], ". Skipping row(s) ", paste(rows, collapse = ", "), ".")
  }
}
#names(data)

##############################################################################
### Step 5: Merging all elements together

# Merging data dataframe with activity table
datamerge<- merge (x=data, y=activity.table, by="activityId", all.x=TRUE)

# Merging data dataframe with site comments
datamerge1 <- merge (x=datamerge, y=sites, by=c("siteId","activityId"), all.x=TRUE)


# Loading location CSV which contains list of all the Location ID with Governorates and Districts 
#location <- read.csv("data/config/location.csv")

# Merging data dataframe with Governorates and Districts
#datamerge2<- merge (x=datamerge1, y=location, by="locationName", all.x=TRUE)


##############################################################################
### Step 6: Reformatting final ouput

# Renaming labels of data dataframe as per extraction spreadsheet format
#names(datamerge2)

output <- rename ( datamerge1, c(
  "locationName" = "Reporting Site" ,
  "siteId"="SiteID" , 
  "governorate"="Governorate", 
  "district"="District" ,  
  "partnerName" = "PartnerName",
  "indicatorName"= "ActivityIndicatorName",
  "startDate"="StartDate",
  "endDate"="EndDate",
  "X1..Registration.Type.Requirement"="RegistrationTypeRequirement",
  "X2..Nationality"="Nationality",
  "X3..Intake.Criteria...Open.to.all"= "OpenToAll",
  "X3..Intake.Criteria...Specific.Vulnerability.Calculation.Scoring" = "SpecificVulnerabilityCalculationScoring",
  "X3..Intake.Criteria...Child.at.risk"="ChildatRisk",
  "X3..Intake.Criteria...Unaccompanied.or.separated.child" = "UnaccompaniedorSeparatedChild",
  "X3..Intake.Criteria...Woman.at.risk"="WomanatRisk",
  "X3..Intake.Criteria...Older.person.at.risk"="OlderPeopleatRisk",
  "X3..Intake.Criteria...Single.parent.or.caregiver"="SingleParentorCaregiver",
  "X3..Intake.Criteria...Disability"="Disability",
  "X3..Intake.Criteria...Serious.medical.condition"="SeriousMedicalCondition",
  "X3..Intake.Criteria...Family.unity"="FamilyUnity",
  "X3..Intake.Criteria...Specific.legal.and.physical.protection.needs"="SpecificLegalandPhysicalProtectionNeeds",
  "X3..Intake.Criteria...Torture"="Torture",
  "X3..Intake.Criteria...SGBV"="SGBV",
  "X4..Accessibility"="Accessibility",
  "X5..Coverage"="Coverage",
  "X6..Availability"="Availability",
  "X7..Availability.Day"="AvaiabilityDay",
  "X8..Office.Open.at"="OfficeOpenAt",
  "X9..Office.close.at"="OfficeCloseAt",
  "X10..Referral.Method"="ReferralMethod",
  "X11..Immediate.Next.step..response.after.referal"="ImmediateNextStepResponseAfterReferral",
  "X12..Response.delay.after.referrals"="ResponseDelayAfterReferrals",
  "X13..Feedback.Mechanism"="FeedbackMechanism",
  "X14..Feedback.delay"="FeedbackDelay",
  "ReferralContact"="ReferralContact"
  #Comments
)
)

names(output)

# Generating data frame as per the Extraction template
dataServices <- output[, c("SiteID","Governorate",
                           "District",
                           "PartnerName",
                           "activityCategory","activityName",
                           "ActivityIndicatorName",
                           "StartDate","EndDate",
                           "RegistrationTypeRequirement", 
                           "Nationality", 
                           "OpenToAll", "SpecificVulnerabilityCalculationScoring", "ChildatRisk",
                           "UnaccompaniedorSeparatedChild", "WomanatRisk", "OlderPeopleatRisk","SingleParentorCaregiver",
                           "Disability", "SeriousMedicalCondition", "FamilyUnity", "SpecificLegalandPhysicalProtectionNeeds",
                           "Torture","SGBV", "Accessibility", "Coverage", "Availability", "AvaiabilityDay", 
                           "OfficeOpenAt", "OfficeCloseAt", "ReferralMethod", "ImmediateNextStepResponseAfterReferral",
                           "ResponseDelayAfterReferrals", 
                           "FeedbackMechanism", "FeedbackDelay", "ReferralContact",
                           "comments")]

# Writing CSV file with the dataframe which is generated as per the template
write.csv(dataServices, file="out/JOR-Refugee-Services-Maping.csv",row.names=F, na="")






# cleaning dataframes
rm(activity.table)
rm(data)
rm(dataServices)
rm(datamerge)
rm(datamerge1)
rm(location)
rm(output)

