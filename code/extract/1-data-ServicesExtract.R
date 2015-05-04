#####################################################
#  ActivityInfo Service Advisor Extraction Script   #
#####################################################


# Required Packages and authenticate

source("code/0-packages.R")
source("code/0-activityinfo.R")



### JOR-REF-Services -- Refugee Services Database Jordan db 1100

# list all activities and put it in a data frame
activity.table <-asActivityDataFrame(getDatabaseSchema(1100))

# put the DB schema  in a  R list
schema1100 <- getDatabaseSchema(1100)

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

# Renaming labels of data dataframe as per extraction spreadsheet format

output <- rename (data, c(
  "siteId"="siteID" ,
  #Governorate
  #District
  "partnerName" = "PartnerName",
  #Sector
  #Activity Name
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
# Get Activities List 
activity.table <-asActivityDataFrame(getDatabaseSchema(1100))


# Merging data dataframe with activity table
datamerge<- merge (x=output, y=activity.table, by="activityId", all.x=TRUE)

# Loading location CSV which contains list of all the Location ID with Governorates and Districts 
location <- read.csv("data/config/location.csv")

# Merging data dataframe with Governorates and Districts

datamerge1<- merge (x=datamerge, y=location, by="locationName", all.x=TRUE)


# Generating data frame as per the Extraction template

dataServices <- datamerge1[, c("siteID","Governorate","District","PartnerName","activityCategory","activityName","ActivityIndicatorName","StartDate","EndDate","RegistrationTypeRequirement", "Nationality", 
                               "OpenToAll", "SpecificVulnerabilityCalculationScoring", "ChildatRisk", "UnaccompaniedorSeparatedChild", "WomanatRisk", "OlderPeopleatRisk","SingleParentorCaregiver",
                               "Disability", "SeriousMedicalCondition", "FamilyUnity", "SpecificLegalandPhysicalProtectionNeeds", "Torture","SGBV", "Accessibility", "Coverage", "Availability", "AvaiabilityDay", "OfficeOpenAt", "OfficeCloseAt", "ReferralMethod", "ImmediateNextStepResponseAfterReferral", "ResponseDelayAfterReferrals", 
                           "FeedbackMechanism", "FeedbackDelay", "ReferralContact")]

# Writing CSV file with the dataframe which is generated as per the template

write.csv(dataServices, file="dataServices.csv",row.names=F, na="")


# cleaning dataframes

rm(activity.table)
rm(data)
rm(dataServices)
rm(datamerge)
rm(datamerge1)
rm(location)
rm(output)


