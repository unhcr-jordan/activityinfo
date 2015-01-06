# required packages:
require("activityinfo")
require("reshape2")

#####################################################
#  ActivityInfo Monitoring analysis script          #
#####################################################

#  This script retrieves a selection of information for all activities in a
#  database which have reporting frequency "once".

# authenticate
activityInfoLogin()

### JOR-#RP Plan Database Jordan db 1662
database.id <- 1662

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

# a helper function to extract all single-selection attributes from an activity:
extractSingleAttributes <- function(activity) {
  
  extractAttributes <- function(attr) {
    do.call(rbind, lapply(attr, function (attr) {
      data.frame(id = attr$id, name = attr$name, stringsAsFactors = FALSE)
    }))
  }
  
  single.attributes <-
    do.call(rbind,
            lapply(activity$attributeGroups, function(group) {
              if (!group$multipleAllowed) {
                attributes <- extractAttributes(group$attributes)
                attributes$group <- rep(group$name, times = nrow(attributes))
                return(attributes)
              } else {
                NULL
              }
            }))
  
  single.attributes
}

# extract singel-selection attributes from the current database:
attributes.single <-
  do.call(rbind, lapply(schema$activities,
                        function(activity) extractSingleAttributes(activity)))

### Step 2: extract start/end date and attributes from all sites:

# select the identifiers of activities that have reporting frequency "once":
activities.reported.once <-
  activities.table$activityId[activities.table$reportingFrequency == 0]

# retrieve a data frame with all sites linked to all indicators in the database
# and which contains the information missing in the 'data' object:
sites <- do.call(rbind, lapply(activities.reported.once, function(id) {
  cat("Getting sites for indicator", id, "\n")
  sites <- getSites(id)
  do.call(rbind, lapply(sites, function(site) {
    df<-data.frame(siteId = site$id,
                   activityId = site$activity,
                   startDate = site$startDate,
                   endDate = site$endDate,
                   stringsAsFactors = FALSE)
    if (length(site$attributes)) {
      # site$attributes is a vector with attribute identifiers. Some of these
      # may be multiple-selection attributes, which we currently ignore.
      i <- match(site$attributes, attributes.single$id, nomatch = 0)
      df$attributeGroup <- attributes.single$group[i]
      df$attributeValue <- attributes.single$name[i]
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
sites.wide <- dcast(sites,
                    siteId + activityId + startDate + endDate ~ attributeGroup)

### Step 3: merge missing information into the 'values' data frame:
values <- merge(values, sites.wide, by = c("siteId", "activityId"), all.x = TRUE)

# 'values' should now have a separate column for every single-selection
# attribute found in all indicators that exist in the given database.
