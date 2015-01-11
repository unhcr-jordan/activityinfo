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

#source("code/activityinfo.R")

### JOR-#RP Plan Database Jordan db 1662
database.id <- 1064

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

###

db.1064.monitor <- values

### Clean unused elements

rm(activities.table)
rm(admin.levels.table)
rm(attributes.single)
rm(location.types.table)
rm(sites)
rm(sites.wide)
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
