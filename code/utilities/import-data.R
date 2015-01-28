#############################################
### Import in Activity Info Script  #########
#############################################


library(rjson)
library(RCurl)

#activityInfoLogin()

root <- "https://www.activityinfo.org"
credentials <- readLines("~/.activityinfo.credentials")[1]

database.id <- 1100

## Some functions for interacting with the Server

ExecuteCommand <- function(type, command) {
  content <- charToRaw(toJSON(list(type = type, command = command)))  
  response <- curlPerform(url = paste(root, "command", sep="/" ), 
                          .opts = list(
                            infilesize = length(content), 
                            readfunction = content, 
                            upload = TRUE, 
                            customrequest = "POST",
                            userpwd = credentials,
                            httpauth = 1L,
                            httpheader = c(  'Content-Type' = 'application/json' )
                          ))
}

FetchResource <- function(...) {
  url <- paste(root, "resources", paste(list(...), collapse="/"), sep="/")
  cat(sprintf("Fetching %s...", url))
  fromJSON(getURL(url, .opts = list(
    userpwd = credentials,
    httpauth = 1L,
    httpheader = c( 'Accept' = 'application/json' ))))
}

## Fetch structure from server

schema <- FetchResource("database", database.id, "schema")
### location for RRP6 
locations <- FetchResource("locations?type=50512")

### This is location for Villages in Jordan
#locations2 <- FetchResource("locations?type=1360")


## Build lookup tables 

location.table <-  data.frame(
  location.id = sapply(locations, function(location) location$id), 
  location.name = sapply(locations, function(location) location$name)) 

indicator.table <- data.frame(
  activity.id = unlist(sapply(schema$activities, function(activity) 
    rep(activity$id, times=length(activity$indicators)))),
  indicator.id = unlist(sapply(schema$activities, function(activity) 
    sapply(activity$indicators, function(indicator) indicator$id))),
  indicator.name = unlist(sapply(schema$activities, function(activity) 
    sapply(activity$indicators, function(indicator) indicator$name))))


partner.table <- data.frame(
  partner.id = sapply(schema$partners, function(partner) partner$id),
  partner.name = sapply(schema$partners, function(partner) partner$name)
)

attribute.tables <- lapply(schema$activities, function(activity) {
  
  group.id <- unlist(sapply(activity$attributeGroups, function(g) 
    rep(g$id, each = length(g$attributes))))
  
  group.name <- unlist(sapply(activity$attributeGroups, function(g) 
    rep(g$name, each = length(g$attributes))))
  
  attribute.id <- unlist(sapply(activity$attributeGroups, function(g) 
    sapply(g$attributes, function(attribute) 
      attribute$id)))
  
  attribute.name <- unlist(sapply(activity$attributeGroups, function(g) 
    sapply(g$attributes, function(attribute) 
      attribute$name)))
  
  data.frame(activity.id = activity$id, 
             group.id,
             group.name,
             attribute.id,
             attribute.name, 
             stringsAsFactors=FALSE)
})

attribute.table <- do.call("rbind", attribute.tables)

# Fuzzily Match the attribute value to id based on activityid, the column's
# name and the cell value
MatchAttributeId <- function(activity.id, column.name, value) {
  with(attribute.table[attribute.table$activity.id == activity.id, ], {
    
    cleanup <- function(s) 
      tolower(
        sub("$\\d+\\.", "", # get rid of "9." at the beginning
            x = gsub(x = s, "[\\s\\.\\]", ""))) # strip spaces . etcs
    
    if(nchar(cleanup(value)) == 0) {
      return(NULL)
    }
    
    # Find the edit distances between the column/group name
    # and cell/attribute name
    d1 <- adist(cleanup(column.name), cleanup(group.name))
    d2 <- adist(cleanup(value), cleanup(attribute.name))
    
    best.match <- which.min(d1+d2)
    
    cat(sprintf("Matching [%s]:[%s] to [%s]:[%s]\n", column.name, value, 
                group.name[best.match], attribute.name[best.match]))
    
    attribute.id[best.match]
  })
}

#### Now we match all attributes

import <- read.csv("data/import-monit-jan.csv", stringsAsFactors = FALSE)
## Merge with our lookup tables

import_loc <- merge(import, location.table, by.x = 'District', by.y = 'location.name', all.x=TRUE)
import_loc_part <- merge(import_loc, partner.table, by.x = 'Agency.', by.y = 'partner.name', all.x=TRUE )
import_loc_part_act <- merge(import_loc_part, indicator.table, by.x = 'Activities', by.y = 'indicator.name', all.x=TRUE)


#  write.csv(importapril_loc_part_act, file="importapril_loc_part_act.csv", row.names=F, na="")

## define which columns contain attributes
# get the attribute ids for this activity
attribute.columns <- c("Availability", "Availability.Day", "Office.Open.at",
                       "Office.close.at", "Coverage", "Accessibility",
                       "Registration.Type.Requirement", 
                       "Nationality", "Intake.Criteria", "Referral.Method", 
                       "Immediate.Next.step..response.after.referal",
                       "Response.delay.after.referrals", "Feedback.Mechanism",
                       "Feedback.delay")

## import all now

for(i in 1:nrow(import_loc_part_act)) {
  
  row <- import_loc_part_act[i, ]
  
  properties <- list( id = as.integer(runif(n=1, min=2^5, max=2^31)),
                      reportingPeriodId = as.integer(runif(n=1, min=2^5, max=2^31)),
                      activityId = row$activity.id,
                      locationId = row$location.id,
                  #    date1 = "2014-01-01",
                 #     date2 = "2014-12-31",
                      partnerId = row$partner.id )
  
  # For each column, match to attribute id for correct activity
  # and group
  attribute.ids <- unlist(sapply(attribute.columns, function(column) {
    MatchAttributeId( row$activity.id, column, row[[column]])
  }))
  
  properties[ paste("ATTRIB", attribute.ids, sep="") ] <- TRUE  
  
  
  properties[[ "comments" ]] <- paste(
    row[, c("comments" )], collapse="\n")
  
  # set the indicator value to 1 for the activity
  properties[[ paste("I", row$indicator.id, sep="" ) ]] <- 1
  # print(properties) 
  ExecuteCommand("CreateSite", list(properties = properties))
}





