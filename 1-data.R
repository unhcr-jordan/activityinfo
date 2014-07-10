#load required packages

source("0-packages.R")

#####################################################
#  ActivityInfo Monitoring analysis script          #
#####################################################


# authenticate
activityInfoLogin()

# Check authentication
activityinfo:::authenticate()

activityInfoAuthentication()

### JOR-RRP-Monitor -- RRP Monitoring Database Jordan db 1064

# list all activities and put it ina  data frame
activity.table <-asActivityDataFrame(getDatabaseSchema(1064))

# put the DB schema  in a  R list
#schema1064 <- getDatabaseSchema(1064)

datajan <- getMonthlyReportsCubeForDatabase(1064, '2014-01')
datafeb <- getMonthlyReportsCubeForDatabase(1064, '2014-02')
datamar <- getMonthlyReportsCubeForDatabase(1064, '2014-03')
dataapr <- getMonthlyReportsCubeForDatabase(1064, '2014-04')
datamay <- getMonthlyReportsCubeForDatabase(1064, '2014-05')
datajun <- getMonthlyReportsCubeForDatabase(1064, '2014-06')

data <- rbind(datajan,datafeb,datamar,dataapr,datamay,datajun)

write.csv(data, file="RRP-Monitor.csv",row.names=F, na="")

write.xls(data, "RRP-Monitor.xls") 
write.xlsx(x = data, file = "RRP-Monitor.xlsx",
           sheetName = "data", row.names = FALSE)


data1 <- getSitesDataFrame(1064, 4050)



root <- "https://www.syrianrefugeeresponse.org"
database.id <- 1064

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

### location used for monitoring 
locations <- FetchResource("locations?type=50512")

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



# next phase is to merge

#activityall<- merge(activity.table, indicator.table, by.x = 'activityId', by.y = 'activity.id', all.x=TRUE)

# Retrieve data for each activity
#activity4050 <- getSites(activityId=4050)
#activity4138 <- getSites(activityId=4128)

# Convert list in data frame
#library(data.table)
#activitydata4050 <- rbindlist(activity4050)
#do.call(rbind.data.frame, activity4050)

# Hawraa
