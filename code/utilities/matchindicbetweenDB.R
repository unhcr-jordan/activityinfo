#load required packages
source("code/0-packages.R")

## Log in plus some helper function for the activityinfo R module
source("code/0-activityinfo.R")


####################################
#### Indicators matching
#####################################
## Get a list of each indicators


### JOR-3RP Plan Database Jordan db 1662
database.id <- 1662

# retrieve the database schema as a list:
schema.rrrp <- getDatabaseSchema(database.id)

# convert list of activities & Indicators to a data frame
activities.rrrp <-asActivityDataFrame(schema.rrrp)
indicators.rrrp <- asIndicatorDataFrame(schema.rrrp)

# merge the Db together
rrrp.indicator <- merge(x=indicators.rrrp, y=activities.rrrp, by="activityId", all=TRUE)

#names(rrp.indicator)
rrrp.indicator <-rrrp.indicator[ , c(  "activityCategory" ,  "activityName", "indicatorCategory","indicatorName","aggregation","units",
                                     "reportingFrequency" ,"locationTypeName","activityId", "databaseId.x","indicatorId" , "indicatorCode")]
# "mandatory" - "listHeader"- "databaseId.y" -  "published" 

#rrrp.indicator <- rename(rrrp.indicator, c("activityCategory"= "activityCategoryrrrp" ,  "activityName" = "activityNamerrrp", "indicatorCategory"= "indicatorCategoryrrrp",
#                                         "indicatorName"="indicatorNamerrrp","aggregation"="aggregationrrrp","units"= "unitsrrrp",
#                                         "reportingFrequency" ="reportingFrequencyrrrp","locationTypeName"="locationTypeNamerrrp",
#                                         "activityId"="activityIdrrrp", "databaseId.x"="rrrp","indicatorId"="indicatorIdrrrp" , "indicatorCode"= "indicatorCoderrrp"))


### Parsing Sector, Objective, indicator
rrrp.indicator$objective <- substr(rrrp.indicator$activityCategory , (regexpr("]", rrrp.indicator$activityCategory , ignore.case=FALSE, fixed=TRUE))+1,50)
rrrp.indicator$sector <- substr(rrrp.indicator$activityCategory ,1, (regexpr("[", rrrp.indicator$activityCategory , ignore.case=FALSE, fixed=TRUE))-1)
rrrp.indicator$sector[rrrp.indicator$sector=="EDU"] <-"EDUCATION"
rrrp.indicator$sector[rrrp.indicator$sector=="FOOD/LIV"] <-"FOOD/LIVELIHOOD"
rrrp.indicator$sector[rrrp.indicator$sector=="PROT"] <-"PROTECTION"
rrrp.indicator$sector[rrrp.indicator$sector=="SHLT"] <-"SHELTER"
rrrp.indicator$sector[rrrp.indicator$sector=="HLTH"] <-"HEALTH"
rrrp.indicator$Category <- substr(rrrp.indicator$activityName ,(regexpr("[", rrrp.indicator$activityName , ignore.case=FALSE, fixed=TRUE))+1, (regexpr("]", rrrp.indicator$activityName , ignore.case=FALSE, fixed=TRUE))-4)
rrrp.indicator$Category[rrrp.indicator$Category=="RES "] <- "Resilience"
rrrp.indicator$Category[rrrp.indicator$Category=="RES"] <- "Resilience"
rrrp.indicator$Category[rrrp.indicator$Category=="REF"] <- "Refugee"
rrrp.indicator$Category <- as.factor(rrrp.indicator$Category)
rrrp.indicator$activity2 <- substr(rrrp.indicator$activityName , (regexpr("]", rrrp.indicator$activityName , ignore.case=FALSE, fixed=TRUE))+1,50)


write.csv(rrrp.indicator, file="out/indicator/rrrpindicator2015.csv",row.names=F, na="")

rm(activities.rrrp)
rm(indicators.rrrp)
rm(schema.rrrp)
rm(database.id)


###############################################################################################
### JOR Monitoring 2015 Database Jordan db 2300
database.id <- 2300
schema.monitor <- getDatabaseSchema(database.id)
activities.monitor <-asActivityDataFrame(schema.monitor)
indicators.monitor <- asIndicatorDataFrame(schema.monitor)
monitor.indicator <- merge(x=indicators.monitor, y=activities.monitor, by="activityId", all=TRUE)
monitor.indicator <-monitor.indicator[ , c(  "activityCategory" ,  "activityName", "indicatorCategory","indicatorName","aggregation","units",
                                             "reportingFrequency" ,"locationTypeName","activityId", "databaseId.x","indicatorId" , "indicatorCode")]
#monitor.indicator <- rename(monitor.indicator, c("activityCategory"= "activityCategorymonitor" ,  "activityName" = "activityNamemonitor", "indicatorCategory"= "indicatorCategorymonitor",
#                                                 "indicatorName"="indicatorNamemonitor","aggregation"="aggregationmonitor","units"= "unitsmonitor",
#                                                 "reportingFrequency" ="reportingFrequencymonitor","locationTypeName"="locationTypeNamemonitor",
#                                                 "activityId"="activityIdmonitor", "databaseId.x"="monitor","indicatorId"="indicatorIdmonitor" , "indicatorCode"= "indicatorCodemonitor"))

### Parsing Sector, Objective, indicator
monitor.indicator$objective <- substr(monitor.indicator$activityCategory , (regexpr("]", monitor.indicator$activityCategory , ignore.case=FALSE, fixed=TRUE))+1,50)
monitor.indicator$sector <- substr(monitor.indicator$activityCategory ,1, (regexpr("[", monitor.indicator$activityCategory , ignore.case=FALSE, fixed=TRUE))-1)
monitor.indicator$sector[monitor.indicator$sector=="EDU"] <-"EDUCATION"
monitor.indicator$sector[monitor.indicator$sector=="FOOD/LIV"] <-"FOOD/LIVELIHOOD"
monitor.indicator$sector[monitor.indicator$sector=="PROT"] <-"PROTECTION"
monitor.indicator$sector[monitor.indicator$sector=="SHLT"] <-"SHELTER"
monitor.indicator$sector[monitor.indicator$sector=="HLTH"] <-"HEALTH"
monitor.indicator$Category <- substr(monitor.indicator$activityName ,(regexpr("[", monitor.indicator$activityName , ignore.case=FALSE, fixed=TRUE))+1, (regexpr("]", monitor.indicator$activityName , ignore.case=FALSE, fixed=TRUE))-4)
monitor.indicator$Category[monitor.indicator$Category=="RES "] <- "Resilience"
monitor.indicator$Category[monitor.indicator$Category=="RES"] <- "Resilience"
monitor.indicator$Category[monitor.indicator$Category=="REF"] <- "Refugee"
monitor.indicator$Category <- as.factor(monitor.indicator$Category)
monitor.indicator$activity2 <- substr(monitor.indicator$activityName , (regexpr("]", monitor.indicator$activityName , ignore.case=FALSE, fixed=TRUE))+1,50)


write.csv(monitor.indicator, file="out/indicator/monitorindicator2015.csv",row.names=F, na="")



#########################################################
### Merging RRP Plan and Monitor Indicators
###################


monitor2plan <- merge (monitor.indicator, rrrp.indicator, by=c("indicatorName","activityName" ), all=TRUE)
write.csv(monitor2plan, file="out/indicator/monitor2plan2015.csv",row.names=F, na="")






######################################################################################
### JOR Monitoring Database Jordan db 1064
database.id <- 1064
schema.monitor <- getDatabaseSchema(database.id)
activities.monitor <-asActivityDataFrame(schema.monitor)
indicators.monitor <- asIndicatorDataFrame(schema.monitor)
monitor.indicator <- merge(x=indicators.monitor, y=activities.monitor, by="activityId", all=TRUE)
monitor.indicator <-monitor.indicator[ , c(  "activityCategory" ,  "activityName", "indicatorCategory","indicatorName","aggregation","units",
                                             "reportingFrequency" ,"locationTypeName","activityId", "databaseId.x","indicatorId" , "indicatorCode")]
monitor.indicator <- rename(monitor.indicator, c("activityCategory"= "activityCategorymonitor" ,  "activityName" = "activityNamemonitor", "indicatorCategory"= "indicatorCategorymonitor",
                                                 "indicatorName"="indicatorNamemonitor","aggregation"="aggregationmonitor","units"= "unitsmonitor",
                                                 "reportingFrequency" ="reportingFrequencymonitor","locationTypeName"="locationTypeNamemonitor",
                                                 "activityId"="activityIdmonitor", "databaseId.x"="monitor","indicatorId"="indicatorIdmonitor" , "indicatorCode"= "indicatorCodemonitor"))

write.csv(monitor.indicator, file="out/indicator/monitorindicator.csv",row.names=F, na="")

## variable for merging
monitor.indicator$sector <- as.factor(monitor.indicator$activityCategorymonitor)
monitor.indicator$output <- as.factor(monitor.indicator$activityNamemonitor)
monitor.indicator$indicator <- as.factor(monitor.indicator$indicatorNamemonitor)

rm(activities.monitor)
rm(indicators.monitor)
rm(schema.monitor)
rm(database.id)

##########################################################################
### JOR-RRP6 Plan Review Database Jordan db 1272
database.id <- 1272
schema.rrp6 <- getDatabaseSchema(database.id)
activities.rrp6 <-asActivityDataFrame(schema.rrp6)
indicators.rrp6 <- asIndicatorDataFrame(schema.rrp6)
rrp6.indicator <- merge(x=indicators.rrp6, y=activities.rrp6, by="activityId", all=TRUE)
rrp6.indicator <-rrp6.indicator[ , c(  "activityCategory" ,  "activityName", "indicatorCategory","indicatorName","aggregation","units",
                                       "reportingFrequency" ,"locationTypeName","activityId", "databaseId.x","indicatorId" , "indicatorCode")]
rrp6.indicator <- rename(rrp6.indicator, c("activityCategory"= "activityCategoryrrp6" ,  "activityName" = "activityNamerrp6", "indicatorCategory"= "indicatorCategoryrrp6",
                                           "indicatorName"="indicatorNamerrp6","aggregation"="aggregationrrp6","units"= "unitsrrp6",
                                           "reportingFrequency" ="reportingFrequencyrrp6","locationTypeName"="locationTypeNamerrp6",
                                           "activityId"="activityIdrrp6", "databaseId.x"="rrp6","indicatorId"="indicatorIdrrp6" , "indicatorCode"= "indicatorCoderrp6"))


write.csv(rrp6.indicator, file="out/indicator/rrp6indicator.csv",row.names=F, na="")

## variable for merging
rrp6.indicator$sector <- as.factor(rrp6.indicator$activityCategoryrrp6)
rrp6.indicator$output <- as.factor(rrp6.indicator$activityNamerrp6)
rrp6.indicator$indicator <- as.factor(rrp6.indicator$indicatorNamerrp6)

rm(activities.rrp6)
rm(indicators.rrp6)
rm(schema.rrp6)
rm(database.id)



####################################################################################
############# Matching rrp6 to monitor

rrp6out <- as.data.frame(levels(rrp6.indicator$output))
names(rrp6out )
rrp6out  <- rename(rrp6out , c("levels(rrp6.indicator$output)"= "output"))

monitout <- as.data.frame(levels(monitor.indicator$output))
names(monitout)
monitout <- rename(monitout, c("levels(monitor.indicator$output)"= "output"))


check <- merge (rrp6out, monitout, all=TRUE)

indicators.plan2monitor <- merge (x=rrp6.indicator, y=monitor.indicator, by=c("sector","output","indicator"))

## poor matching -- needs to be done manually




### Clean
